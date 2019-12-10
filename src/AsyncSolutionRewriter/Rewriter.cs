using System;
using System.Collections.Generic;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Reflection;
using AsyncRewriter.Logging;
using AsyncSolutionRewriter;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace AsyncRewriter
{
    /// <summary>
    /// </summary>
    /// <remarks>
    /// http://stackoverflow.com/questions/2961753/how-to-hide-files-generated-by-custom-tool-in-visual-studio
    /// </remarks>
    public class Rewriter
    {
        /// <summary>
        /// Invocations of methods on these types never get rewritten to async
        /// </summary>
        HashSet<ITypeSymbol> excludedTypesSet = new HashSet<ITypeSymbol>(); // TODO: This was null

        public bool GenerateConfigureAwait { get; set; } = false;

        /// <summary>
        /// Using directives required for async, not expected to be in the source (sync) files
        /// </summary>
        static readonly UsingDirectiveSyntax[] ExtraUsingDirectives = {
            SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Threading")),
            SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System.Threading.Tasks")),
        };

        /// <summary>
        /// Calls of methods on these types never get rewritten, because they aren't actually
        /// asynchronous. An additional user-determined list may also be passed in.
        /// </summary>
        static readonly string[] AlwaysExcludedTypes = {
            "System.IO.TextWriter",
            "System.IO.StringWriter",
            "System.IO.MemoryStream"
        };

        readonly ILogger _log;

        public Rewriter(ILogger? log = null)
        {
            _log = log ?? new ConsoleLoggingAdapter();
        }

        public string RewriteAndMerge(string[] paths, string[]? additionalAssemblyNames = null, string[]? excludedTypes = null)
        {
            if (paths.All(p => Path.GetFileName(p) != "AsyncRewriterHelpers.cs"))
                throw new ArgumentException("AsyncRewriterHelpers.cs must be included in paths", nameof(paths));

            var syntaxTrees = paths.Select(p => SyntaxFactory.ParseSyntaxTree(File.ReadAllText(p))).ToArray();

            var compilation = CSharpCompilation.Create("Temp", syntaxTrees, null, new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddReferences(
                        MetadataReference.CreateFromFile(typeof(object).GetTypeInfo().Assembly.Location),
                        MetadataReference.CreateFromFile(typeof(Stream).GetTypeInfo().Assembly.Location),
                        MetadataReference.CreateFromFile(typeof(DbConnection).GetTypeInfo().Assembly.Location)
                );

#if NETSTANDARD1_5
            var assemblyPath = Path.GetDirectoryName(typeof(object).GetTypeInfo().Assembly.Location);
            compilation = compilation.AddReferences(
                MetadataReference.CreateFromFile(Path.Combine(assemblyPath, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(assemblyPath, "mscorlib.dll"))
            );
#endif

            if (additionalAssemblyNames != null)
            {
                compilation = compilation.AddReferences(additionalAssemblyNames.Select(n => MetadataReference.CreateFromFile(n)));
            }

            return RewriteAndMerge(syntaxTrees, compilation, excludedTypes).ToString();
        }

        public SyntaxTree RewriteAndMerge(SyntaxTree[] syntaxTrees, CSharpCompilation compilation, string[]? excludedTypes = null)
        {
            var rewrittenTrees = Rewrite(syntaxTrees, compilation, excludedTypes).ToArray();

            var usings = rewrittenTrees.SelectMany(t => t.GetCompilationUnitRoot().Usings).ToList();
            //// Add "Resharper disable all" comment
            //usings[usings.Count - 1] = usings[usings.Count - 1].WithTrailingTrivia(
            //    SyntaxTriviaList.Create(SyntaxFactory.Comment("\n// Resharper disable all"))
            //);

            return SyntaxFactory.SyntaxTree(
                SyntaxFactory.CompilationUnit()
                    .WithUsings(SyntaxFactory.List(usings))
                    .WithMembers(SyntaxFactory.List<MemberDeclarationSyntax>(
                        rewrittenTrees
                            .SelectMany(t => t.GetCompilationUnitRoot().Members)
                            .Cast<NamespaceDeclarationSyntax>()
                            .SelectMany(ns => ns.Members)
                            .Cast<TypeDeclarationSyntax>()
                            .GroupBy(cls => cls.FirstAncestorOrSelf<NamespaceDeclarationSyntax>().Name.ToString())
                            .Select(g => SyntaxFactory.NamespaceDeclaration(SyntaxFactory.ParseName(g.Key))
                                .WithMembers(SyntaxFactory.List<MemberDeclarationSyntax>(g))
                            )
                    ))
                    .WithEndOfFileToken(SyntaxFactory.Token(SyntaxKind.EndOfFileToken))
                    .NormalizeWhitespace()
            );
        }

        public IEnumerable<SyntaxTree> Rewrite(SyntaxTree[] syntaxTrees, CSharpCompilation compilation, string[]? excludedTypes = null)
        {
            var cancellationTokenSymbol = compilation.GetTypeByMetadataName("System.Threading.CancellationToken");

            excludedTypesSet = new HashSet<ITypeSymbol>();

            // Handle the user-provided exclude list
            if (excludedTypes != null)
            {
                var excludedTypeSymbols = excludedTypes.Select(compilation.GetTypeByMetadataName).ToList();
                var notFound = excludedTypeSymbols.IndexOf(null);
                if (notFound != -1)
                    throw new ArgumentException($"Type {excludedTypes[notFound]} not found in compilation", nameof(excludedTypes));
                excludedTypesSet.UnionWith(excludedTypeSymbols);
            }

            // And the builtin exclude list
            excludedTypesSet.UnionWith(
                AlwaysExcludedTypes
                    .Select(compilation.GetTypeByMetadataName)
                    .Where(sym => sym != null)
            );

            foreach (var syntaxTree in syntaxTrees)
            {
                var semanticModel = compilation.GetSemanticModel(syntaxTree, true);
                if (semanticModel == null)
                    throw new ArgumentException("A provided syntax tree wasn't compiled into the provided compilation");

                var usings = syntaxTree.GetCompilationUnitRoot().Usings;

                var asyncRewriterUsing = usings.SingleOrDefault(u => u.Name.ToString() == "AsyncRewriter");
                if (asyncRewriterUsing == null)
                    continue;   // No "using AsyncRewriter", skip this file

                usings = usings
                    // Remove the AsyncRewriter using directive
                    .Remove(asyncRewriterUsing)
                    // Add the extra using directives
                    .AddRange(ExtraUsingDirectives);

                // Add #pragma warning disable at the top of the file
                usings = usings.Replace(usings[0], usings[0].WithLeadingTrivia(SyntaxFactory.Trivia(SyntaxFactory.PragmaWarningDirectiveTrivia(SyntaxFactory.Token(SyntaxKind.DisableKeyword), true))));

                var namespaces = SyntaxFactory.List<MemberDeclarationSyntax>(
                    syntaxTree.GetRoot()
                    .DescendantNodes()
                    .OfType<MethodDeclarationSyntax>()
                    .Where(m => m.AttributeLists.SelectMany(al => al.Attributes).Any(a => a.Name.ToString() == "RewriteAsync"))
                    .GroupBy(m => m.FirstAncestorOrSelf<TypeDeclarationSyntax>())
                    .GroupBy(g => g.Key.FirstAncestorOrSelf<NamespaceDeclarationSyntax>())
                    .Select(nsGrp =>
                        SyntaxFactory.NamespaceDeclaration(nsGrp.Key.Name)
                        .WithMembers(SyntaxFactory.List<MemberDeclarationSyntax>(nsGrp.Select(clsGrp =>
                            SyntaxFactory.TypeDeclaration(clsGrp.Key.Kind(), clsGrp.Key.Identifier)
                                .WithModifiers(clsGrp.Key.Modifiers)
                                .WithTypeParameterList(clsGrp.Key.TypeParameterList)
                                .WithMembers(SyntaxFactory.List<MemberDeclarationSyntax>(
                                    clsGrp.Select(m => RewriteMethod(m, semanticModel, cancellationTokenSymbol, new HashSet<string>()))
                                ))
                        )))
                    )
                );

                yield return SyntaxFactory.SyntaxTree(
                    SyntaxFactory.CompilationUnit()
                        .WithUsings(SyntaxFactory.List(usings))
                        .WithMembers(namespaces)
                        .WithEndOfFileToken(SyntaxFactory.Token(SyntaxKind.EndOfFileToken))
                        .NormalizeWhitespace()
                );
            }
        }

        public MethodDeclarationSyntax RewriteMethod(MethodDeclarationSyntax inMethodSyntax, SemanticModel semanticModel, ITypeSymbol cancellationTokenSymbol, HashSet<string> symbolsToRewrite)
        {
            var inMethodSymbol = semanticModel.GetDeclaredSymbol(inMethodSyntax);
            // ASYNC_TODO: Find all references

            //Log.LogMessage("Method {0}: {1}", inMethodInfo.Symbol.Name, inMethodInfo.Symbol.);

            var outMethodName = inMethodSyntax.Identifier.Text + "Async";

            _log.Debug("  Rewriting method {0} to {1}", inMethodSymbol.Name, outMethodName);

            // Visit all method invocations inside the method, rewrite them to async if needed
            var rewriter = new MethodInvocationRewriter(_log, semanticModel, excludedTypesSet, cancellationTokenSymbol, GenerateConfigureAwait, symbolsToRewrite);
            var outMethod = (MethodDeclarationSyntax)rewriter.Visit(inMethodSyntax);

            // Method signature
            outMethod = outMethod
                .WithIdentifier(SyntaxFactory.Identifier(outMethodName));
            if (outMethod.AttributeLists.Any(al => al.Attributes.Any(attr => attr.Name.ToString().Contains("RewriteAsync"))))
            {
                var newAttrList = new SyntaxList<AttributeListSyntax>();

                foreach (var attrList in outMethod.AttributeLists)
                {
                    var curList = attrList;
                    foreach (var attrItem in attrList.Attributes)
                    {
                        if (attrItem.Name.ToString().Contains("RewriteAsync"))
                        {
                            curList = curList.RemoveNode(attrItem, SyntaxRemoveOptions.KeepNoTrivia);
                        }
                    }

                    if (curList.Attributes.Any())
                    {
                        newAttrList = newAttrList.Add(curList);
                    }
                }

                outMethod = outMethod.WithAttributeLists(newAttrList);
            }   

            if (inMethodSyntax.FirstAncestorOrSelf<TypeDeclarationSyntax>().Kind() == SyntaxKind.InterfaceDeclaration)
            {
                outMethod = outMethod
                    .WithModifiers(inMethodSyntax.Modifiers);
            }
            else
            {
                outMethod = outMethod
                    .WithModifiers(inMethodSyntax.Modifiers
                      .Add(SyntaxFactory.Token(SyntaxKind.AsyncKeyword)));
            }
                //.Remove(SyntaxFactory.Token(SyntaxKind.OverrideKeyword))
                //.Remove(SyntaxFactory.Token(SyntaxKind.NewKeyword))
                
                // Insert the cancellation token into the parameter list at the right place
            outMethod = outMethod
                .WithParameterList(SyntaxFactory.ParameterList(inMethodSyntax.ParameterList.Parameters.Insert(
                    inMethodSyntax.ParameterList.Parameters.TakeWhile(p => p.Default == null && !p.Modifiers.Any(m => m.IsKind(SyntaxKind.ParamsKeyword))).Count(),
                    SyntaxFactory.Parameter(
                            SyntaxFactory.List<AttributeListSyntax>(),
                            SyntaxFactory.TokenList(),
                            SyntaxFactory.ParseTypeName("CancellationToken"),
                            SyntaxFactory.Identifier("cancellationToken"),
                            null
                ))));

            // Transform return type adding Task<>
            var returnType = inMethodSyntax.ReturnType.ToString();
            outMethod = outMethod.WithReturnType(SyntaxFactory.ParseTypeName(
                returnType == "void" ? "Task" : $"Task<{returnType}>")
            );

            // Remove the override and new attributes. Seems like the clean .Remove above doesn't work...
            //for (var i = 0; i < outMethod.Modifiers.Count;)
            //{
            //    var text = outMethod.Modifiers[i].Text;
            //    if (text == "override" || text == "new")
            //    {
            //        outMethod = outMethod.WithModifiers(outMethod.Modifiers.RemoveAt(i));
            //        continue;
            //    }
            //    i++;
            //}

            var attr = inMethodSymbol.GetAttributes().SingleOrDefault(a => a.AttributeClass.Name == "RewriteAsyncAttribute");

            if (attr != null && attr.ConstructorArguments.Length > 0 && (bool)attr.ConstructorArguments[0].Value)
            {
                outMethod = outMethod.AddModifiers(SyntaxFactory.Token(SyntaxKind.OverrideKeyword));
            }

            return outMethod;
        }
    }

    internal class MethodInvocationRewriter : CSharpSyntaxRewriter
    {
        readonly SemanticModel _model;
        readonly HashSet<ITypeSymbol> _excludeTypes;
        readonly ITypeSymbol _cancellationTokenSymbol;
        readonly bool _generateConfigureAwait;
        readonly ParameterComparer _paramComparer;
        readonly ILogger _log;

        readonly HashSet<string> _symbolsToRewrite;

        public MethodInvocationRewriter(ILogger log, SemanticModel model, HashSet<ITypeSymbol> excludeTypes,
                                        ITypeSymbol cancellationTokenSymbol, bool generateConfigureAwait, HashSet<string> symbolsToRewrite)
        {
            _log = log;
            _model = model;
            _cancellationTokenSymbol = cancellationTokenSymbol;
            _generateConfigureAwait = generateConfigureAwait;
            _excludeTypes = excludeTypes;
            _paramComparer = new ParameterComparer();
            
            _symbolsToRewrite = symbolsToRewrite;
        }

        public override SyntaxNode? VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            int cancellationTokenPos;
            if (!IsInvocationExpressionRewritable(node, out cancellationTokenPos))
                return base.VisitInvocationExpression(node);

            var rewritten = RewriteInvocationExpression(node, cancellationTokenPos);
            
            rewritten = SyntaxFactory.AwaitExpression(rewritten);

            var parent = node.Parent;
            if (!(parent is StatementSyntax || parent is ArgumentSyntax || parent is EqualsValueClauseSyntax || parent is AssignmentExpressionSyntax))
                rewritten = SyntaxFactory.ParenthesizedExpression(rewritten);

            return rewritten;
        }

        public bool IsInvocationExpressionRewritable(InvocationExpressionSyntax node, out int cancellationTokenPos)
        {
            cancellationTokenPos = -1;

            Program.WriteDualColorLine("Checking Invocation: ", ConsoleColor.DarkYellow, node.ToString(), ConsoleColor.Yellow);

            var syncSymbol = (IMethodSymbol)_model.GetSymbolInfo(node).Symbol;
            if (syncSymbol == null)
                return false;

            // Skip invocations of methods that don't have [RewriteAsync], or an Async
            // counterpart to them
            if (_symbolsToRewrite.Contains(syncSymbol.ToString()) || syncSymbol.GetAttributes().Any(a => a.AttributeClass.Name == "RewriteAsyncAttribute"))
            {
                // This is one of our methods, flagged for async rewriting.
                // Find the proper position for the cancellation token
                cancellationTokenPos = syncSymbol.Parameters.TakeWhile(p => !p.IsOptional && !p.IsParams).Count();
                Program.WriteDualColorLine("Invocation Marked for Rewrite: ", ConsoleColor.DarkGreen, node.ToString(), ConsoleColor.Green);
            }
            else
            {
                if (_excludeTypes.Contains(syncSymbol.ContainingType))
                    return false;

                var asyncCandidates = syncSymbol.ContainingType.GetMembers(syncSymbol.Name + "Async").Cast<IMethodSymbol>().ToList();

                // First attempt to find an async counterpart method accepting a cancellation token.
                foreach (var candidate in asyncCandidates.Where(c => c.Parameters.Length == syncSymbol.Parameters.Length + 1))
                {
                    var ctPos = candidate.Parameters.TakeWhile(p => p.Type != _cancellationTokenSymbol).Count();
                    if (ctPos == candidate.Parameters.Length)  // No cancellation token
                        continue;
                    if (!candidate.Parameters.RemoveAt(ctPos).SequenceEqual(syncSymbol.Parameters, _paramComparer))
                        continue;
                    cancellationTokenPos = ctPos;

                    Program.WriteDualColorLine("Cancelable Invocation Candidate Found: ", ConsoleColor.DarkCyan, candidate.ToString(), ConsoleColor.Cyan);
                }

                if (cancellationTokenPos == -1)
                {
                    // Couldn't find an async overload that accepts a cancellation token.
                    // Next attempt to find an async method with a matching parameter list with no cancellation token
                    IMethodSymbol? fallback = asyncCandidates.FirstOrDefault(ms =>
                                                                                ms.Parameters.Length == syncSymbol.Parameters.Length &&
                                                                                ms.Parameters.SequenceEqual(syncSymbol.Parameters)
                                                                            );
                    if (fallback != null)
                    {
                        Program.WriteDualColorLine("Fallback Invocation Candidate Found: ", ConsoleColor.DarkMagenta, fallback.ToString(), ConsoleColor.Magenta);
                        cancellationTokenPos = -1;
                    }
                    else
                    {
                        Program.WriteDualColorLine("No Async Symbol Replacement Found: ", ConsoleColor.DarkGray, syncSymbol.ToString(), ConsoleColor.White);
                        // Couldn't find anything, don't rewrite the invocation
                        return false;
                    }
                }
            }

            //_log.Debug("    Found rewritable invocation: " + syncSymbol);
            return true;
        }

        ExpressionSyntax RewriteInvocationExpression(InvocationExpressionSyntax node, int cancellationTokenPos)
        {
            InvocationExpressionSyntax rewrittenInvocation = null;

            if (node.Expression is IdentifierNameSyntax)
            {
                var identifierName = (IdentifierNameSyntax)node.Expression;
                rewrittenInvocation = node.WithExpression(identifierName.WithIdentifier(
                    SyntaxFactory.Identifier(identifierName.Identifier.Text + "Async")
                ));
            }
            else if (node.Expression is MemberAccessExpressionSyntax)
            {
                var memberAccessExp = (MemberAccessExpressionSyntax)node.Expression;
                var nestedInvocation = memberAccessExp.Expression as InvocationExpressionSyntax;
                if (nestedInvocation != null)
                    memberAccessExp = memberAccessExp.WithExpression((ExpressionSyntax)VisitInvocationExpression(nestedInvocation));

                rewrittenInvocation = node.WithExpression(memberAccessExp.WithName(
                    memberAccessExp.Name.WithIdentifier(
                        SyntaxFactory.Identifier(memberAccessExp.Name.Identifier.Text + "Async")
                    )
                ));
            }
            else if (node.Expression is GenericNameSyntax)
            {
                var genericNameExp = (GenericNameSyntax)node.Expression;
                rewrittenInvocation = node.WithExpression(
                    genericNameExp.WithIdentifier(SyntaxFactory.Identifier(genericNameExp.Identifier.Text + "Async"))
                );
            }
            else if (node.Expression is MemberBindingExpressionSyntax &&
                     node.Parent is ConditionalAccessExpressionSyntax)
            {
                // This is somewhat of a special case...
                // X?.Func() needs to be rewritten to await (X?.FuncAsync(CancellationToken token))
                // So the await preceeds the ConditionalAccessExpression rather than the invocation.
                // We have VisitConditionalAccessExpression below which takes care of this, ignore here.

                var memberBindingExp = (MemberBindingExpressionSyntax)node.Expression;
                rewrittenInvocation = node.WithExpression(
                    memberBindingExp.WithName(SyntaxFactory.IdentifierName(memberBindingExp.Name + "Async"))
                );
            }
            else throw new NotSupportedException($"It seems there's an expression type ({node.Expression.GetType().Name}) not yet supported by the AsyncRewriter");

            if (cancellationTokenPos != -1)
            {
                var cancellationTokenArg = SyntaxFactory.Argument(SyntaxFactory.IdentifierName("cancellationToken"));

                if (cancellationTokenPos == rewrittenInvocation.ArgumentList.Arguments.Count)
                    rewrittenInvocation = rewrittenInvocation.WithArgumentList(
                        rewrittenInvocation.ArgumentList.AddArguments(cancellationTokenArg)
                    );
                else
                    rewrittenInvocation = rewrittenInvocation.WithArgumentList(SyntaxFactory.ArgumentList(
                        rewrittenInvocation.ArgumentList.Arguments.Insert(cancellationTokenPos, cancellationTokenArg)
                    ));
            }

            if (_generateConfigureAwait)
            {
                rewrittenInvocation =
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            rewrittenInvocation,
                            SyntaxFactory.IdentifierName("ConfigureAwait")
                        ),
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.LiteralExpression(
                                        SyntaxKind.FalseLiteralExpression))))
                    );
            }

            return rewrittenInvocation;
        }

        public override SyntaxNode VisitConditionalAccessExpression(ConditionalAccessExpressionSyntax node)
        {
            var asInvocation = node.WhenNotNull as InvocationExpressionSyntax;
            if (asInvocation == null)
                return node;

            int cancellationTokenPos;
            if (!IsInvocationExpressionRewritable(asInvocation, out cancellationTokenPos))
                return node;

            var rewritten = node.WithWhenNotNull(RewriteInvocationExpression(asInvocation, cancellationTokenPos));
            return SyntaxFactory.AwaitExpression(SyntaxFactory.ParenthesizedExpression(rewritten));
        }

        class ParameterComparer : IEqualityComparer<IParameterSymbol>
        {
            public bool Equals(IParameterSymbol x, IParameterSymbol y)
            {
                return
                    x.Name.Equals(y.Name) &&
                    x.Type.Equals(y.Type);
            }

            public int GetHashCode(IParameterSymbol p)
            {
                return p.GetHashCode();
            }
        }
    }
}
