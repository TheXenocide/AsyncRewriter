using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.MSBuild;

namespace AsyncSolutionRewriter
{
    class Program
    {
        static async Task Main(string[] args)
        {
            try
            {
                var fullPath = args != null ? string.Join(" ", args) : null;

                if (fullPath == null || !File.Exists(fullPath))
                {
                    WriteUsage();
                    return;
                }

                await RewriteSolution(fullPath);
            } 
            catch (Exception ex)
            {
                var prevColor = Console.ForegroundColor;
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine("Exception Occurred:");
                Console.WriteLine(ex.ToString());
                Console.ForegroundColor = prevColor;
            }
#if DEBUG
            finally
            {
                Console.WriteLine("Press Enter to Exit");
                Console.ReadLine();
            }
#endif
        }

        static void WriteUsage()
        {
            Console.WriteLine("Usage: AsyncSolutionRewriter.exe C:\\Path To The\\Solution.sln");
        }

        static void WriteDualColor(string prefix, ConsoleColor prefixColor, string suffix, ConsoleColor suffixColor)
        {
            var originalColor = Console.ForegroundColor;
            try
            {
                Console.ForegroundColor = prefixColor;
                Console.Write(prefix);
                Console.ForegroundColor = suffixColor;
                Console.Write(suffix);
            }
            finally
            {
                Console.ForegroundColor = originalColor;
            }
        }

        static void WriteDualColorLine(string prefix, ConsoleColor prefixColor, string suffix, ConsoleColor suffixColor)
        {
            WriteDualColor(prefix, prefixColor, suffix, suffixColor);
            Console.WriteLine();
        }

        static async Task RewriteSolution(string solutionPath)
        {
            var defaultVS = MSBuildLocator.RegisterDefaults();

#if DEBUG
            foreach (var vs in MSBuildLocator.QueryVisualStudioInstances())
            {
                Console.WriteLine($"{vs.Version}\t{vs.Name}");
            }
            Console.WriteLine();
            Console.WriteLine($"{defaultVS.Version}\t{defaultVS.Name}");
            Console.WriteLine(defaultVS.VisualStudioRootPath);
#endif 

            var tfExePath = Path.Combine(defaultVS.VisualStudioRootPath, @"Common7\IDE\CommonExtensions\Microsoft\TeamFoundation\Team Explorer\tf.exe");
            // ASYNC_TODO: Checkout files before overwriting

            using (var workspace = MSBuildWorkspace.Create())
            {
                var solution = await workspace.OpenSolutionAsync(solutionPath);
                INamedTypeSymbol attr = null;
                bool projectMissingRef = false;

                foreach (var project in solution.Projects)
                {
                    var compilation = await project.GetCompilationAsync();

                    var newAttr = compilation.GetTypeByMetadataName("AsyncRewriter.RewriteAsyncAttribute");

                    if (newAttr == null)
                    {
                        projectMissingRef = true;
                        Console.WriteLine($"Project: {project.Name}");
                        Console.WriteLine("\tUnable to find RewriteAsyncAttribute in Project");
                    }
                    else
                    {
                        if (attr == null)
                        {
                            attr = newAttr;
                        }
                        else
                        {
                            if (attr != newAttr)
                            {
                                Console.WriteLine("RewriteAsyncAttribute Symbols do not match across projects!");
                            }
                        }
                    }
                }

                if (projectMissingRef)
                {
                    Console.WriteLine();
                    Console.WriteLine("Unable to find RewriteAsyncAttribute. Ensure all projects reference AsyncRewriter 0.9.0 Package.");
                    return;
                }

                var references = await SymbolFinder.FindReferencesAsync(attr, solution);
                var declarationsToRewrite = new HashSet<SyntaxNode>();
                var symbolsToRewrite = new HashSet<ISymbol>();

                var pendingHierarchiesToAnalyze = new Queue<ISymbol>();
                await AnalyzeReferences(references, solution, declarationsToRewrite, pendingHierarchiesToAnalyze);

                while (pendingHierarchiesToAnalyze.Count > 0)
                {
                    var symbol = pendingHierarchiesToAnalyze.Dequeue();
                    symbolsToRewrite.Add(symbol);

                    if (symbol.ContainingType.TypeKind == TypeKind.Interface)
                    {
                        Console.WriteLine();
                        WriteDualColorLine("Finding Implementations of ", ConsoleColor.DarkBlue, symbol.ToString(), ConsoleColor.Cyan);

                        var implementations = await SymbolFinder.FindImplementationsAsync(symbol, solution);
                        await AnalyzeImplementations(implementations, solution, declarationsToRewrite, pendingHierarchiesToAnalyze);
                    }
                    // TODO: Overrides/Overridden
                    // TODO: Lambdas?
                    // TODO: Add REWRITE_TODO for: all property accessors reading properties that meant to be rewritten, also add the rewrite attribute or a comment to the properties themselves
                    //       also add to all un-rewritable field accessors (e.g. Lazy<T>)
                    // TODO: Write Cancelation token as optional = default.
                    // TODO: Improve optional/params/named parameter handling
                    // TODO: Do NOT rewrite already async methods

                    Console.WriteLine();
                    WriteDualColorLine("Finding References of ", ConsoleColor.DarkGreen, symbol.ToString(), ConsoleColor.Cyan);

                    references = await SymbolFinder.FindReferencesAsync(symbol, solution);
                    await AnalyzeReferences(references, solution, declarationsToRewrite, pendingHierarchiesToAnalyze);
                }

                Console.WriteLine();
                Console.WriteLine("**************************************");
                Console.WriteLine();

                var rewriter = new AsyncRewriter.Rewriter();

                foreach (var toRewrite in declarationsToRewrite)
                {
                    if (toRewrite is MethodDeclarationSyntax methodToRewrite)
                    {
                        var compilation = await solution.GetDocument(methodToRewrite.SyntaxTree).Project.GetCompilationAsync();
                        var cancellationTokenSymbol = compilation.GetTypeByMetadataName("System.Threading.CancellationToken");
                        var rewritten = rewriter.RewriteMethod(methodToRewrite, compilation.GetSemanticModel(methodToRewrite.SyntaxTree), cancellationTokenSymbol, symbolsToRewrite);

                        var prevColor = Console.ForegroundColor;

                        Console.ForegroundColor = ConsoleColor.DarkGreen;
                        Console.WriteLine("--------------------------");
                        Console.WriteLine();
                        Console.ForegroundColor = ConsoleColor.Gray;
                        Console.WriteLine("BEFORE: ");
                        Console.WriteLine(methodToRewrite.ToString());
                        Console.WriteLine();
                        Console.ForegroundColor = ConsoleColor.Green;
                        //Console.WriteLine("//////////////////////////");
                        Console.ForegroundColor = ConsoleColor.White;
                        Console.WriteLine();
                        Console.WriteLine("AFTER: ");
                        Console.WriteLine(rewritten.NormalizeWhitespace().ToString());
                        Console.WriteLine();
                        Console.ForegroundColor = prevColor;
                    }
                    else
                    {
                        var todoRewrite = toRewrite.WithLeadingTrivia(toRewrite.GetLeadingTrivia().Add(SyntaxFactory.Comment("// REWRITE_TODO: Manually adjust to async!")));

                        var prevColor = Console.ForegroundColor;
                        Console.ForegroundColor = ConsoleColor.DarkRed;
                        Console.WriteLine("--------------------------");
                        Console.WriteLine();
                        Console.ForegroundColor = ConsoleColor.Gray;
                        Console.WriteLine("BEFORE: ");
                        Console.WriteLine(toRewrite.ToFullString());
                        Console.WriteLine();
                        Console.ForegroundColor = ConsoleColor.Green;
                        //Console.WriteLine("//////////////////////////");
                        Console.ForegroundColor = ConsoleColor.White;
                        Console.WriteLine();
                        Console.WriteLine("AFTER: ");
                        Console.WriteLine(todoRewrite.NormalizeWhitespace().ToFullString());
                        Console.WriteLine();
                        Console.ForegroundColor = prevColor;
                    }
                }
                
                //foreach (var toRewrite in symbolsToRewrite)
                //{
                //    Console.WriteLine($"{toRewrite.ToString()}");
                //    Console.WriteLine();
                //}
            }
        }

        private static async Task AnalyzeImplementations(IEnumerable<ISymbol> implementations, Solution solution, HashSet<SyntaxNode> declarationsToRewrite, Queue<ISymbol> pendingHierarchiesToAnalyze)
        {
            foreach (var implementation in implementations)
            {
                var implementationDeclaration = await implementation.DeclaringSyntaxReferences.Single().GetSyntaxAsync();

                if (implementationDeclaration is MemberDeclarationSyntax implementationMemberDeclaration)
                {
                    await AnalyzeMemberDeclaration(implementationMemberDeclaration, solution, declarationsToRewrite, pendingHierarchiesToAnalyze);
                }
                else
                {
                    WriteDualColorLine("Unexpected Implementation Syntax: ", ConsoleColor.Red, implementationDeclaration.ToString(), ConsoleColor.DarkGray);
                }
            }
        }

        private static async Task AnalyzeReferences(IEnumerable<ReferencedSymbol> references, Solution solution, HashSet<SyntaxNode> declarationsToRewrite, Queue<ISymbol> pendingHierarchiesToAnalyze)
        {
            foreach (ReferencedSymbol referencedSymbol in references)
            {
                foreach (ReferenceLocation location in referencedSymbol.Locations)
                {
                    var refUsage = (await location.Document.GetSyntaxRootAsync())
                        ?.FindToken(location.Location.SourceSpan.Start);

                    if (refUsage == null)
                    {
                        WriteDualColor("Unexpected Null Syntax Root: ", ConsoleColor.DarkRed, location.Document.Name, ConsoleColor.Red);
                        WriteDualColorLine(", Start: ", ConsoleColor.DarkRed, location.Location.SourceSpan.Start.ToString(), ConsoleColor.Red);
                    }
                    else
                    {
                        var declarationToAnalyze = refUsage.Value.Parent.FirstAncestorOrSelf<MemberDeclarationSyntax>();

                        if (declarationToAnalyze is FieldDeclarationSyntax fieldDeclarationToAnalyze)
                        {
                            var refAncestors = refUsage.Value.Parent.Ancestors().ToHashSet();

                            var variableDeclarator = fieldDeclarationToAnalyze.Declaration.Variables.Single(declarator => refAncestors.Contains(declarator));

                            await AnalyzeMemberDeclaration(variableDeclarator, solution, declarationsToRewrite, pendingHierarchiesToAnalyze);

                            //if (declarationsToRewrite.Add(variableDeclarator)) 

                            //var semanticModel = (await solution.GetDocument(variableDeclarator.SyntaxTree).Project.GetCompilationAsync()).GetSemanticModel(variableDeclarator.SyntaxTree);
                            //var symbolToAnalyze = semanticModel.GetDeclaredSymbol(variableDeclarator);

                            //var fieldReferences = await SymbolFinder.FindReferencesAsync(symbolToAnalyze, solution);

                            //WriteDualColorLine("TEST; Field Declaration: ", ConsoleColor.DarkYellow, symbolToAnalyze.ToString(), ConsoleColor.Magenta);

                            //foreach (var fieldRef in fieldReferences)
                            //{
                            //    foreach (var fieldRefLocation in fieldRef.Locations)
                            //    {
                            //        var fieldRefUsage = (await fieldRefLocation.Document.GetSyntaxRootAsync())
                            //            ?.FindToken(fieldRefLocation.Location.SourceSpan.Start);

                            //        var fieldRefMemDecl = fieldRefUsage.Value.Parent.FirstAncestorOrSelf<MemberDeclarationSyntax>();

                            //        WriteDualColorLine("TEST; Field Ref: ", ConsoleColor.Yellow, fieldRefMemDecl.ToString(), ConsoleColor.Magenta);
                            //    }
                            //}
                        }
                        else
                        {
                            await AnalyzeMemberDeclaration(declarationToAnalyze, solution, declarationsToRewrite, pendingHierarchiesToAnalyze);
                        }
                    }
                }
            }
        }

        private static async Task AnalyzeMemberDeclaration(SyntaxNode declarationToAnalyze, Solution solution, HashSet<SyntaxNode> declarationsToRewrite, Queue<ISymbol> pendingHierarchiesToAnalyze)
        {
            if (declarationsToRewrite.Add(declarationToAnalyze))
            {
                var semanticModel = (await solution.GetDocument(declarationToAnalyze.SyntaxTree).Project.GetCompilationAsync()).GetSemanticModel(declarationToAnalyze.SyntaxTree);
                var symbolToAnalyze = semanticModel.GetDeclaredSymbol(declarationToAnalyze);

                WriteDualColorLine("Adding Symbol to Analysis: ", ConsoleColor.DarkCyan, symbolToAnalyze.ToString(), ConsoleColor.Cyan);

                pendingHierarchiesToAnalyze.Enqueue(symbolToAnalyze);
            }
        }
    }
}
