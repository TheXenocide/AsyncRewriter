using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Build.Locator;
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

                foreach (var project in solution.Projects)
                {
                    Console.WriteLine();
                    Console.WriteLine($"Project: {project.Name}");

                    var compilation = await project.GetCompilationAsync();

                    var attr = compilation.GetTypeByMetadataName("AsyncRewriter.RewriteAsyncAttribute");

                    var references = await SymbolFinder.FindReferencesAsync(attr, solution);

                    foreach (ReferencedSymbol referencedSymbol in references)
                    {
                        foreach (ReferenceLocation location in referencedSymbol.Locations)
                        {
                            var attrUsage = (await location.Document.GetSyntaxRootAsync())
                                ?.FindToken(location.Location.SourceSpan.Start);

                            if (attrUsage == null)
                            {
                                Console.WriteLine($"Unexpected Null Syntax Root: {location.Document.Name}, Start: {location.Location.SourceSpan.Start}");
                            }
                            else
                            {
                                var str = attrUsage.Value.Parent.FirstAncestorOrSelf<MemberDeclarationSyntax>().ToString();

                                Console.WriteLine($"Found Usage of Attribute at {location.Document.Name}: {str}");
                            }
                        }
                    }
                }
            }
        }
    }
}
