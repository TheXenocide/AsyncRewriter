using System.IO;
using System.Linq;
using AsyncRewriter.Logging;
using Microsoft.Build.Framework;

namespace AsyncRewriter
{
    /// <summary>
    /// </summary>
    /// <remarks>
    /// http://stackoverflow.com/questions/2961753/how-to-hide-files-generated-by-custom-tool-in-visual-studio
    /// </remarks>
    public class RewriteAsync : Microsoft.Build.Utilities.Task
    {

        [Required]
        public ITaskItem[] InputFiles { get; set; } = default!;

        [Required]
        public ITaskItem OutputFile { get; set; } = default!;

        readonly Rewriter _rewriter;

        public RewriteAsync()
        {
            _rewriter = Log == null ? new Rewriter() : new Rewriter(new TaskLoggingAdapter(Log));
        }

        public override bool Execute()
        {
            var asyncCode = _rewriter.RewriteAndMerge(InputFiles.Select(f => f.ItemSpec).ToArray());
            File.WriteAllText(OutputFile.ItemSpec, asyncCode);
            return true;
        }

    }
}