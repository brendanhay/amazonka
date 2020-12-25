{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineExecution
  ( PipelineExecution (..),

    -- * Smart constructor
    mkPipelineExecution,

    -- * Lenses
    peArtifactRevisions,
    pePipelineExecutionId,
    pePipelineName,
    pePipelineVersion,
    peStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types.ArtifactRevision as Types
import qualified Network.AWS.CodePipeline.Types.PipelineExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.PipelineExecutionStatus as Types
import qualified Network.AWS.CodePipeline.Types.PipelineName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an execution of a pipeline.
--
-- /See:/ 'mkPipelineExecution' smart constructor.
data PipelineExecution = PipelineExecution'
  { -- | A list of @ArtifactRevision@ objects included in a pipeline execution.
    artifactRevisions :: Core.Maybe [Types.ArtifactRevision],
    -- | The ID of the pipeline execution.
    pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId,
    -- | The name of the pipeline with the specified pipeline execution.
    pipelineName :: Core.Maybe Types.PipelineName,
    -- | The version number of the pipeline with the specified pipeline execution.
    pipelineVersion :: Core.Maybe Core.Natural,
    -- | The status of the pipeline execution.
    --
    --
    --     * InProgress: The pipeline execution is currently running.
    --
    --
    --     * Stopped: The pipeline execution was manually stopped. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
    --
    --
    --     * Stopping: The pipeline execution received a request to be manually stopped. Depending on the selected stop mode, the execution is either completing or abandoning in-progress actions. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
    --
    --
    --     * Succeeded: The pipeline execution was completed successfully.
    --
    --
    --     * Superseded: While this pipeline execution was waiting for the next stage to be completed, a newer pipeline execution advanced and continued through the pipeline instead. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions> .
    --
    --
    --     * Failed: The pipeline execution was not completed successfully.
    status :: Core.Maybe Types.PipelineExecutionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PipelineExecution' value with any optional fields omitted.
mkPipelineExecution ::
  PipelineExecution
mkPipelineExecution =
  PipelineExecution'
    { artifactRevisions = Core.Nothing,
      pipelineExecutionId = Core.Nothing,
      pipelineName = Core.Nothing,
      pipelineVersion = Core.Nothing,
      status = Core.Nothing
    }

-- | A list of @ArtifactRevision@ objects included in a pipeline execution.
--
-- /Note:/ Consider using 'artifactRevisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peArtifactRevisions :: Lens.Lens' PipelineExecution (Core.Maybe [Types.ArtifactRevision])
peArtifactRevisions = Lens.field @"artifactRevisions"
{-# DEPRECATED peArtifactRevisions "Use generic-lens or generic-optics with 'artifactRevisions' instead." #-}

-- | The ID of the pipeline execution.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePipelineExecutionId :: Lens.Lens' PipelineExecution (Core.Maybe Types.PipelineExecutionId)
pePipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED pePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The name of the pipeline with the specified pipeline execution.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePipelineName :: Lens.Lens' PipelineExecution (Core.Maybe Types.PipelineName)
pePipelineName = Lens.field @"pipelineName"
{-# DEPRECATED pePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The version number of the pipeline with the specified pipeline execution.
--
-- /Note:/ Consider using 'pipelineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePipelineVersion :: Lens.Lens' PipelineExecution (Core.Maybe Core.Natural)
pePipelineVersion = Lens.field @"pipelineVersion"
{-# DEPRECATED pePipelineVersion "Use generic-lens or generic-optics with 'pipelineVersion' instead." #-}

-- | The status of the pipeline execution.
--
--
--     * InProgress: The pipeline execution is currently running.
--
--
--     * Stopped: The pipeline execution was manually stopped. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
--
--
--     * Stopping: The pipeline execution received a request to be manually stopped. Depending on the selected stop mode, the execution is either completing or abandoning in-progress actions. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .
--
--
--     * Succeeded: The pipeline execution was completed successfully.
--
--
--     * Superseded: While this pipeline execution was waiting for the next stage to be completed, a newer pipeline execution advanced and continued through the pipeline instead. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions> .
--
--
--     * Failed: The pipeline execution was not completed successfully.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peStatus :: Lens.Lens' PipelineExecution (Core.Maybe Types.PipelineExecutionStatus)
peStatus = Lens.field @"status"
{-# DEPRECATED peStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON PipelineExecution where
  parseJSON =
    Core.withObject "PipelineExecution" Core.$
      \x ->
        PipelineExecution'
          Core.<$> (x Core..:? "artifactRevisions")
          Core.<*> (x Core..:? "pipelineExecutionId")
          Core.<*> (x Core..:? "pipelineName")
          Core.<*> (x Core..:? "pipelineVersion")
          Core.<*> (x Core..:? "status")
