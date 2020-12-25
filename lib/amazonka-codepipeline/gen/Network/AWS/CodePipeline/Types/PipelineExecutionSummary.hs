{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineExecutionSummary
  ( PipelineExecutionSummary (..),

    -- * Smart constructor
    mkPipelineExecutionSummary,

    -- * Lenses
    pesLastUpdateTime,
    pesPipelineExecutionId,
    pesSourceRevisions,
    pesStartTime,
    pesStatus,
    pesStopTrigger,
    pesTrigger,
  )
where

import qualified Network.AWS.CodePipeline.Types.ExecutionTrigger as Types
import qualified Network.AWS.CodePipeline.Types.PipelineExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.PipelineExecutionStatus as Types
import qualified Network.AWS.CodePipeline.Types.SourceRevision as Types
import qualified Network.AWS.CodePipeline.Types.StopExecutionTrigger as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information about a pipeline execution.
--
-- /See:/ 'mkPipelineExecutionSummary' smart constructor.
data PipelineExecutionSummary = PipelineExecutionSummary'
  { -- | The date and time of the last change to the pipeline execution, in timestamp format.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the pipeline execution.
    pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId,
    -- | A list of the source artifact revisions that initiated a pipeline execution.
    sourceRevisions :: Core.Maybe [Types.SourceRevision],
    -- | The date and time when the pipeline execution began, in timestamp format.
    startTime :: Core.Maybe Core.NominalDiffTime,
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
    status :: Core.Maybe Types.PipelineExecutionStatus,
    -- | The interaction that stopped a pipeline execution.
    stopTrigger :: Core.Maybe Types.StopExecutionTrigger,
    -- | The interaction or event that started a pipeline execution, such as automated change detection or a @StartPipelineExecution@ API call.
    trigger :: Core.Maybe Types.ExecutionTrigger
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PipelineExecutionSummary' value with any optional fields omitted.
mkPipelineExecutionSummary ::
  PipelineExecutionSummary
mkPipelineExecutionSummary =
  PipelineExecutionSummary'
    { lastUpdateTime = Core.Nothing,
      pipelineExecutionId = Core.Nothing,
      sourceRevisions = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      stopTrigger = Core.Nothing,
      trigger = Core.Nothing
    }

-- | The date and time of the last change to the pipeline execution, in timestamp format.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesLastUpdateTime :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.NominalDiffTime)
pesLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED pesLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The ID of the pipeline execution.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPipelineExecutionId :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Types.PipelineExecutionId)
pesPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED pesPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | A list of the source artifact revisions that initiated a pipeline execution.
--
-- /Note:/ Consider using 'sourceRevisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesSourceRevisions :: Lens.Lens' PipelineExecutionSummary (Core.Maybe [Types.SourceRevision])
pesSourceRevisions = Lens.field @"sourceRevisions"
{-# DEPRECATED pesSourceRevisions "Use generic-lens or generic-optics with 'sourceRevisions' instead." #-}

-- | The date and time when the pipeline execution began, in timestamp format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesStartTime :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.NominalDiffTime)
pesStartTime = Lens.field @"startTime"
{-# DEPRECATED pesStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
pesStatus :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Types.PipelineExecutionStatus)
pesStatus = Lens.field @"status"
{-# DEPRECATED pesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The interaction that stopped a pipeline execution.
--
-- /Note:/ Consider using 'stopTrigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesStopTrigger :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Types.StopExecutionTrigger)
pesStopTrigger = Lens.field @"stopTrigger"
{-# DEPRECATED pesStopTrigger "Use generic-lens or generic-optics with 'stopTrigger' instead." #-}

-- | The interaction or event that started a pipeline execution, such as automated change detection or a @StartPipelineExecution@ API call.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesTrigger :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Types.ExecutionTrigger)
pesTrigger = Lens.field @"trigger"
{-# DEPRECATED pesTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

instance Core.FromJSON PipelineExecutionSummary where
  parseJSON =
    Core.withObject "PipelineExecutionSummary" Core.$
      \x ->
        PipelineExecutionSummary'
          Core.<$> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "pipelineExecutionId")
          Core.<*> (x Core..:? "sourceRevisions")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "stopTrigger")
          Core.<*> (x Core..:? "trigger")
