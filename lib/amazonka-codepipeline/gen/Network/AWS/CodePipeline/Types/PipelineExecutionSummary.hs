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
    pesStatus,
    pesStartTime,
    pesStopTrigger,
    pesPipelineExecutionId,
    pesSourceRevisions,
    pesTrigger,
    pesLastUpdateTime,
  )
where

import Network.AWS.CodePipeline.Types.ExecutionTrigger
import Network.AWS.CodePipeline.Types.PipelineExecutionStatus
import Network.AWS.CodePipeline.Types.SourceRevision
import Network.AWS.CodePipeline.Types.StopExecutionTrigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about a pipeline execution.
--
-- /See:/ 'mkPipelineExecutionSummary' smart constructor.
data PipelineExecutionSummary = PipelineExecutionSummary'
  { status ::
      Lude.Maybe PipelineExecutionStatus,
    startTime :: Lude.Maybe Lude.Timestamp,
    stopTrigger ::
      Lude.Maybe StopExecutionTrigger,
    pipelineExecutionId ::
      Lude.Maybe Lude.Text,
    sourceRevisions ::
      Lude.Maybe [SourceRevision],
    trigger :: Lude.Maybe ExecutionTrigger,
    lastUpdateTime ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineExecutionSummary' with the minimum fields required to make a request.
--
-- * 'lastUpdateTime' - The date and time of the last change to the pipeline execution, in timestamp format.
-- * 'pipelineExecutionId' - The ID of the pipeline execution.
-- * 'sourceRevisions' - A list of the source artifact revisions that initiated a pipeline execution.
-- * 'startTime' - The date and time when the pipeline execution began, in timestamp format.
-- * 'status' - The status of the pipeline execution.
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
-- * 'stopTrigger' - The interaction that stopped a pipeline execution.
-- * 'trigger' - The interaction or event that started a pipeline execution, such as automated change detection or a @StartPipelineExecution@ API call.
mkPipelineExecutionSummary ::
  PipelineExecutionSummary
mkPipelineExecutionSummary =
  PipelineExecutionSummary'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      stopTrigger = Lude.Nothing,
      pipelineExecutionId = Lude.Nothing,
      sourceRevisions = Lude.Nothing,
      trigger = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

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
pesStatus :: Lens.Lens' PipelineExecutionSummary (Lude.Maybe PipelineExecutionStatus)
pesStatus = Lens.lens (status :: PipelineExecutionSummary -> Lude.Maybe PipelineExecutionStatus) (\s a -> s {status = a} :: PipelineExecutionSummary)
{-# DEPRECATED pesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time when the pipeline execution began, in timestamp format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesStartTime :: Lens.Lens' PipelineExecutionSummary (Lude.Maybe Lude.Timestamp)
pesStartTime = Lens.lens (startTime :: PipelineExecutionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: PipelineExecutionSummary)
{-# DEPRECATED pesStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The interaction that stopped a pipeline execution.
--
-- /Note:/ Consider using 'stopTrigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesStopTrigger :: Lens.Lens' PipelineExecutionSummary (Lude.Maybe StopExecutionTrigger)
pesStopTrigger = Lens.lens (stopTrigger :: PipelineExecutionSummary -> Lude.Maybe StopExecutionTrigger) (\s a -> s {stopTrigger = a} :: PipelineExecutionSummary)
{-# DEPRECATED pesStopTrigger "Use generic-lens or generic-optics with 'stopTrigger' instead." #-}

-- | The ID of the pipeline execution.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesPipelineExecutionId :: Lens.Lens' PipelineExecutionSummary (Lude.Maybe Lude.Text)
pesPipelineExecutionId = Lens.lens (pipelineExecutionId :: PipelineExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: PipelineExecutionSummary)
{-# DEPRECATED pesPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | A list of the source artifact revisions that initiated a pipeline execution.
--
-- /Note:/ Consider using 'sourceRevisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesSourceRevisions :: Lens.Lens' PipelineExecutionSummary (Lude.Maybe [SourceRevision])
pesSourceRevisions = Lens.lens (sourceRevisions :: PipelineExecutionSummary -> Lude.Maybe [SourceRevision]) (\s a -> s {sourceRevisions = a} :: PipelineExecutionSummary)
{-# DEPRECATED pesSourceRevisions "Use generic-lens or generic-optics with 'sourceRevisions' instead." #-}

-- | The interaction or event that started a pipeline execution, such as automated change detection or a @StartPipelineExecution@ API call.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesTrigger :: Lens.Lens' PipelineExecutionSummary (Lude.Maybe ExecutionTrigger)
pesTrigger = Lens.lens (trigger :: PipelineExecutionSummary -> Lude.Maybe ExecutionTrigger) (\s a -> s {trigger = a} :: PipelineExecutionSummary)
{-# DEPRECATED pesTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

-- | The date and time of the last change to the pipeline execution, in timestamp format.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesLastUpdateTime :: Lens.Lens' PipelineExecutionSummary (Lude.Maybe Lude.Timestamp)
pesLastUpdateTime = Lens.lens (lastUpdateTime :: PipelineExecutionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: PipelineExecutionSummary)
{-# DEPRECATED pesLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON PipelineExecutionSummary where
  parseJSON =
    Lude.withObject
      "PipelineExecutionSummary"
      ( \x ->
          PipelineExecutionSummary'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "stopTrigger")
            Lude.<*> (x Lude..:? "pipelineExecutionId")
            Lude.<*> (x Lude..:? "sourceRevisions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "trigger")
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
