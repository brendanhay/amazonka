{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineExecutionSummary where

import Network.AWS.CodePipeline.Types.ExecutionTrigger
import Network.AWS.CodePipeline.Types.PipelineExecutionStatus
import Network.AWS.CodePipeline.Types.SourceRevision
import Network.AWS.CodePipeline.Types.StopExecutionTrigger
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary information about a pipeline execution.
--
-- /See:/ 'newPipelineExecutionSummary' smart constructor.
data PipelineExecutionSummary = PipelineExecutionSummary'
  { -- | The status of the pipeline execution.
    --
    -- -   InProgress: The pipeline execution is currently running.
    --
    -- -   Stopped: The pipeline execution was manually stopped. For more
    --     information, see
    --     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions>.
    --
    -- -   Stopping: The pipeline execution received a request to be manually
    --     stopped. Depending on the selected stop mode, the execution is
    --     either completing or abandoning in-progress actions. For more
    --     information, see
    --     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions>.
    --
    -- -   Succeeded: The pipeline execution was completed successfully.
    --
    -- -   Superseded: While this pipeline execution was waiting for the next
    --     stage to be completed, a newer pipeline execution advanced and
    --     continued through the pipeline instead. For more information, see
    --     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions>.
    --
    -- -   Failed: The pipeline execution was not completed successfully.
    status :: Core.Maybe PipelineExecutionStatus,
    -- | The date and time of the last change to the pipeline execution, in
    -- timestamp format.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The interaction or event that started a pipeline execution, such as
    -- automated change detection or a @StartPipelineExecution@ API call.
    trigger :: Core.Maybe ExecutionTrigger,
    -- | The date and time when the pipeline execution began, in timestamp
    -- format.
    startTime :: Core.Maybe Core.POSIX,
    -- | The interaction that stopped a pipeline execution.
    stopTrigger :: Core.Maybe StopExecutionTrigger,
    -- | A list of the source artifact revisions that initiated a pipeline
    -- execution.
    sourceRevisions :: Core.Maybe [SourceRevision],
    -- | The ID of the pipeline execution.
    pipelineExecutionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'pipelineExecutionSummary_status' - The status of the pipeline execution.
--
-- -   InProgress: The pipeline execution is currently running.
--
-- -   Stopped: The pipeline execution was manually stopped. For more
--     information, see
--     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions>.
--
-- -   Stopping: The pipeline execution received a request to be manually
--     stopped. Depending on the selected stop mode, the execution is
--     either completing or abandoning in-progress actions. For more
--     information, see
--     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions>.
--
-- -   Succeeded: The pipeline execution was completed successfully.
--
-- -   Superseded: While this pipeline execution was waiting for the next
--     stage to be completed, a newer pipeline execution advanced and
--     continued through the pipeline instead. For more information, see
--     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions>.
--
-- -   Failed: The pipeline execution was not completed successfully.
--
-- 'lastUpdateTime', 'pipelineExecutionSummary_lastUpdateTime' - The date and time of the last change to the pipeline execution, in
-- timestamp format.
--
-- 'trigger', 'pipelineExecutionSummary_trigger' - The interaction or event that started a pipeline execution, such as
-- automated change detection or a @StartPipelineExecution@ API call.
--
-- 'startTime', 'pipelineExecutionSummary_startTime' - The date and time when the pipeline execution began, in timestamp
-- format.
--
-- 'stopTrigger', 'pipelineExecutionSummary_stopTrigger' - The interaction that stopped a pipeline execution.
--
-- 'sourceRevisions', 'pipelineExecutionSummary_sourceRevisions' - A list of the source artifact revisions that initiated a pipeline
-- execution.
--
-- 'pipelineExecutionId', 'pipelineExecutionSummary_pipelineExecutionId' - The ID of the pipeline execution.
newPipelineExecutionSummary ::
  PipelineExecutionSummary
newPipelineExecutionSummary =
  PipelineExecutionSummary'
    { status = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      trigger = Core.Nothing,
      startTime = Core.Nothing,
      stopTrigger = Core.Nothing,
      sourceRevisions = Core.Nothing,
      pipelineExecutionId = Core.Nothing
    }

-- | The status of the pipeline execution.
--
-- -   InProgress: The pipeline execution is currently running.
--
-- -   Stopped: The pipeline execution was manually stopped. For more
--     information, see
--     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions>.
--
-- -   Stopping: The pipeline execution received a request to be manually
--     stopped. Depending on the selected stop mode, the execution is
--     either completing or abandoning in-progress actions. For more
--     information, see
--     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions>.
--
-- -   Succeeded: The pipeline execution was completed successfully.
--
-- -   Superseded: While this pipeline execution was waiting for the next
--     stage to be completed, a newer pipeline execution advanced and
--     continued through the pipeline instead. For more information, see
--     <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions>.
--
-- -   Failed: The pipeline execution was not completed successfully.
pipelineExecutionSummary_status :: Lens.Lens' PipelineExecutionSummary (Core.Maybe PipelineExecutionStatus)
pipelineExecutionSummary_status = Lens.lens (\PipelineExecutionSummary' {status} -> status) (\s@PipelineExecutionSummary' {} a -> s {status = a} :: PipelineExecutionSummary)

-- | The date and time of the last change to the pipeline execution, in
-- timestamp format.
pipelineExecutionSummary_lastUpdateTime :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.UTCTime)
pipelineExecutionSummary_lastUpdateTime = Lens.lens (\PipelineExecutionSummary' {lastUpdateTime} -> lastUpdateTime) (\s@PipelineExecutionSummary' {} a -> s {lastUpdateTime = a} :: PipelineExecutionSummary) Core.. Lens.mapping Core._Time

-- | The interaction or event that started a pipeline execution, such as
-- automated change detection or a @StartPipelineExecution@ API call.
pipelineExecutionSummary_trigger :: Lens.Lens' PipelineExecutionSummary (Core.Maybe ExecutionTrigger)
pipelineExecutionSummary_trigger = Lens.lens (\PipelineExecutionSummary' {trigger} -> trigger) (\s@PipelineExecutionSummary' {} a -> s {trigger = a} :: PipelineExecutionSummary)

-- | The date and time when the pipeline execution began, in timestamp
-- format.
pipelineExecutionSummary_startTime :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.UTCTime)
pipelineExecutionSummary_startTime = Lens.lens (\PipelineExecutionSummary' {startTime} -> startTime) (\s@PipelineExecutionSummary' {} a -> s {startTime = a} :: PipelineExecutionSummary) Core.. Lens.mapping Core._Time

-- | The interaction that stopped a pipeline execution.
pipelineExecutionSummary_stopTrigger :: Lens.Lens' PipelineExecutionSummary (Core.Maybe StopExecutionTrigger)
pipelineExecutionSummary_stopTrigger = Lens.lens (\PipelineExecutionSummary' {stopTrigger} -> stopTrigger) (\s@PipelineExecutionSummary' {} a -> s {stopTrigger = a} :: PipelineExecutionSummary)

-- | A list of the source artifact revisions that initiated a pipeline
-- execution.
pipelineExecutionSummary_sourceRevisions :: Lens.Lens' PipelineExecutionSummary (Core.Maybe [SourceRevision])
pipelineExecutionSummary_sourceRevisions = Lens.lens (\PipelineExecutionSummary' {sourceRevisions} -> sourceRevisions) (\s@PipelineExecutionSummary' {} a -> s {sourceRevisions = a} :: PipelineExecutionSummary) Core.. Lens.mapping Lens._Coerce

-- | The ID of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionId :: Lens.Lens' PipelineExecutionSummary (Core.Maybe Core.Text)
pipelineExecutionSummary_pipelineExecutionId = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionId} -> pipelineExecutionId) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionId = a} :: PipelineExecutionSummary)

instance Core.FromJSON PipelineExecutionSummary where
  parseJSON =
    Core.withObject
      "PipelineExecutionSummary"
      ( \x ->
          PipelineExecutionSummary'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "lastUpdateTime")
            Core.<*> (x Core..:? "trigger")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "stopTrigger")
            Core.<*> (x Core..:? "sourceRevisions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "pipelineExecutionId")
      )

instance Core.Hashable PipelineExecutionSummary

instance Core.NFData PipelineExecutionSummary
