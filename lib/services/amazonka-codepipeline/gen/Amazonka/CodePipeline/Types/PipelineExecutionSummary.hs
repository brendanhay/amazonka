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
-- Module      : Amazonka.CodePipeline.Types.PipelineExecutionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.PipelineExecutionSummary where

import Amazonka.CodePipeline.Types.ExecutionTrigger
import Amazonka.CodePipeline.Types.PipelineExecutionStatus
import Amazonka.CodePipeline.Types.SourceRevision
import Amazonka.CodePipeline.Types.StopExecutionTrigger
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a pipeline execution.
--
-- /See:/ 'newPipelineExecutionSummary' smart constructor.
data PipelineExecutionSummary = PipelineExecutionSummary'
  { -- | The interaction or event that started a pipeline execution, such as
    -- automated change detection or a @StartPipelineExecution@ API call.
    trigger :: Prelude.Maybe ExecutionTrigger,
    -- | The ID of the pipeline execution.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe PipelineExecutionStatus,
    -- | A list of the source artifact revisions that initiated a pipeline
    -- execution.
    sourceRevisions :: Prelude.Maybe [SourceRevision],
    -- | The interaction that stopped a pipeline execution.
    stopTrigger :: Prelude.Maybe StopExecutionTrigger,
    -- | The date and time of the last change to the pipeline execution, in
    -- timestamp format.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time when the pipeline execution began, in timestamp
    -- format.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trigger', 'pipelineExecutionSummary_trigger' - The interaction or event that started a pipeline execution, such as
-- automated change detection or a @StartPipelineExecution@ API call.
--
-- 'pipelineExecutionId', 'pipelineExecutionSummary_pipelineExecutionId' - The ID of the pipeline execution.
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
-- 'sourceRevisions', 'pipelineExecutionSummary_sourceRevisions' - A list of the source artifact revisions that initiated a pipeline
-- execution.
--
-- 'stopTrigger', 'pipelineExecutionSummary_stopTrigger' - The interaction that stopped a pipeline execution.
--
-- 'lastUpdateTime', 'pipelineExecutionSummary_lastUpdateTime' - The date and time of the last change to the pipeline execution, in
-- timestamp format.
--
-- 'startTime', 'pipelineExecutionSummary_startTime' - The date and time when the pipeline execution began, in timestamp
-- format.
newPipelineExecutionSummary ::
  PipelineExecutionSummary
newPipelineExecutionSummary =
  PipelineExecutionSummary'
    { trigger =
        Prelude.Nothing,
      pipelineExecutionId = Prelude.Nothing,
      status = Prelude.Nothing,
      sourceRevisions = Prelude.Nothing,
      stopTrigger = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The interaction or event that started a pipeline execution, such as
-- automated change detection or a @StartPipelineExecution@ API call.
pipelineExecutionSummary_trigger :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe ExecutionTrigger)
pipelineExecutionSummary_trigger = Lens.lens (\PipelineExecutionSummary' {trigger} -> trigger) (\s@PipelineExecutionSummary' {} a -> s {trigger = a} :: PipelineExecutionSummary)

-- | The ID of the pipeline execution.
pipelineExecutionSummary_pipelineExecutionId :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe Prelude.Text)
pipelineExecutionSummary_pipelineExecutionId = Lens.lens (\PipelineExecutionSummary' {pipelineExecutionId} -> pipelineExecutionId) (\s@PipelineExecutionSummary' {} a -> s {pipelineExecutionId = a} :: PipelineExecutionSummary)

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
pipelineExecutionSummary_status :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe PipelineExecutionStatus)
pipelineExecutionSummary_status = Lens.lens (\PipelineExecutionSummary' {status} -> status) (\s@PipelineExecutionSummary' {} a -> s {status = a} :: PipelineExecutionSummary)

-- | A list of the source artifact revisions that initiated a pipeline
-- execution.
pipelineExecutionSummary_sourceRevisions :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe [SourceRevision])
pipelineExecutionSummary_sourceRevisions = Lens.lens (\PipelineExecutionSummary' {sourceRevisions} -> sourceRevisions) (\s@PipelineExecutionSummary' {} a -> s {sourceRevisions = a} :: PipelineExecutionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The interaction that stopped a pipeline execution.
pipelineExecutionSummary_stopTrigger :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe StopExecutionTrigger)
pipelineExecutionSummary_stopTrigger = Lens.lens (\PipelineExecutionSummary' {stopTrigger} -> stopTrigger) (\s@PipelineExecutionSummary' {} a -> s {stopTrigger = a} :: PipelineExecutionSummary)

-- | The date and time of the last change to the pipeline execution, in
-- timestamp format.
pipelineExecutionSummary_lastUpdateTime :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe Prelude.UTCTime)
pipelineExecutionSummary_lastUpdateTime = Lens.lens (\PipelineExecutionSummary' {lastUpdateTime} -> lastUpdateTime) (\s@PipelineExecutionSummary' {} a -> s {lastUpdateTime = a} :: PipelineExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | The date and time when the pipeline execution began, in timestamp
-- format.
pipelineExecutionSummary_startTime :: Lens.Lens' PipelineExecutionSummary (Prelude.Maybe Prelude.UTCTime)
pipelineExecutionSummary_startTime = Lens.lens (\PipelineExecutionSummary' {startTime} -> startTime) (\s@PipelineExecutionSummary' {} a -> s {startTime = a} :: PipelineExecutionSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON PipelineExecutionSummary where
  parseJSON =
    Core.withObject
      "PipelineExecutionSummary"
      ( \x ->
          PipelineExecutionSummary'
            Prelude.<$> (x Core..:? "trigger")
            Prelude.<*> (x Core..:? "pipelineExecutionId")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> ( x Core..:? "sourceRevisions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "stopTrigger")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "startTime")
      )

instance Prelude.Hashable PipelineExecutionSummary where
  hashWithSalt _salt PipelineExecutionSummary' {..} =
    _salt `Prelude.hashWithSalt` trigger
      `Prelude.hashWithSalt` pipelineExecutionId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sourceRevisions
      `Prelude.hashWithSalt` stopTrigger
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData PipelineExecutionSummary where
  rnf PipelineExecutionSummary' {..} =
    Prelude.rnf trigger
      `Prelude.seq` Prelude.rnf pipelineExecutionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sourceRevisions
      `Prelude.seq` Prelude.rnf stopTrigger
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf startTime
