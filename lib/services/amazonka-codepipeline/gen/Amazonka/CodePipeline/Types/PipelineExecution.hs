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
-- Module      : Amazonka.CodePipeline.Types.PipelineExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.PipelineExecution where

import Amazonka.CodePipeline.Types.ArtifactRevision
import Amazonka.CodePipeline.Types.PipelineExecutionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an execution of a pipeline.
--
-- /See:/ 'newPipelineExecution' smart constructor.
data PipelineExecution = PipelineExecution'
  { -- | A list of @ArtifactRevision@ objects included in a pipeline execution.
    artifactRevisions :: Prelude.Maybe [ArtifactRevision],
    -- | The ID of the pipeline execution.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline with the specified pipeline execution.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the pipeline with the specified pipeline
    -- execution.
    pipelineVersion :: Prelude.Maybe Prelude.Natural,
    -- | The status of the pipeline execution.
    --
    -- -   Cancelled: The pipeline’s definition was updated before the pipeline
    --     execution could be completed.
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
    -- | A summary that contains a description of the pipeline execution status.
    statusSummary :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactRevisions', 'pipelineExecution_artifactRevisions' - A list of @ArtifactRevision@ objects included in a pipeline execution.
--
-- 'pipelineExecutionId', 'pipelineExecution_pipelineExecutionId' - The ID of the pipeline execution.
--
-- 'pipelineName', 'pipelineExecution_pipelineName' - The name of the pipeline with the specified pipeline execution.
--
-- 'pipelineVersion', 'pipelineExecution_pipelineVersion' - The version number of the pipeline with the specified pipeline
-- execution.
--
-- 'status', 'pipelineExecution_status' - The status of the pipeline execution.
--
-- -   Cancelled: The pipeline’s definition was updated before the pipeline
--     execution could be completed.
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
-- 'statusSummary', 'pipelineExecution_statusSummary' - A summary that contains a description of the pipeline execution status.
newPipelineExecution ::
  PipelineExecution
newPipelineExecution =
  PipelineExecution'
    { artifactRevisions =
        Prelude.Nothing,
      pipelineExecutionId = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      pipelineVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      statusSummary = Prelude.Nothing
    }

-- | A list of @ArtifactRevision@ objects included in a pipeline execution.
pipelineExecution_artifactRevisions :: Lens.Lens' PipelineExecution (Prelude.Maybe [ArtifactRevision])
pipelineExecution_artifactRevisions = Lens.lens (\PipelineExecution' {artifactRevisions} -> artifactRevisions) (\s@PipelineExecution' {} a -> s {artifactRevisions = a} :: PipelineExecution) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the pipeline execution.
pipelineExecution_pipelineExecutionId :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionId = Lens.lens (\PipelineExecution' {pipelineExecutionId} -> pipelineExecutionId) (\s@PipelineExecution' {} a -> s {pipelineExecutionId = a} :: PipelineExecution)

-- | The name of the pipeline with the specified pipeline execution.
pipelineExecution_pipelineName :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineName = Lens.lens (\PipelineExecution' {pipelineName} -> pipelineName) (\s@PipelineExecution' {} a -> s {pipelineName = a} :: PipelineExecution)

-- | The version number of the pipeline with the specified pipeline
-- execution.
pipelineExecution_pipelineVersion :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Natural)
pipelineExecution_pipelineVersion = Lens.lens (\PipelineExecution' {pipelineVersion} -> pipelineVersion) (\s@PipelineExecution' {} a -> s {pipelineVersion = a} :: PipelineExecution)

-- | The status of the pipeline execution.
--
-- -   Cancelled: The pipeline’s definition was updated before the pipeline
--     execution could be completed.
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
pipelineExecution_status :: Lens.Lens' PipelineExecution (Prelude.Maybe PipelineExecutionStatus)
pipelineExecution_status = Lens.lens (\PipelineExecution' {status} -> status) (\s@PipelineExecution' {} a -> s {status = a} :: PipelineExecution)

-- | A summary that contains a description of the pipeline execution status.
pipelineExecution_statusSummary :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_statusSummary = Lens.lens (\PipelineExecution' {statusSummary} -> statusSummary) (\s@PipelineExecution' {} a -> s {statusSummary = a} :: PipelineExecution)

instance Data.FromJSON PipelineExecution where
  parseJSON =
    Data.withObject
      "PipelineExecution"
      ( \x ->
          PipelineExecution'
            Prelude.<$> ( x
                            Data..:? "artifactRevisions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "pipelineExecutionId")
            Prelude.<*> (x Data..:? "pipelineName")
            Prelude.<*> (x Data..:? "pipelineVersion")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusSummary")
      )

instance Prelude.Hashable PipelineExecution where
  hashWithSalt _salt PipelineExecution' {..} =
    _salt
      `Prelude.hashWithSalt` artifactRevisions
      `Prelude.hashWithSalt` pipelineExecutionId
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` pipelineVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusSummary

instance Prelude.NFData PipelineExecution where
  rnf PipelineExecution' {..} =
    Prelude.rnf artifactRevisions
      `Prelude.seq` Prelude.rnf pipelineExecutionId
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf pipelineVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusSummary
