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
-- Module      : Amazonka.SageMaker.Types.PipelineExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ParallelismConfiguration
import Amazonka.SageMaker.Types.Parameter
import Amazonka.SageMaker.Types.PipelineExecutionStatus
import Amazonka.SageMaker.Types.PipelineExperimentConfig
import Amazonka.SageMaker.Types.UserContext

-- | An execution of a pipeline.
--
-- /See:/ 'newPipelineExecution' smart constructor.
data PipelineExecution = PipelineExecution'
  { createdBy :: Prelude.Maybe UserContext,
    -- | The creation time of the pipeline execution.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | If the execution failed, a message describing why.
    failureReason :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The time that the pipeline execution was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The parallelism configuration applied to the pipeline execution.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The Amazon Resource Name (ARN) of the pipeline that was executed.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The status of the pipeline status.
    pipelineExecutionStatus :: Prelude.Maybe PipelineExecutionStatus,
    pipelineExperimentConfig :: Prelude.Maybe PipelineExperimentConfig,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Prelude.Maybe [Parameter]
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
-- 'createdBy', 'pipelineExecution_createdBy' - Undocumented member.
--
-- 'creationTime', 'pipelineExecution_creationTime' - The creation time of the pipeline execution.
--
-- 'failureReason', 'pipelineExecution_failureReason' - If the execution failed, a message describing why.
--
-- 'lastModifiedBy', 'pipelineExecution_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'pipelineExecution_lastModifiedTime' - The time that the pipeline execution was last modified.
--
-- 'parallelismConfiguration', 'pipelineExecution_parallelismConfiguration' - The parallelism configuration applied to the pipeline execution.
--
-- 'pipelineArn', 'pipelineExecution_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline that was executed.
--
-- 'pipelineExecutionArn', 'pipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'pipelineExecutionDescription', 'pipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'pipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionStatus', 'pipelineExecution_pipelineExecutionStatus' - The status of the pipeline status.
--
-- 'pipelineExperimentConfig', 'pipelineExecution_pipelineExperimentConfig' - Undocumented member.
--
-- 'pipelineParameters', 'pipelineExecution_pipelineParameters' - Contains a list of pipeline parameters. This list can be empty.
newPipelineExecution ::
  PipelineExecution
newPipelineExecution =
  PipelineExecution'
    { createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      parallelismConfiguration = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineExecutionArn = Prelude.Nothing,
      pipelineExecutionDescription = Prelude.Nothing,
      pipelineExecutionDisplayName = Prelude.Nothing,
      pipelineExecutionStatus = Prelude.Nothing,
      pipelineExperimentConfig = Prelude.Nothing,
      pipelineParameters = Prelude.Nothing
    }

-- | Undocumented member.
pipelineExecution_createdBy :: Lens.Lens' PipelineExecution (Prelude.Maybe UserContext)
pipelineExecution_createdBy = Lens.lens (\PipelineExecution' {createdBy} -> createdBy) (\s@PipelineExecution' {} a -> s {createdBy = a} :: PipelineExecution)

-- | The creation time of the pipeline execution.
pipelineExecution_creationTime :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.UTCTime)
pipelineExecution_creationTime = Lens.lens (\PipelineExecution' {creationTime} -> creationTime) (\s@PipelineExecution' {} a -> s {creationTime = a} :: PipelineExecution) Prelude.. Lens.mapping Data._Time

-- | If the execution failed, a message describing why.
pipelineExecution_failureReason :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_failureReason = Lens.lens (\PipelineExecution' {failureReason} -> failureReason) (\s@PipelineExecution' {} a -> s {failureReason = a} :: PipelineExecution)

-- | Undocumented member.
pipelineExecution_lastModifiedBy :: Lens.Lens' PipelineExecution (Prelude.Maybe UserContext)
pipelineExecution_lastModifiedBy = Lens.lens (\PipelineExecution' {lastModifiedBy} -> lastModifiedBy) (\s@PipelineExecution' {} a -> s {lastModifiedBy = a} :: PipelineExecution)

-- | The time that the pipeline execution was last modified.
pipelineExecution_lastModifiedTime :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.UTCTime)
pipelineExecution_lastModifiedTime = Lens.lens (\PipelineExecution' {lastModifiedTime} -> lastModifiedTime) (\s@PipelineExecution' {} a -> s {lastModifiedTime = a} :: PipelineExecution) Prelude.. Lens.mapping Data._Time

-- | The parallelism configuration applied to the pipeline execution.
pipelineExecution_parallelismConfiguration :: Lens.Lens' PipelineExecution (Prelude.Maybe ParallelismConfiguration)
pipelineExecution_parallelismConfiguration = Lens.lens (\PipelineExecution' {parallelismConfiguration} -> parallelismConfiguration) (\s@PipelineExecution' {} a -> s {parallelismConfiguration = a} :: PipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline that was executed.
pipelineExecution_pipelineArn :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineArn = Lens.lens (\PipelineExecution' {pipelineArn} -> pipelineArn) (\s@PipelineExecution' {} a -> s {pipelineArn = a} :: PipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
pipelineExecution_pipelineExecutionArn :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionArn = Lens.lens (\PipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@PipelineExecution' {} a -> s {pipelineExecutionArn = a} :: PipelineExecution)

-- | The description of the pipeline execution.
pipelineExecution_pipelineExecutionDescription :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionDescription = Lens.lens (\PipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@PipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: PipelineExecution)

-- | The display name of the pipeline execution.
pipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionDisplayName = Lens.lens (\PipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@PipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: PipelineExecution)

-- | The status of the pipeline status.
pipelineExecution_pipelineExecutionStatus :: Lens.Lens' PipelineExecution (Prelude.Maybe PipelineExecutionStatus)
pipelineExecution_pipelineExecutionStatus = Lens.lens (\PipelineExecution' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@PipelineExecution' {} a -> s {pipelineExecutionStatus = a} :: PipelineExecution)

-- | Undocumented member.
pipelineExecution_pipelineExperimentConfig :: Lens.Lens' PipelineExecution (Prelude.Maybe PipelineExperimentConfig)
pipelineExecution_pipelineExperimentConfig = Lens.lens (\PipelineExecution' {pipelineExperimentConfig} -> pipelineExperimentConfig) (\s@PipelineExecution' {} a -> s {pipelineExperimentConfig = a} :: PipelineExecution)

-- | Contains a list of pipeline parameters. This list can be empty.
pipelineExecution_pipelineParameters :: Lens.Lens' PipelineExecution (Prelude.Maybe [Parameter])
pipelineExecution_pipelineParameters = Lens.lens (\PipelineExecution' {pipelineParameters} -> pipelineParameters) (\s@PipelineExecution' {} a -> s {pipelineParameters = a} :: PipelineExecution) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PipelineExecution where
  parseJSON =
    Data.withObject
      "PipelineExecution"
      ( \x ->
          PipelineExecution'
            Prelude.<$> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ParallelismConfiguration")
            Prelude.<*> (x Data..:? "PipelineArn")
            Prelude.<*> (x Data..:? "PipelineExecutionArn")
            Prelude.<*> (x Data..:? "PipelineExecutionDescription")
            Prelude.<*> (x Data..:? "PipelineExecutionDisplayName")
            Prelude.<*> (x Data..:? "PipelineExecutionStatus")
            Prelude.<*> (x Data..:? "PipelineExperimentConfig")
            Prelude.<*> ( x
                            Data..:? "PipelineParameters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PipelineExecution where
  hashWithSalt _salt PipelineExecution' {..} =
    _salt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineExecutionArn
      `Prelude.hashWithSalt` pipelineExecutionDescription
      `Prelude.hashWithSalt` pipelineExecutionDisplayName
      `Prelude.hashWithSalt` pipelineExecutionStatus
      `Prelude.hashWithSalt` pipelineExperimentConfig
      `Prelude.hashWithSalt` pipelineParameters

instance Prelude.NFData PipelineExecution where
  rnf PipelineExecution' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineExecutionArn
      `Prelude.seq` Prelude.rnf pipelineExecutionDescription
      `Prelude.seq` Prelude.rnf pipelineExecutionDisplayName
      `Prelude.seq` Prelude.rnf pipelineExecutionStatus
      `Prelude.seq` Prelude.rnf pipelineExperimentConfig
      `Prelude.seq` Prelude.rnf pipelineParameters
