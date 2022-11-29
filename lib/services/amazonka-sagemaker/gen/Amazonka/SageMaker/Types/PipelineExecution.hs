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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The Amazon Resource Name (ARN) of the pipeline that was executed.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    pipelineExperimentConfig :: Prelude.Maybe PipelineExperimentConfig,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Prelude.Maybe [Parameter],
    -- | The time that the pipeline execution was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The parallelism configuration applied to the pipeline execution.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the pipeline execution.
    creationTime :: Prelude.Maybe Core.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    createdBy :: Prelude.Maybe UserContext,
    -- | The status of the pipeline status.
    pipelineExecutionStatus :: Prelude.Maybe PipelineExecutionStatus,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | If the execution failed, a message describing why.
    failureReason :: Prelude.Maybe Prelude.Text
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
-- 'pipelineArn', 'pipelineExecution_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline that was executed.
--
-- 'pipelineExperimentConfig', 'pipelineExecution_pipelineExperimentConfig' - Undocumented member.
--
-- 'pipelineParameters', 'pipelineExecution_pipelineParameters' - Contains a list of pipeline parameters. This list can be empty.
--
-- 'lastModifiedTime', 'pipelineExecution_lastModifiedTime' - The time that the pipeline execution was last modified.
--
-- 'parallelismConfiguration', 'pipelineExecution_parallelismConfiguration' - The parallelism configuration applied to the pipeline execution.
--
-- 'pipelineExecutionDescription', 'pipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'creationTime', 'pipelineExecution_creationTime' - The creation time of the pipeline execution.
--
-- 'lastModifiedBy', 'pipelineExecution_lastModifiedBy' - Undocumented member.
--
-- 'createdBy', 'pipelineExecution_createdBy' - Undocumented member.
--
-- 'pipelineExecutionStatus', 'pipelineExecution_pipelineExecutionStatus' - The status of the pipeline status.
--
-- 'pipelineExecutionDisplayName', 'pipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionArn', 'pipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'failureReason', 'pipelineExecution_failureReason' - If the execution failed, a message describing why.
newPipelineExecution ::
  PipelineExecution
newPipelineExecution =
  PipelineExecution'
    { pipelineArn = Prelude.Nothing,
      pipelineExperimentConfig = Prelude.Nothing,
      pipelineParameters = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      parallelismConfiguration = Prelude.Nothing,
      pipelineExecutionDescription = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      pipelineExecutionStatus = Prelude.Nothing,
      pipelineExecutionDisplayName = Prelude.Nothing,
      pipelineExecutionArn = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline that was executed.
pipelineExecution_pipelineArn :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineArn = Lens.lens (\PipelineExecution' {pipelineArn} -> pipelineArn) (\s@PipelineExecution' {} a -> s {pipelineArn = a} :: PipelineExecution)

-- | Undocumented member.
pipelineExecution_pipelineExperimentConfig :: Lens.Lens' PipelineExecution (Prelude.Maybe PipelineExperimentConfig)
pipelineExecution_pipelineExperimentConfig = Lens.lens (\PipelineExecution' {pipelineExperimentConfig} -> pipelineExperimentConfig) (\s@PipelineExecution' {} a -> s {pipelineExperimentConfig = a} :: PipelineExecution)

-- | Contains a list of pipeline parameters. This list can be empty.
pipelineExecution_pipelineParameters :: Lens.Lens' PipelineExecution (Prelude.Maybe [Parameter])
pipelineExecution_pipelineParameters = Lens.lens (\PipelineExecution' {pipelineParameters} -> pipelineParameters) (\s@PipelineExecution' {} a -> s {pipelineParameters = a} :: PipelineExecution) Prelude.. Lens.mapping Lens.coerced

-- | The time that the pipeline execution was last modified.
pipelineExecution_lastModifiedTime :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.UTCTime)
pipelineExecution_lastModifiedTime = Lens.lens (\PipelineExecution' {lastModifiedTime} -> lastModifiedTime) (\s@PipelineExecution' {} a -> s {lastModifiedTime = a} :: PipelineExecution) Prelude.. Lens.mapping Core._Time

-- | The parallelism configuration applied to the pipeline execution.
pipelineExecution_parallelismConfiguration :: Lens.Lens' PipelineExecution (Prelude.Maybe ParallelismConfiguration)
pipelineExecution_parallelismConfiguration = Lens.lens (\PipelineExecution' {parallelismConfiguration} -> parallelismConfiguration) (\s@PipelineExecution' {} a -> s {parallelismConfiguration = a} :: PipelineExecution)

-- | The description of the pipeline execution.
pipelineExecution_pipelineExecutionDescription :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionDescription = Lens.lens (\PipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@PipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: PipelineExecution)

-- | The creation time of the pipeline execution.
pipelineExecution_creationTime :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.UTCTime)
pipelineExecution_creationTime = Lens.lens (\PipelineExecution' {creationTime} -> creationTime) (\s@PipelineExecution' {} a -> s {creationTime = a} :: PipelineExecution) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
pipelineExecution_lastModifiedBy :: Lens.Lens' PipelineExecution (Prelude.Maybe UserContext)
pipelineExecution_lastModifiedBy = Lens.lens (\PipelineExecution' {lastModifiedBy} -> lastModifiedBy) (\s@PipelineExecution' {} a -> s {lastModifiedBy = a} :: PipelineExecution)

-- | Undocumented member.
pipelineExecution_createdBy :: Lens.Lens' PipelineExecution (Prelude.Maybe UserContext)
pipelineExecution_createdBy = Lens.lens (\PipelineExecution' {createdBy} -> createdBy) (\s@PipelineExecution' {} a -> s {createdBy = a} :: PipelineExecution)

-- | The status of the pipeline status.
pipelineExecution_pipelineExecutionStatus :: Lens.Lens' PipelineExecution (Prelude.Maybe PipelineExecutionStatus)
pipelineExecution_pipelineExecutionStatus = Lens.lens (\PipelineExecution' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@PipelineExecution' {} a -> s {pipelineExecutionStatus = a} :: PipelineExecution)

-- | The display name of the pipeline execution.
pipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionDisplayName = Lens.lens (\PipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@PipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: PipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
pipelineExecution_pipelineExecutionArn :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionArn = Lens.lens (\PipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@PipelineExecution' {} a -> s {pipelineExecutionArn = a} :: PipelineExecution)

-- | If the execution failed, a message describing why.
pipelineExecution_failureReason :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_failureReason = Lens.lens (\PipelineExecution' {failureReason} -> failureReason) (\s@PipelineExecution' {} a -> s {failureReason = a} :: PipelineExecution)

instance Core.FromJSON PipelineExecution where
  parseJSON =
    Core.withObject
      "PipelineExecution"
      ( \x ->
          PipelineExecution'
            Prelude.<$> (x Core..:? "PipelineArn")
            Prelude.<*> (x Core..:? "PipelineExperimentConfig")
            Prelude.<*> ( x Core..:? "PipelineParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ParallelismConfiguration")
            Prelude.<*> (x Core..:? "PipelineExecutionDescription")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "PipelineExecutionStatus")
            Prelude.<*> (x Core..:? "PipelineExecutionDisplayName")
            Prelude.<*> (x Core..:? "PipelineExecutionArn")
            Prelude.<*> (x Core..:? "FailureReason")
      )

instance Prelude.Hashable PipelineExecution where
  hashWithSalt _salt PipelineExecution' {..} =
    _salt `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineExperimentConfig
      `Prelude.hashWithSalt` pipelineParameters
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineExecutionDescription
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` pipelineExecutionStatus
      `Prelude.hashWithSalt` pipelineExecutionDisplayName
      `Prelude.hashWithSalt` pipelineExecutionArn
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData PipelineExecution where
  rnf PipelineExecution' {..} =
    Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineExperimentConfig
      `Prelude.seq` Prelude.rnf pipelineParameters
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineExecutionDescription
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf pipelineExecutionStatus
      `Prelude.seq` Prelude.rnf pipelineExecutionDisplayName
      `Prelude.seq` Prelude.rnf pipelineExecutionArn
      `Prelude.seq` Prelude.rnf failureReason
