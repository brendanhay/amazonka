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
-- Module      : Network.AWS.SageMaker.Types.PipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineExecution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.Parameter
import Network.AWS.SageMaker.Types.PipelineExecutionStatus
import Network.AWS.SageMaker.Types.PipelineExperimentConfig
import Network.AWS.SageMaker.Types.UserContext

-- | An execution of a pipeline.
--
-- /See:/ 'newPipelineExecution' smart constructor.
data PipelineExecution = PipelineExecution'
  { -- | The Amazon Resource Name (ARN) of the pipeline that was executed.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the pipeline execution.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Prelude.Maybe [Parameter],
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The status of the pipeline status.
    pipelineExecutionStatus :: Prelude.Maybe PipelineExecutionStatus,
    -- | If the execution failed, a message describing why.
    failureReason :: Prelude.Maybe Prelude.Text,
    pipelineExperimentConfig :: Prelude.Maybe PipelineExperimentConfig,
    -- | The time that the pipeline execution was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'pipelineExecution_creationTime' - The creation time of the pipeline execution.
--
-- 'pipelineExecutionDescription', 'pipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineParameters', 'pipelineExecution_pipelineParameters' - Contains a list of pipeline parameters. This list can be empty.
--
-- 'pipelineExecutionDisplayName', 'pipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionStatus', 'pipelineExecution_pipelineExecutionStatus' - The status of the pipeline status.
--
-- 'failureReason', 'pipelineExecution_failureReason' - If the execution failed, a message describing why.
--
-- 'pipelineExperimentConfig', 'pipelineExecution_pipelineExperimentConfig' - Undocumented member.
--
-- 'lastModifiedTime', 'pipelineExecution_lastModifiedTime' - The time that the pipeline execution was last modified.
--
-- 'createdBy', 'pipelineExecution_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'pipelineExecution_lastModifiedBy' - Undocumented member.
--
-- 'pipelineExecutionArn', 'pipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newPipelineExecution ::
  PipelineExecution
newPipelineExecution =
  PipelineExecution'
    { pipelineArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      pipelineExecutionDescription = Prelude.Nothing,
      pipelineParameters = Prelude.Nothing,
      pipelineExecutionDisplayName = Prelude.Nothing,
      pipelineExecutionStatus = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      pipelineExperimentConfig = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      pipelineExecutionArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline that was executed.
pipelineExecution_pipelineArn :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineArn = Lens.lens (\PipelineExecution' {pipelineArn} -> pipelineArn) (\s@PipelineExecution' {} a -> s {pipelineArn = a} :: PipelineExecution)

-- | The creation time of the pipeline execution.
pipelineExecution_creationTime :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.UTCTime)
pipelineExecution_creationTime = Lens.lens (\PipelineExecution' {creationTime} -> creationTime) (\s@PipelineExecution' {} a -> s {creationTime = a} :: PipelineExecution) Prelude.. Lens.mapping Core._Time

-- | The description of the pipeline execution.
pipelineExecution_pipelineExecutionDescription :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionDescription = Lens.lens (\PipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@PipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: PipelineExecution)

-- | Contains a list of pipeline parameters. This list can be empty.
pipelineExecution_pipelineParameters :: Lens.Lens' PipelineExecution (Prelude.Maybe [Parameter])
pipelineExecution_pipelineParameters = Lens.lens (\PipelineExecution' {pipelineParameters} -> pipelineParameters) (\s@PipelineExecution' {} a -> s {pipelineParameters = a} :: PipelineExecution) Prelude.. Lens.mapping Lens._Coerce

-- | The display name of the pipeline execution.
pipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionDisplayName = Lens.lens (\PipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@PipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: PipelineExecution)

-- | The status of the pipeline status.
pipelineExecution_pipelineExecutionStatus :: Lens.Lens' PipelineExecution (Prelude.Maybe PipelineExecutionStatus)
pipelineExecution_pipelineExecutionStatus = Lens.lens (\PipelineExecution' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@PipelineExecution' {} a -> s {pipelineExecutionStatus = a} :: PipelineExecution)

-- | If the execution failed, a message describing why.
pipelineExecution_failureReason :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_failureReason = Lens.lens (\PipelineExecution' {failureReason} -> failureReason) (\s@PipelineExecution' {} a -> s {failureReason = a} :: PipelineExecution)

-- | Undocumented member.
pipelineExecution_pipelineExperimentConfig :: Lens.Lens' PipelineExecution (Prelude.Maybe PipelineExperimentConfig)
pipelineExecution_pipelineExperimentConfig = Lens.lens (\PipelineExecution' {pipelineExperimentConfig} -> pipelineExperimentConfig) (\s@PipelineExecution' {} a -> s {pipelineExperimentConfig = a} :: PipelineExecution)

-- | The time that the pipeline execution was last modified.
pipelineExecution_lastModifiedTime :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.UTCTime)
pipelineExecution_lastModifiedTime = Lens.lens (\PipelineExecution' {lastModifiedTime} -> lastModifiedTime) (\s@PipelineExecution' {} a -> s {lastModifiedTime = a} :: PipelineExecution) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
pipelineExecution_createdBy :: Lens.Lens' PipelineExecution (Prelude.Maybe UserContext)
pipelineExecution_createdBy = Lens.lens (\PipelineExecution' {createdBy} -> createdBy) (\s@PipelineExecution' {} a -> s {createdBy = a} :: PipelineExecution)

-- | Undocumented member.
pipelineExecution_lastModifiedBy :: Lens.Lens' PipelineExecution (Prelude.Maybe UserContext)
pipelineExecution_lastModifiedBy = Lens.lens (\PipelineExecution' {lastModifiedBy} -> lastModifiedBy) (\s@PipelineExecution' {} a -> s {lastModifiedBy = a} :: PipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
pipelineExecution_pipelineExecutionArn :: Lens.Lens' PipelineExecution (Prelude.Maybe Prelude.Text)
pipelineExecution_pipelineExecutionArn = Lens.lens (\PipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@PipelineExecution' {} a -> s {pipelineExecutionArn = a} :: PipelineExecution)

instance Core.FromJSON PipelineExecution where
  parseJSON =
    Core.withObject
      "PipelineExecution"
      ( \x ->
          PipelineExecution'
            Prelude.<$> (x Core..:? "PipelineArn")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "PipelineExecutionDescription")
            Prelude.<*> ( x Core..:? "PipelineParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PipelineExecutionDisplayName")
            Prelude.<*> (x Core..:? "PipelineExecutionStatus")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "PipelineExperimentConfig")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "PipelineExecutionArn")
      )

instance Prelude.Hashable PipelineExecution

instance Prelude.NFData PipelineExecution
