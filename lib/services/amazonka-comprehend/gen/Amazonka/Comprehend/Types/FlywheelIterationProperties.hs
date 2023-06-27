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
-- Module      : Amazonka.Comprehend.Types.FlywheelIterationProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelIterationProperties where

import Amazonka.Comprehend.Types.FlywheelIterationStatus
import Amazonka.Comprehend.Types.FlywheelModelEvaluationMetrics
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration properties of a flywheel iteration.
--
-- /See:/ 'newFlywheelIterationProperties' smart constructor.
data FlywheelIterationProperties = FlywheelIterationProperties'
  { -- | The creation start time of the flywheel iteration.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The completion time of this flywheel iteration.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the evaluated model associated with this flywheel iteration.
    evaluatedModelArn :: Prelude.Maybe Prelude.Text,
    evaluatedModelMetrics :: Prelude.Maybe FlywheelModelEvaluationMetrics,
    evaluationManifestS3Prefix :: Prelude.Maybe Prelude.Text,
    flywheelArn :: Prelude.Maybe Prelude.Text,
    flywheelIterationId :: Prelude.Maybe Prelude.Text,
    -- | A description of the status of the flywheel iteration.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the flywheel iteration.
    status :: Prelude.Maybe FlywheelIterationStatus,
    -- | The ARN of the trained model associated with this flywheel iteration.
    trainedModelArn :: Prelude.Maybe Prelude.Text,
    -- | The metrics associated with the trained model.
    trainedModelMetrics :: Prelude.Maybe FlywheelModelEvaluationMetrics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlywheelIterationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'flywheelIterationProperties_creationTime' - The creation start time of the flywheel iteration.
--
-- 'endTime', 'flywheelIterationProperties_endTime' - The completion time of this flywheel iteration.
--
-- 'evaluatedModelArn', 'flywheelIterationProperties_evaluatedModelArn' - The ARN of the evaluated model associated with this flywheel iteration.
--
-- 'evaluatedModelMetrics', 'flywheelIterationProperties_evaluatedModelMetrics' - Undocumented member.
--
-- 'evaluationManifestS3Prefix', 'flywheelIterationProperties_evaluationManifestS3Prefix' -
--
-- 'flywheelArn', 'flywheelIterationProperties_flywheelArn' -
--
-- 'flywheelIterationId', 'flywheelIterationProperties_flywheelIterationId' -
--
-- 'message', 'flywheelIterationProperties_message' - A description of the status of the flywheel iteration.
--
-- 'status', 'flywheelIterationProperties_status' - The status of the flywheel iteration.
--
-- 'trainedModelArn', 'flywheelIterationProperties_trainedModelArn' - The ARN of the trained model associated with this flywheel iteration.
--
-- 'trainedModelMetrics', 'flywheelIterationProperties_trainedModelMetrics' - The metrics associated with the trained model.
newFlywheelIterationProperties ::
  FlywheelIterationProperties
newFlywheelIterationProperties =
  FlywheelIterationProperties'
    { creationTime =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      evaluatedModelArn = Prelude.Nothing,
      evaluatedModelMetrics = Prelude.Nothing,
      evaluationManifestS3Prefix = Prelude.Nothing,
      flywheelArn = Prelude.Nothing,
      flywheelIterationId = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing,
      trainedModelArn = Prelude.Nothing,
      trainedModelMetrics = Prelude.Nothing
    }

-- | The creation start time of the flywheel iteration.
flywheelIterationProperties_creationTime :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.UTCTime)
flywheelIterationProperties_creationTime = Lens.lens (\FlywheelIterationProperties' {creationTime} -> creationTime) (\s@FlywheelIterationProperties' {} a -> s {creationTime = a} :: FlywheelIterationProperties) Prelude.. Lens.mapping Data._Time

-- | The completion time of this flywheel iteration.
flywheelIterationProperties_endTime :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.UTCTime)
flywheelIterationProperties_endTime = Lens.lens (\FlywheelIterationProperties' {endTime} -> endTime) (\s@FlywheelIterationProperties' {} a -> s {endTime = a} :: FlywheelIterationProperties) Prelude.. Lens.mapping Data._Time

-- | The ARN of the evaluated model associated with this flywheel iteration.
flywheelIterationProperties_evaluatedModelArn :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.Text)
flywheelIterationProperties_evaluatedModelArn = Lens.lens (\FlywheelIterationProperties' {evaluatedModelArn} -> evaluatedModelArn) (\s@FlywheelIterationProperties' {} a -> s {evaluatedModelArn = a} :: FlywheelIterationProperties)

-- | Undocumented member.
flywheelIterationProperties_evaluatedModelMetrics :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe FlywheelModelEvaluationMetrics)
flywheelIterationProperties_evaluatedModelMetrics = Lens.lens (\FlywheelIterationProperties' {evaluatedModelMetrics} -> evaluatedModelMetrics) (\s@FlywheelIterationProperties' {} a -> s {evaluatedModelMetrics = a} :: FlywheelIterationProperties)

flywheelIterationProperties_evaluationManifestS3Prefix :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.Text)
flywheelIterationProperties_evaluationManifestS3Prefix = Lens.lens (\FlywheelIterationProperties' {evaluationManifestS3Prefix} -> evaluationManifestS3Prefix) (\s@FlywheelIterationProperties' {} a -> s {evaluationManifestS3Prefix = a} :: FlywheelIterationProperties)

flywheelIterationProperties_flywheelArn :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.Text)
flywheelIterationProperties_flywheelArn = Lens.lens (\FlywheelIterationProperties' {flywheelArn} -> flywheelArn) (\s@FlywheelIterationProperties' {} a -> s {flywheelArn = a} :: FlywheelIterationProperties)

flywheelIterationProperties_flywheelIterationId :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.Text)
flywheelIterationProperties_flywheelIterationId = Lens.lens (\FlywheelIterationProperties' {flywheelIterationId} -> flywheelIterationId) (\s@FlywheelIterationProperties' {} a -> s {flywheelIterationId = a} :: FlywheelIterationProperties)

-- | A description of the status of the flywheel iteration.
flywheelIterationProperties_message :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.Text)
flywheelIterationProperties_message = Lens.lens (\FlywheelIterationProperties' {message} -> message) (\s@FlywheelIterationProperties' {} a -> s {message = a} :: FlywheelIterationProperties)

-- | The status of the flywheel iteration.
flywheelIterationProperties_status :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe FlywheelIterationStatus)
flywheelIterationProperties_status = Lens.lens (\FlywheelIterationProperties' {status} -> status) (\s@FlywheelIterationProperties' {} a -> s {status = a} :: FlywheelIterationProperties)

-- | The ARN of the trained model associated with this flywheel iteration.
flywheelIterationProperties_trainedModelArn :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe Prelude.Text)
flywheelIterationProperties_trainedModelArn = Lens.lens (\FlywheelIterationProperties' {trainedModelArn} -> trainedModelArn) (\s@FlywheelIterationProperties' {} a -> s {trainedModelArn = a} :: FlywheelIterationProperties)

-- | The metrics associated with the trained model.
flywheelIterationProperties_trainedModelMetrics :: Lens.Lens' FlywheelIterationProperties (Prelude.Maybe FlywheelModelEvaluationMetrics)
flywheelIterationProperties_trainedModelMetrics = Lens.lens (\FlywheelIterationProperties' {trainedModelMetrics} -> trainedModelMetrics) (\s@FlywheelIterationProperties' {} a -> s {trainedModelMetrics = a} :: FlywheelIterationProperties)

instance Data.FromJSON FlywheelIterationProperties where
  parseJSON =
    Data.withObject
      "FlywheelIterationProperties"
      ( \x ->
          FlywheelIterationProperties'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "EvaluatedModelArn")
            Prelude.<*> (x Data..:? "EvaluatedModelMetrics")
            Prelude.<*> (x Data..:? "EvaluationManifestS3Prefix")
            Prelude.<*> (x Data..:? "FlywheelArn")
            Prelude.<*> (x Data..:? "FlywheelIterationId")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TrainedModelArn")
            Prelude.<*> (x Data..:? "TrainedModelMetrics")
      )

instance Prelude.Hashable FlywheelIterationProperties where
  hashWithSalt _salt FlywheelIterationProperties' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` evaluatedModelArn
      `Prelude.hashWithSalt` evaluatedModelMetrics
      `Prelude.hashWithSalt` evaluationManifestS3Prefix
      `Prelude.hashWithSalt` flywheelArn
      `Prelude.hashWithSalt` flywheelIterationId
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` trainedModelArn
      `Prelude.hashWithSalt` trainedModelMetrics

instance Prelude.NFData FlywheelIterationProperties where
  rnf FlywheelIterationProperties' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf evaluatedModelArn
      `Prelude.seq` Prelude.rnf evaluatedModelMetrics
      `Prelude.seq` Prelude.rnf evaluationManifestS3Prefix
      `Prelude.seq` Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf flywheelIterationId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf trainedModelArn
      `Prelude.seq` Prelude.rnf trainedModelMetrics
