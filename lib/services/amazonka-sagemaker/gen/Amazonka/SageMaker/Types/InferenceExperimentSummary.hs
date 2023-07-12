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
-- Module      : Amazonka.SageMaker.Types.InferenceExperimentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceExperimentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.InferenceExperimentSchedule
import Amazonka.SageMaker.Types.InferenceExperimentStatus
import Amazonka.SageMaker.Types.InferenceExperimentType

-- | Lists a summary of properties of an inference experiment.
--
-- /See:/ 'newInferenceExperimentSummary' smart constructor.
data InferenceExperimentSummary = InferenceExperimentSummary'
  { -- | The timestamp at which the inference experiment was completed.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the inference experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that Amazon SageMaker can assume to access model
    -- artifacts and container images, and manage Amazon SageMaker Inference
    -- endpoints for model deployment.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The duration for which the inference experiment ran or will run.
    --
    -- The maximum duration that you can set for an inference experiment is 30
    -- days.
    schedule :: Prelude.Maybe InferenceExperimentSchedule,
    -- | The error message for the inference experiment status result.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the inference experiment.
    name :: Prelude.Text,
    -- | The type of the inference experiment.
    type' :: InferenceExperimentType,
    -- | The status of the inference experiment.
    status :: InferenceExperimentStatus,
    -- | The timestamp at which the inference experiment was created.
    creationTime :: Data.POSIX,
    -- | The timestamp when you last modified the inference experiment.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceExperimentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'inferenceExperimentSummary_completionTime' - The timestamp at which the inference experiment was completed.
--
-- 'description', 'inferenceExperimentSummary_description' - The description of the inference experiment.
--
-- 'roleArn', 'inferenceExperimentSummary_roleArn' - The ARN of the IAM role that Amazon SageMaker can assume to access model
-- artifacts and container images, and manage Amazon SageMaker Inference
-- endpoints for model deployment.
--
-- 'schedule', 'inferenceExperimentSummary_schedule' - The duration for which the inference experiment ran or will run.
--
-- The maximum duration that you can set for an inference experiment is 30
-- days.
--
-- 'statusReason', 'inferenceExperimentSummary_statusReason' - The error message for the inference experiment status result.
--
-- 'name', 'inferenceExperimentSummary_name' - The name of the inference experiment.
--
-- 'type'', 'inferenceExperimentSummary_type' - The type of the inference experiment.
--
-- 'status', 'inferenceExperimentSummary_status' - The status of the inference experiment.
--
-- 'creationTime', 'inferenceExperimentSummary_creationTime' - The timestamp at which the inference experiment was created.
--
-- 'lastModifiedTime', 'inferenceExperimentSummary_lastModifiedTime' - The timestamp when you last modified the inference experiment.
newInferenceExperimentSummary ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  InferenceExperimentType ->
  -- | 'status'
  InferenceExperimentStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  InferenceExperimentSummary
newInferenceExperimentSummary
  pName_
  pType_
  pStatus_
  pCreationTime_
  pLastModifiedTime_ =
    InferenceExperimentSummary'
      { completionTime =
          Prelude.Nothing,
        description = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        schedule = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        name = pName_,
        type' = pType_,
        status = pStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The timestamp at which the inference experiment was completed.
inferenceExperimentSummary_completionTime :: Lens.Lens' InferenceExperimentSummary (Prelude.Maybe Prelude.UTCTime)
inferenceExperimentSummary_completionTime = Lens.lens (\InferenceExperimentSummary' {completionTime} -> completionTime) (\s@InferenceExperimentSummary' {} a -> s {completionTime = a} :: InferenceExperimentSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the inference experiment.
inferenceExperimentSummary_description :: Lens.Lens' InferenceExperimentSummary (Prelude.Maybe Prelude.Text)
inferenceExperimentSummary_description = Lens.lens (\InferenceExperimentSummary' {description} -> description) (\s@InferenceExperimentSummary' {} a -> s {description = a} :: InferenceExperimentSummary)

-- | The ARN of the IAM role that Amazon SageMaker can assume to access model
-- artifacts and container images, and manage Amazon SageMaker Inference
-- endpoints for model deployment.
inferenceExperimentSummary_roleArn :: Lens.Lens' InferenceExperimentSummary (Prelude.Maybe Prelude.Text)
inferenceExperimentSummary_roleArn = Lens.lens (\InferenceExperimentSummary' {roleArn} -> roleArn) (\s@InferenceExperimentSummary' {} a -> s {roleArn = a} :: InferenceExperimentSummary)

-- | The duration for which the inference experiment ran or will run.
--
-- The maximum duration that you can set for an inference experiment is 30
-- days.
inferenceExperimentSummary_schedule :: Lens.Lens' InferenceExperimentSummary (Prelude.Maybe InferenceExperimentSchedule)
inferenceExperimentSummary_schedule = Lens.lens (\InferenceExperimentSummary' {schedule} -> schedule) (\s@InferenceExperimentSummary' {} a -> s {schedule = a} :: InferenceExperimentSummary)

-- | The error message for the inference experiment status result.
inferenceExperimentSummary_statusReason :: Lens.Lens' InferenceExperimentSummary (Prelude.Maybe Prelude.Text)
inferenceExperimentSummary_statusReason = Lens.lens (\InferenceExperimentSummary' {statusReason} -> statusReason) (\s@InferenceExperimentSummary' {} a -> s {statusReason = a} :: InferenceExperimentSummary)

-- | The name of the inference experiment.
inferenceExperimentSummary_name :: Lens.Lens' InferenceExperimentSummary Prelude.Text
inferenceExperimentSummary_name = Lens.lens (\InferenceExperimentSummary' {name} -> name) (\s@InferenceExperimentSummary' {} a -> s {name = a} :: InferenceExperimentSummary)

-- | The type of the inference experiment.
inferenceExperimentSummary_type :: Lens.Lens' InferenceExperimentSummary InferenceExperimentType
inferenceExperimentSummary_type = Lens.lens (\InferenceExperimentSummary' {type'} -> type') (\s@InferenceExperimentSummary' {} a -> s {type' = a} :: InferenceExperimentSummary)

-- | The status of the inference experiment.
inferenceExperimentSummary_status :: Lens.Lens' InferenceExperimentSummary InferenceExperimentStatus
inferenceExperimentSummary_status = Lens.lens (\InferenceExperimentSummary' {status} -> status) (\s@InferenceExperimentSummary' {} a -> s {status = a} :: InferenceExperimentSummary)

-- | The timestamp at which the inference experiment was created.
inferenceExperimentSummary_creationTime :: Lens.Lens' InferenceExperimentSummary Prelude.UTCTime
inferenceExperimentSummary_creationTime = Lens.lens (\InferenceExperimentSummary' {creationTime} -> creationTime) (\s@InferenceExperimentSummary' {} a -> s {creationTime = a} :: InferenceExperimentSummary) Prelude.. Data._Time

-- | The timestamp when you last modified the inference experiment.
inferenceExperimentSummary_lastModifiedTime :: Lens.Lens' InferenceExperimentSummary Prelude.UTCTime
inferenceExperimentSummary_lastModifiedTime = Lens.lens (\InferenceExperimentSummary' {lastModifiedTime} -> lastModifiedTime) (\s@InferenceExperimentSummary' {} a -> s {lastModifiedTime = a} :: InferenceExperimentSummary) Prelude.. Data._Time

instance Data.FromJSON InferenceExperimentSummary where
  parseJSON =
    Data.withObject
      "InferenceExperimentSummary"
      ( \x ->
          InferenceExperimentSummary'
            Prelude.<$> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Schedule")
            Prelude.<*> (x Data..:? "StatusReason")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable InferenceExperimentSummary where
  hashWithSalt _salt InferenceExperimentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData InferenceExperimentSummary where
  rnf InferenceExperimentSummary' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
