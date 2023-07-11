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
-- Module      : Amazonka.SageMaker.Types.BatchDescribeModelPackageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.BatchDescribeModelPackageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.InferenceSpecification
import Amazonka.SageMaker.Types.ModelApprovalStatus
import Amazonka.SageMaker.Types.ModelPackageStatus

-- | Provides summary information about the model package.
--
-- /See:/ 'newBatchDescribeModelPackageSummary' smart constructor.
data BatchDescribeModelPackageSummary = BatchDescribeModelPackageSummary'
  { -- | The approval status of the model.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | The description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | The version number of a versioned model.
    modelPackageVersion :: Prelude.Maybe Prelude.Natural,
    -- | The group name for the model package
    modelPackageGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Text,
    -- | The creation time of the mortgage package summary.
    creationTime :: Data.POSIX,
    inferenceSpecification :: InferenceSpecification,
    -- | The status of the mortgage package.
    modelPackageStatus :: ModelPackageStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeModelPackageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelApprovalStatus', 'batchDescribeModelPackageSummary_modelApprovalStatus' - The approval status of the model.
--
-- 'modelPackageDescription', 'batchDescribeModelPackageSummary_modelPackageDescription' - The description of the model package.
--
-- 'modelPackageVersion', 'batchDescribeModelPackageSummary_modelPackageVersion' - The version number of a versioned model.
--
-- 'modelPackageGroupName', 'batchDescribeModelPackageSummary_modelPackageGroupName' - The group name for the model package
--
-- 'modelPackageArn', 'batchDescribeModelPackageSummary_modelPackageArn' - The Amazon Resource Name (ARN) of the model package.
--
-- 'creationTime', 'batchDescribeModelPackageSummary_creationTime' - The creation time of the mortgage package summary.
--
-- 'inferenceSpecification', 'batchDescribeModelPackageSummary_inferenceSpecification' - Undocumented member.
--
-- 'modelPackageStatus', 'batchDescribeModelPackageSummary_modelPackageStatus' - The status of the mortgage package.
newBatchDescribeModelPackageSummary ::
  -- | 'modelPackageGroupName'
  Prelude.Text ->
  -- | 'modelPackageArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'inferenceSpecification'
  InferenceSpecification ->
  -- | 'modelPackageStatus'
  ModelPackageStatus ->
  BatchDescribeModelPackageSummary
newBatchDescribeModelPackageSummary
  pModelPackageGroupName_
  pModelPackageArn_
  pCreationTime_
  pInferenceSpecification_
  pModelPackageStatus_ =
    BatchDescribeModelPackageSummary'
      { modelApprovalStatus =
          Prelude.Nothing,
        modelPackageDescription = Prelude.Nothing,
        modelPackageVersion = Prelude.Nothing,
        modelPackageGroupName =
          pModelPackageGroupName_,
        modelPackageArn = pModelPackageArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        inferenceSpecification =
          pInferenceSpecification_,
        modelPackageStatus = pModelPackageStatus_
      }

-- | The approval status of the model.
batchDescribeModelPackageSummary_modelApprovalStatus :: Lens.Lens' BatchDescribeModelPackageSummary (Prelude.Maybe ModelApprovalStatus)
batchDescribeModelPackageSummary_modelApprovalStatus = Lens.lens (\BatchDescribeModelPackageSummary' {modelApprovalStatus} -> modelApprovalStatus) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelApprovalStatus = a} :: BatchDescribeModelPackageSummary)

-- | The description of the model package.
batchDescribeModelPackageSummary_modelPackageDescription :: Lens.Lens' BatchDescribeModelPackageSummary (Prelude.Maybe Prelude.Text)
batchDescribeModelPackageSummary_modelPackageDescription = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageDescription} -> modelPackageDescription) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageDescription = a} :: BatchDescribeModelPackageSummary)

-- | The version number of a versioned model.
batchDescribeModelPackageSummary_modelPackageVersion :: Lens.Lens' BatchDescribeModelPackageSummary (Prelude.Maybe Prelude.Natural)
batchDescribeModelPackageSummary_modelPackageVersion = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageVersion} -> modelPackageVersion) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageVersion = a} :: BatchDescribeModelPackageSummary)

-- | The group name for the model package
batchDescribeModelPackageSummary_modelPackageGroupName :: Lens.Lens' BatchDescribeModelPackageSummary Prelude.Text
batchDescribeModelPackageSummary_modelPackageGroupName = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageGroupName} -> modelPackageGroupName) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageGroupName = a} :: BatchDescribeModelPackageSummary)

-- | The Amazon Resource Name (ARN) of the model package.
batchDescribeModelPackageSummary_modelPackageArn :: Lens.Lens' BatchDescribeModelPackageSummary Prelude.Text
batchDescribeModelPackageSummary_modelPackageArn = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageArn} -> modelPackageArn) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageArn = a} :: BatchDescribeModelPackageSummary)

-- | The creation time of the mortgage package summary.
batchDescribeModelPackageSummary_creationTime :: Lens.Lens' BatchDescribeModelPackageSummary Prelude.UTCTime
batchDescribeModelPackageSummary_creationTime = Lens.lens (\BatchDescribeModelPackageSummary' {creationTime} -> creationTime) (\s@BatchDescribeModelPackageSummary' {} a -> s {creationTime = a} :: BatchDescribeModelPackageSummary) Prelude.. Data._Time

-- | Undocumented member.
batchDescribeModelPackageSummary_inferenceSpecification :: Lens.Lens' BatchDescribeModelPackageSummary InferenceSpecification
batchDescribeModelPackageSummary_inferenceSpecification = Lens.lens (\BatchDescribeModelPackageSummary' {inferenceSpecification} -> inferenceSpecification) (\s@BatchDescribeModelPackageSummary' {} a -> s {inferenceSpecification = a} :: BatchDescribeModelPackageSummary)

-- | The status of the mortgage package.
batchDescribeModelPackageSummary_modelPackageStatus :: Lens.Lens' BatchDescribeModelPackageSummary ModelPackageStatus
batchDescribeModelPackageSummary_modelPackageStatus = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageStatus} -> modelPackageStatus) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageStatus = a} :: BatchDescribeModelPackageSummary)

instance
  Data.FromJSON
    BatchDescribeModelPackageSummary
  where
  parseJSON =
    Data.withObject
      "BatchDescribeModelPackageSummary"
      ( \x ->
          BatchDescribeModelPackageSummary'
            Prelude.<$> (x Data..:? "ModelApprovalStatus")
            Prelude.<*> (x Data..:? "ModelPackageDescription")
            Prelude.<*> (x Data..:? "ModelPackageVersion")
            Prelude.<*> (x Data..: "ModelPackageGroupName")
            Prelude.<*> (x Data..: "ModelPackageArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "InferenceSpecification")
            Prelude.<*> (x Data..: "ModelPackageStatus")
      )

instance
  Prelude.Hashable
    BatchDescribeModelPackageSummary
  where
  hashWithSalt
    _salt
    BatchDescribeModelPackageSummary' {..} =
      _salt
        `Prelude.hashWithSalt` modelApprovalStatus
        `Prelude.hashWithSalt` modelPackageDescription
        `Prelude.hashWithSalt` modelPackageVersion
        `Prelude.hashWithSalt` modelPackageGroupName
        `Prelude.hashWithSalt` modelPackageArn
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` inferenceSpecification
        `Prelude.hashWithSalt` modelPackageStatus

instance
  Prelude.NFData
    BatchDescribeModelPackageSummary
  where
  rnf BatchDescribeModelPackageSummary' {..} =
    Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf modelPackageDescription
      `Prelude.seq` Prelude.rnf modelPackageVersion
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf inferenceSpecification
      `Prelude.seq` Prelude.rnf modelPackageStatus
