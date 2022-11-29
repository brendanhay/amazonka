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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.BatchDescribeModelPackageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.InferenceSpecification
import Amazonka.SageMaker.Types.ModelApprovalStatus
import Amazonka.SageMaker.Types.ModelPackageStatus

-- | Provides summary information about the model package.
--
-- /See:/ 'newBatchDescribeModelPackageSummary' smart constructor.
data BatchDescribeModelPackageSummary = BatchDescribeModelPackageSummary'
  { -- | The version number of a versioned model.
    modelPackageVersion :: Prelude.Maybe Prelude.Natural,
    -- | The approval status of the model.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | The description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | The group name for the model package
    modelPackageGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Text,
    -- | The creation time of the mortgage package summary.
    creationTime :: Core.POSIX,
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
-- 'modelPackageVersion', 'batchDescribeModelPackageSummary_modelPackageVersion' - The version number of a versioned model.
--
-- 'modelApprovalStatus', 'batchDescribeModelPackageSummary_modelApprovalStatus' - The approval status of the model.
--
-- 'modelPackageDescription', 'batchDescribeModelPackageSummary_modelPackageDescription' - The description of the model package.
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
      { modelPackageVersion =
          Prelude.Nothing,
        modelApprovalStatus = Prelude.Nothing,
        modelPackageDescription = Prelude.Nothing,
        modelPackageGroupName =
          pModelPackageGroupName_,
        modelPackageArn = pModelPackageArn_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        inferenceSpecification =
          pInferenceSpecification_,
        modelPackageStatus = pModelPackageStatus_
      }

-- | The version number of a versioned model.
batchDescribeModelPackageSummary_modelPackageVersion :: Lens.Lens' BatchDescribeModelPackageSummary (Prelude.Maybe Prelude.Natural)
batchDescribeModelPackageSummary_modelPackageVersion = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageVersion} -> modelPackageVersion) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageVersion = a} :: BatchDescribeModelPackageSummary)

-- | The approval status of the model.
batchDescribeModelPackageSummary_modelApprovalStatus :: Lens.Lens' BatchDescribeModelPackageSummary (Prelude.Maybe ModelApprovalStatus)
batchDescribeModelPackageSummary_modelApprovalStatus = Lens.lens (\BatchDescribeModelPackageSummary' {modelApprovalStatus} -> modelApprovalStatus) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelApprovalStatus = a} :: BatchDescribeModelPackageSummary)

-- | The description of the model package.
batchDescribeModelPackageSummary_modelPackageDescription :: Lens.Lens' BatchDescribeModelPackageSummary (Prelude.Maybe Prelude.Text)
batchDescribeModelPackageSummary_modelPackageDescription = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageDescription} -> modelPackageDescription) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageDescription = a} :: BatchDescribeModelPackageSummary)

-- | The group name for the model package
batchDescribeModelPackageSummary_modelPackageGroupName :: Lens.Lens' BatchDescribeModelPackageSummary Prelude.Text
batchDescribeModelPackageSummary_modelPackageGroupName = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageGroupName} -> modelPackageGroupName) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageGroupName = a} :: BatchDescribeModelPackageSummary)

-- | The Amazon Resource Name (ARN) of the model package.
batchDescribeModelPackageSummary_modelPackageArn :: Lens.Lens' BatchDescribeModelPackageSummary Prelude.Text
batchDescribeModelPackageSummary_modelPackageArn = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageArn} -> modelPackageArn) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageArn = a} :: BatchDescribeModelPackageSummary)

-- | The creation time of the mortgage package summary.
batchDescribeModelPackageSummary_creationTime :: Lens.Lens' BatchDescribeModelPackageSummary Prelude.UTCTime
batchDescribeModelPackageSummary_creationTime = Lens.lens (\BatchDescribeModelPackageSummary' {creationTime} -> creationTime) (\s@BatchDescribeModelPackageSummary' {} a -> s {creationTime = a} :: BatchDescribeModelPackageSummary) Prelude.. Core._Time

-- | Undocumented member.
batchDescribeModelPackageSummary_inferenceSpecification :: Lens.Lens' BatchDescribeModelPackageSummary InferenceSpecification
batchDescribeModelPackageSummary_inferenceSpecification = Lens.lens (\BatchDescribeModelPackageSummary' {inferenceSpecification} -> inferenceSpecification) (\s@BatchDescribeModelPackageSummary' {} a -> s {inferenceSpecification = a} :: BatchDescribeModelPackageSummary)

-- | The status of the mortgage package.
batchDescribeModelPackageSummary_modelPackageStatus :: Lens.Lens' BatchDescribeModelPackageSummary ModelPackageStatus
batchDescribeModelPackageSummary_modelPackageStatus = Lens.lens (\BatchDescribeModelPackageSummary' {modelPackageStatus} -> modelPackageStatus) (\s@BatchDescribeModelPackageSummary' {} a -> s {modelPackageStatus = a} :: BatchDescribeModelPackageSummary)

instance
  Core.FromJSON
    BatchDescribeModelPackageSummary
  where
  parseJSON =
    Core.withObject
      "BatchDescribeModelPackageSummary"
      ( \x ->
          BatchDescribeModelPackageSummary'
            Prelude.<$> (x Core..:? "ModelPackageVersion")
            Prelude.<*> (x Core..:? "ModelApprovalStatus")
            Prelude.<*> (x Core..:? "ModelPackageDescription")
            Prelude.<*> (x Core..: "ModelPackageGroupName")
            Prelude.<*> (x Core..: "ModelPackageArn")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "InferenceSpecification")
            Prelude.<*> (x Core..: "ModelPackageStatus")
      )

instance
  Prelude.Hashable
    BatchDescribeModelPackageSummary
  where
  hashWithSalt
    _salt
    BatchDescribeModelPackageSummary' {..} =
      _salt `Prelude.hashWithSalt` modelPackageVersion
        `Prelude.hashWithSalt` modelApprovalStatus
        `Prelude.hashWithSalt` modelPackageDescription
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
    Prelude.rnf modelPackageVersion
      `Prelude.seq` Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf modelPackageDescription
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf inferenceSpecification
      `Prelude.seq` Prelude.rnf modelPackageStatus
