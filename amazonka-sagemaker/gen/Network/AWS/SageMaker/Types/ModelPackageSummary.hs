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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ModelApprovalStatus
import Network.AWS.SageMaker.Types.ModelPackageStatus

-- | Provides summary information about a model package.
--
-- /See:/ 'newModelPackageSummary' smart constructor.
data ModelPackageSummary = ModelPackageSummary'
  { -- | If the model package is a versioned model, the version of the model.
    modelPackageVersion :: Core.Maybe Core.Natural,
    -- | The approval status of the model. This can be one of the following
    -- values.
    --
    -- -   @APPROVED@ - The model is approved
    --
    -- -   @REJECTED@ - The model is rejected.
    --
    -- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
    --     approval.
    modelApprovalStatus :: Core.Maybe ModelApprovalStatus,
    -- | A brief description of the model package.
    modelPackageDescription :: Core.Maybe Core.Text,
    -- | If the model package is a versioned model, the model group that the
    -- versioned model belongs to.
    modelPackageGroupName :: Core.Maybe Core.Text,
    -- | The name of the model package.
    modelPackageName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Core.Text,
    -- | A timestamp that shows when the model package was created.
    creationTime :: Core.POSIX,
    -- | The overall status of the model package.
    modelPackageStatus :: ModelPackageStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelPackageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageVersion', 'modelPackageSummary_modelPackageVersion' - If the model package is a versioned model, the version of the model.
--
-- 'modelApprovalStatus', 'modelPackageSummary_modelApprovalStatus' - The approval status of the model. This can be one of the following
-- values.
--
-- -   @APPROVED@ - The model is approved
--
-- -   @REJECTED@ - The model is rejected.
--
-- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
--     approval.
--
-- 'modelPackageDescription', 'modelPackageSummary_modelPackageDescription' - A brief description of the model package.
--
-- 'modelPackageGroupName', 'modelPackageSummary_modelPackageGroupName' - If the model package is a versioned model, the model group that the
-- versioned model belongs to.
--
-- 'modelPackageName', 'modelPackageSummary_modelPackageName' - The name of the model package.
--
-- 'modelPackageArn', 'modelPackageSummary_modelPackageArn' - The Amazon Resource Name (ARN) of the model package.
--
-- 'creationTime', 'modelPackageSummary_creationTime' - A timestamp that shows when the model package was created.
--
-- 'modelPackageStatus', 'modelPackageSummary_modelPackageStatus' - The overall status of the model package.
newModelPackageSummary ::
  -- | 'modelPackageName'
  Core.Text ->
  -- | 'modelPackageArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'modelPackageStatus'
  ModelPackageStatus ->
  ModelPackageSummary
newModelPackageSummary
  pModelPackageName_
  pModelPackageArn_
  pCreationTime_
  pModelPackageStatus_ =
    ModelPackageSummary'
      { modelPackageVersion =
          Core.Nothing,
        modelApprovalStatus = Core.Nothing,
        modelPackageDescription = Core.Nothing,
        modelPackageGroupName = Core.Nothing,
        modelPackageName = pModelPackageName_,
        modelPackageArn = pModelPackageArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        modelPackageStatus = pModelPackageStatus_
      }

-- | If the model package is a versioned model, the version of the model.
modelPackageSummary_modelPackageVersion :: Lens.Lens' ModelPackageSummary (Core.Maybe Core.Natural)
modelPackageSummary_modelPackageVersion = Lens.lens (\ModelPackageSummary' {modelPackageVersion} -> modelPackageVersion) (\s@ModelPackageSummary' {} a -> s {modelPackageVersion = a} :: ModelPackageSummary)

-- | The approval status of the model. This can be one of the following
-- values.
--
-- -   @APPROVED@ - The model is approved
--
-- -   @REJECTED@ - The model is rejected.
--
-- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
--     approval.
modelPackageSummary_modelApprovalStatus :: Lens.Lens' ModelPackageSummary (Core.Maybe ModelApprovalStatus)
modelPackageSummary_modelApprovalStatus = Lens.lens (\ModelPackageSummary' {modelApprovalStatus} -> modelApprovalStatus) (\s@ModelPackageSummary' {} a -> s {modelApprovalStatus = a} :: ModelPackageSummary)

-- | A brief description of the model package.
modelPackageSummary_modelPackageDescription :: Lens.Lens' ModelPackageSummary (Core.Maybe Core.Text)
modelPackageSummary_modelPackageDescription = Lens.lens (\ModelPackageSummary' {modelPackageDescription} -> modelPackageDescription) (\s@ModelPackageSummary' {} a -> s {modelPackageDescription = a} :: ModelPackageSummary)

-- | If the model package is a versioned model, the model group that the
-- versioned model belongs to.
modelPackageSummary_modelPackageGroupName :: Lens.Lens' ModelPackageSummary (Core.Maybe Core.Text)
modelPackageSummary_modelPackageGroupName = Lens.lens (\ModelPackageSummary' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackageSummary' {} a -> s {modelPackageGroupName = a} :: ModelPackageSummary)

-- | The name of the model package.
modelPackageSummary_modelPackageName :: Lens.Lens' ModelPackageSummary Core.Text
modelPackageSummary_modelPackageName = Lens.lens (\ModelPackageSummary' {modelPackageName} -> modelPackageName) (\s@ModelPackageSummary' {} a -> s {modelPackageName = a} :: ModelPackageSummary)

-- | The Amazon Resource Name (ARN) of the model package.
modelPackageSummary_modelPackageArn :: Lens.Lens' ModelPackageSummary Core.Text
modelPackageSummary_modelPackageArn = Lens.lens (\ModelPackageSummary' {modelPackageArn} -> modelPackageArn) (\s@ModelPackageSummary' {} a -> s {modelPackageArn = a} :: ModelPackageSummary)

-- | A timestamp that shows when the model package was created.
modelPackageSummary_creationTime :: Lens.Lens' ModelPackageSummary Core.UTCTime
modelPackageSummary_creationTime = Lens.lens (\ModelPackageSummary' {creationTime} -> creationTime) (\s@ModelPackageSummary' {} a -> s {creationTime = a} :: ModelPackageSummary) Core.. Core._Time

-- | The overall status of the model package.
modelPackageSummary_modelPackageStatus :: Lens.Lens' ModelPackageSummary ModelPackageStatus
modelPackageSummary_modelPackageStatus = Lens.lens (\ModelPackageSummary' {modelPackageStatus} -> modelPackageStatus) (\s@ModelPackageSummary' {} a -> s {modelPackageStatus = a} :: ModelPackageSummary)

instance Core.FromJSON ModelPackageSummary where
  parseJSON =
    Core.withObject
      "ModelPackageSummary"
      ( \x ->
          ModelPackageSummary'
            Core.<$> (x Core..:? "ModelPackageVersion")
            Core.<*> (x Core..:? "ModelApprovalStatus")
            Core.<*> (x Core..:? "ModelPackageDescription")
            Core.<*> (x Core..:? "ModelPackageGroupName")
            Core.<*> (x Core..: "ModelPackageName")
            Core.<*> (x Core..: "ModelPackageArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "ModelPackageStatus")
      )

instance Core.Hashable ModelPackageSummary

instance Core.NFData ModelPackageSummary
