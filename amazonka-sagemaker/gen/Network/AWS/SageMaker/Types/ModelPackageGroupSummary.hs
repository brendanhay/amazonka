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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageGroupSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ModelPackageGroupStatus

-- | Summary information about a model group.
--
-- /See:/ 'newModelPackageGroupSummary' smart constructor.
data ModelPackageGroupSummary = ModelPackageGroupSummary'
  { -- | A description of the model group.
    modelPackageGroupDescription :: Core.Maybe Core.Text,
    -- | The name of the model group.
    modelPackageGroupName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the model group.
    modelPackageGroupArn :: Core.Text,
    -- | The time that the model group was created.
    creationTime :: Core.POSIX,
    -- | The status of the model group.
    modelPackageGroupStatus :: ModelPackageGroupStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelPackageGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupDescription', 'modelPackageGroupSummary_modelPackageGroupDescription' - A description of the model group.
--
-- 'modelPackageGroupName', 'modelPackageGroupSummary_modelPackageGroupName' - The name of the model group.
--
-- 'modelPackageGroupArn', 'modelPackageGroupSummary_modelPackageGroupArn' - The Amazon Resource Name (ARN) of the model group.
--
-- 'creationTime', 'modelPackageGroupSummary_creationTime' - The time that the model group was created.
--
-- 'modelPackageGroupStatus', 'modelPackageGroupSummary_modelPackageGroupStatus' - The status of the model group.
newModelPackageGroupSummary ::
  -- | 'modelPackageGroupName'
  Core.Text ->
  -- | 'modelPackageGroupArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'modelPackageGroupStatus'
  ModelPackageGroupStatus ->
  ModelPackageGroupSummary
newModelPackageGroupSummary
  pModelPackageGroupName_
  pModelPackageGroupArn_
  pCreationTime_
  pModelPackageGroupStatus_ =
    ModelPackageGroupSummary'
      { modelPackageGroupDescription =
          Core.Nothing,
        modelPackageGroupName = pModelPackageGroupName_,
        modelPackageGroupArn = pModelPackageGroupArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        modelPackageGroupStatus =
          pModelPackageGroupStatus_
      }

-- | A description of the model group.
modelPackageGroupSummary_modelPackageGroupDescription :: Lens.Lens' ModelPackageGroupSummary (Core.Maybe Core.Text)
modelPackageGroupSummary_modelPackageGroupDescription = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupDescription} -> modelPackageGroupDescription) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupDescription = a} :: ModelPackageGroupSummary)

-- | The name of the model group.
modelPackageGroupSummary_modelPackageGroupName :: Lens.Lens' ModelPackageGroupSummary Core.Text
modelPackageGroupSummary_modelPackageGroupName = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupName = a} :: ModelPackageGroupSummary)

-- | The Amazon Resource Name (ARN) of the model group.
modelPackageGroupSummary_modelPackageGroupArn :: Lens.Lens' ModelPackageGroupSummary Core.Text
modelPackageGroupSummary_modelPackageGroupArn = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupArn = a} :: ModelPackageGroupSummary)

-- | The time that the model group was created.
modelPackageGroupSummary_creationTime :: Lens.Lens' ModelPackageGroupSummary Core.UTCTime
modelPackageGroupSummary_creationTime = Lens.lens (\ModelPackageGroupSummary' {creationTime} -> creationTime) (\s@ModelPackageGroupSummary' {} a -> s {creationTime = a} :: ModelPackageGroupSummary) Core.. Core._Time

-- | The status of the model group.
modelPackageGroupSummary_modelPackageGroupStatus :: Lens.Lens' ModelPackageGroupSummary ModelPackageGroupStatus
modelPackageGroupSummary_modelPackageGroupStatus = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupStatus} -> modelPackageGroupStatus) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupStatus = a} :: ModelPackageGroupSummary)

instance Core.FromJSON ModelPackageGroupSummary where
  parseJSON =
    Core.withObject
      "ModelPackageGroupSummary"
      ( \x ->
          ModelPackageGroupSummary'
            Core.<$> (x Core..:? "ModelPackageGroupDescription")
            Core.<*> (x Core..: "ModelPackageGroupName")
            Core.<*> (x Core..: "ModelPackageGroupArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "ModelPackageGroupStatus")
      )

instance Core.Hashable ModelPackageGroupSummary

instance Core.NFData ModelPackageGroupSummary
