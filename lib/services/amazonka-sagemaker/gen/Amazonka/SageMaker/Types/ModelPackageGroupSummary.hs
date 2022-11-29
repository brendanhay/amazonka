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
-- Module      : Amazonka.SageMaker.Types.ModelPackageGroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelPackageGroupStatus

-- | Summary information about a model group.
--
-- /See:/ 'newModelPackageGroupSummary' smart constructor.
data ModelPackageGroupSummary = ModelPackageGroupSummary'
  { -- | A description of the model group.
    modelPackageGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the model group.
    modelPackageGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model group.
    modelPackageGroupArn :: Prelude.Text,
    -- | The time that the model group was created.
    creationTime :: Core.POSIX,
    -- | The status of the model group.
    modelPackageGroupStatus :: ModelPackageGroupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'modelPackageGroupArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
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
          Prelude.Nothing,
        modelPackageGroupName = pModelPackageGroupName_,
        modelPackageGroupArn = pModelPackageGroupArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        modelPackageGroupStatus =
          pModelPackageGroupStatus_
      }

-- | A description of the model group.
modelPackageGroupSummary_modelPackageGroupDescription :: Lens.Lens' ModelPackageGroupSummary (Prelude.Maybe Prelude.Text)
modelPackageGroupSummary_modelPackageGroupDescription = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupDescription} -> modelPackageGroupDescription) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupDescription = a} :: ModelPackageGroupSummary)

-- | The name of the model group.
modelPackageGroupSummary_modelPackageGroupName :: Lens.Lens' ModelPackageGroupSummary Prelude.Text
modelPackageGroupSummary_modelPackageGroupName = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupName = a} :: ModelPackageGroupSummary)

-- | The Amazon Resource Name (ARN) of the model group.
modelPackageGroupSummary_modelPackageGroupArn :: Lens.Lens' ModelPackageGroupSummary Prelude.Text
modelPackageGroupSummary_modelPackageGroupArn = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupArn = a} :: ModelPackageGroupSummary)

-- | The time that the model group was created.
modelPackageGroupSummary_creationTime :: Lens.Lens' ModelPackageGroupSummary Prelude.UTCTime
modelPackageGroupSummary_creationTime = Lens.lens (\ModelPackageGroupSummary' {creationTime} -> creationTime) (\s@ModelPackageGroupSummary' {} a -> s {creationTime = a} :: ModelPackageGroupSummary) Prelude.. Core._Time

-- | The status of the model group.
modelPackageGroupSummary_modelPackageGroupStatus :: Lens.Lens' ModelPackageGroupSummary ModelPackageGroupStatus
modelPackageGroupSummary_modelPackageGroupStatus = Lens.lens (\ModelPackageGroupSummary' {modelPackageGroupStatus} -> modelPackageGroupStatus) (\s@ModelPackageGroupSummary' {} a -> s {modelPackageGroupStatus = a} :: ModelPackageGroupSummary)

instance Core.FromJSON ModelPackageGroupSummary where
  parseJSON =
    Core.withObject
      "ModelPackageGroupSummary"
      ( \x ->
          ModelPackageGroupSummary'
            Prelude.<$> (x Core..:? "ModelPackageGroupDescription")
            Prelude.<*> (x Core..: "ModelPackageGroupName")
            Prelude.<*> (x Core..: "ModelPackageGroupArn")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "ModelPackageGroupStatus")
      )

instance Prelude.Hashable ModelPackageGroupSummary where
  hashWithSalt _salt ModelPackageGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` modelPackageGroupDescription
      `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` modelPackageGroupArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` modelPackageGroupStatus

instance Prelude.NFData ModelPackageGroupSummary where
  rnf ModelPackageGroupSummary' {..} =
    Prelude.rnf modelPackageGroupDescription
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageGroupArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf modelPackageGroupStatus
