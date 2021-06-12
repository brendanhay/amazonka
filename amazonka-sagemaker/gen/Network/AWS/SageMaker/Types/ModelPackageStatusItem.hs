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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatusItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatusItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus

-- | Represents the overall status of a model package.
--
-- /See:/ 'newModelPackageStatusItem' smart constructor.
data ModelPackageStatusItem = ModelPackageStatusItem'
  { -- | if the overall status is @Failed@, the reason for the failure.
    failureReason :: Core.Maybe Core.Text,
    -- | The name of the model package for which the overall status is being
    -- reported.
    name :: Core.Text,
    -- | The current status.
    status :: DetailedModelPackageStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelPackageStatusItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'modelPackageStatusItem_failureReason' - if the overall status is @Failed@, the reason for the failure.
--
-- 'name', 'modelPackageStatusItem_name' - The name of the model package for which the overall status is being
-- reported.
--
-- 'status', 'modelPackageStatusItem_status' - The current status.
newModelPackageStatusItem ::
  -- | 'name'
  Core.Text ->
  -- | 'status'
  DetailedModelPackageStatus ->
  ModelPackageStatusItem
newModelPackageStatusItem pName_ pStatus_ =
  ModelPackageStatusItem'
    { failureReason =
        Core.Nothing,
      name = pName_,
      status = pStatus_
    }

-- | if the overall status is @Failed@, the reason for the failure.
modelPackageStatusItem_failureReason :: Lens.Lens' ModelPackageStatusItem (Core.Maybe Core.Text)
modelPackageStatusItem_failureReason = Lens.lens (\ModelPackageStatusItem' {failureReason} -> failureReason) (\s@ModelPackageStatusItem' {} a -> s {failureReason = a} :: ModelPackageStatusItem)

-- | The name of the model package for which the overall status is being
-- reported.
modelPackageStatusItem_name :: Lens.Lens' ModelPackageStatusItem Core.Text
modelPackageStatusItem_name = Lens.lens (\ModelPackageStatusItem' {name} -> name) (\s@ModelPackageStatusItem' {} a -> s {name = a} :: ModelPackageStatusItem)

-- | The current status.
modelPackageStatusItem_status :: Lens.Lens' ModelPackageStatusItem DetailedModelPackageStatus
modelPackageStatusItem_status = Lens.lens (\ModelPackageStatusItem' {status} -> status) (\s@ModelPackageStatusItem' {} a -> s {status = a} :: ModelPackageStatusItem)

instance Core.FromJSON ModelPackageStatusItem where
  parseJSON =
    Core.withObject
      "ModelPackageStatusItem"
      ( \x ->
          ModelPackageStatusItem'
            Core.<$> (x Core..:? "FailureReason")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Status")
      )

instance Core.Hashable ModelPackageStatusItem

instance Core.NFData ModelPackageStatusItem
