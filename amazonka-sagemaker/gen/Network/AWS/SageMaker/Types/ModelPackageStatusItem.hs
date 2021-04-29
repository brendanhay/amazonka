{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus

-- | Represents the overall status of a model package.
--
-- /See:/ 'newModelPackageStatusItem' smart constructor.
data ModelPackageStatusItem = ModelPackageStatusItem'
  { -- | if the overall status is @Failed@, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the model package for which the overall status is being
    -- reported.
    name :: Prelude.Text,
    -- | The current status.
    status :: DetailedModelPackageStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'status'
  DetailedModelPackageStatus ->
  ModelPackageStatusItem
newModelPackageStatusItem pName_ pStatus_ =
  ModelPackageStatusItem'
    { failureReason =
        Prelude.Nothing,
      name = pName_,
      status = pStatus_
    }

-- | if the overall status is @Failed@, the reason for the failure.
modelPackageStatusItem_failureReason :: Lens.Lens' ModelPackageStatusItem (Prelude.Maybe Prelude.Text)
modelPackageStatusItem_failureReason = Lens.lens (\ModelPackageStatusItem' {failureReason} -> failureReason) (\s@ModelPackageStatusItem' {} a -> s {failureReason = a} :: ModelPackageStatusItem)

-- | The name of the model package for which the overall status is being
-- reported.
modelPackageStatusItem_name :: Lens.Lens' ModelPackageStatusItem Prelude.Text
modelPackageStatusItem_name = Lens.lens (\ModelPackageStatusItem' {name} -> name) (\s@ModelPackageStatusItem' {} a -> s {name = a} :: ModelPackageStatusItem)

-- | The current status.
modelPackageStatusItem_status :: Lens.Lens' ModelPackageStatusItem DetailedModelPackageStatus
modelPackageStatusItem_status = Lens.lens (\ModelPackageStatusItem' {status} -> status) (\s@ModelPackageStatusItem' {} a -> s {status = a} :: ModelPackageStatusItem)

instance Prelude.FromJSON ModelPackageStatusItem where
  parseJSON =
    Prelude.withObject
      "ModelPackageStatusItem"
      ( \x ->
          ModelPackageStatusItem'
            Prelude.<$> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable ModelPackageStatusItem

instance Prelude.NFData ModelPackageStatusItem
