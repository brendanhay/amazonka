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
-- Module      : Amazonka.SageMaker.Types.ModelPackageStatusItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageStatusItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DetailedModelPackageStatus

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON ModelPackageStatusItem where
  parseJSON =
    Data.withObject
      "ModelPackageStatusItem"
      ( \x ->
          ModelPackageStatusItem'
            Prelude.<$> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable ModelPackageStatusItem where
  hashWithSalt _salt ModelPackageStatusItem' {..} =
    _salt
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData ModelPackageStatusItem where
  rnf ModelPackageStatusItem' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
