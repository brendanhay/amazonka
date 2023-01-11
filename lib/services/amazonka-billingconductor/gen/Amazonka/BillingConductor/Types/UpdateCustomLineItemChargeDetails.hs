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
-- Module      : Amazonka.BillingConductor.Types.UpdateCustomLineItemChargeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.UpdateCustomLineItemChargeDetails where

import Amazonka.BillingConductor.Types.UpdateCustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.UpdateCustomLineItemPercentageChargeDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the new charge details of a custom line item. This
-- should contain only one of @Flat@ or @Percentage@.
--
-- /See:/ 'newUpdateCustomLineItemChargeDetails' smart constructor.
data UpdateCustomLineItemChargeDetails = UpdateCustomLineItemChargeDetails'
  { -- | An @UpdateCustomLineItemFlatChargeDetails@ that describes the new charge
    -- details of a flat custom line item.
    flat :: Prelude.Maybe UpdateCustomLineItemFlatChargeDetails,
    -- | An @UpdateCustomLineItemPercentageChargeDetails@ that describes the new
    -- charge details of a percentage custom line item.
    percentage :: Prelude.Maybe UpdateCustomLineItemPercentageChargeDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomLineItemChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flat', 'updateCustomLineItemChargeDetails_flat' - An @UpdateCustomLineItemFlatChargeDetails@ that describes the new charge
-- details of a flat custom line item.
--
-- 'percentage', 'updateCustomLineItemChargeDetails_percentage' - An @UpdateCustomLineItemPercentageChargeDetails@ that describes the new
-- charge details of a percentage custom line item.
newUpdateCustomLineItemChargeDetails ::
  UpdateCustomLineItemChargeDetails
newUpdateCustomLineItemChargeDetails =
  UpdateCustomLineItemChargeDetails'
    { flat =
        Prelude.Nothing,
      percentage = Prelude.Nothing
    }

-- | An @UpdateCustomLineItemFlatChargeDetails@ that describes the new charge
-- details of a flat custom line item.
updateCustomLineItemChargeDetails_flat :: Lens.Lens' UpdateCustomLineItemChargeDetails (Prelude.Maybe UpdateCustomLineItemFlatChargeDetails)
updateCustomLineItemChargeDetails_flat = Lens.lens (\UpdateCustomLineItemChargeDetails' {flat} -> flat) (\s@UpdateCustomLineItemChargeDetails' {} a -> s {flat = a} :: UpdateCustomLineItemChargeDetails)

-- | An @UpdateCustomLineItemPercentageChargeDetails@ that describes the new
-- charge details of a percentage custom line item.
updateCustomLineItemChargeDetails_percentage :: Lens.Lens' UpdateCustomLineItemChargeDetails (Prelude.Maybe UpdateCustomLineItemPercentageChargeDetails)
updateCustomLineItemChargeDetails_percentage = Lens.lens (\UpdateCustomLineItemChargeDetails' {percentage} -> percentage) (\s@UpdateCustomLineItemChargeDetails' {} a -> s {percentage = a} :: UpdateCustomLineItemChargeDetails)

instance
  Prelude.Hashable
    UpdateCustomLineItemChargeDetails
  where
  hashWithSalt
    _salt
    UpdateCustomLineItemChargeDetails' {..} =
      _salt `Prelude.hashWithSalt` flat
        `Prelude.hashWithSalt` percentage

instance
  Prelude.NFData
    UpdateCustomLineItemChargeDetails
  where
  rnf UpdateCustomLineItemChargeDetails' {..} =
    Prelude.rnf flat
      `Prelude.seq` Prelude.rnf percentage

instance
  Data.ToJSON
    UpdateCustomLineItemChargeDetails
  where
  toJSON UpdateCustomLineItemChargeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Flat" Data..=) Prelude.<$> flat,
            ("Percentage" Data..=) Prelude.<$> percentage
          ]
      )
