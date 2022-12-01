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
-- Module      : Amazonka.BillingConductor.Types.CustomLineItemChargeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CustomLineItemChargeDetails where

import Amazonka.BillingConductor.Types.CustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The charge details of a custom line item. It should contain only one of
-- @Flat@ or @Percentage@.
--
-- /See:/ 'newCustomLineItemChargeDetails' smart constructor.
data CustomLineItemChargeDetails = CustomLineItemChargeDetails'
  { -- | A @CustomLineItemFlatChargeDetails@ that describes the charge details of
    -- a flat custom line item.
    flat :: Prelude.Maybe CustomLineItemFlatChargeDetails,
    -- | A @CustomLineItemPercentageChargeDetails@ that describes the charge
    -- details of a percentage custom line item.
    percentage :: Prelude.Maybe CustomLineItemPercentageChargeDetails,
    -- | The type of the custom line item that indicates whether the charge is a
    -- fee or credit.
    type' :: CustomLineItemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLineItemChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flat', 'customLineItemChargeDetails_flat' - A @CustomLineItemFlatChargeDetails@ that describes the charge details of
-- a flat custom line item.
--
-- 'percentage', 'customLineItemChargeDetails_percentage' - A @CustomLineItemPercentageChargeDetails@ that describes the charge
-- details of a percentage custom line item.
--
-- 'type'', 'customLineItemChargeDetails_type' - The type of the custom line item that indicates whether the charge is a
-- fee or credit.
newCustomLineItemChargeDetails ::
  -- | 'type''
  CustomLineItemType ->
  CustomLineItemChargeDetails
newCustomLineItemChargeDetails pType_ =
  CustomLineItemChargeDetails'
    { flat =
        Prelude.Nothing,
      percentage = Prelude.Nothing,
      type' = pType_
    }

-- | A @CustomLineItemFlatChargeDetails@ that describes the charge details of
-- a flat custom line item.
customLineItemChargeDetails_flat :: Lens.Lens' CustomLineItemChargeDetails (Prelude.Maybe CustomLineItemFlatChargeDetails)
customLineItemChargeDetails_flat = Lens.lens (\CustomLineItemChargeDetails' {flat} -> flat) (\s@CustomLineItemChargeDetails' {} a -> s {flat = a} :: CustomLineItemChargeDetails)

-- | A @CustomLineItemPercentageChargeDetails@ that describes the charge
-- details of a percentage custom line item.
customLineItemChargeDetails_percentage :: Lens.Lens' CustomLineItemChargeDetails (Prelude.Maybe CustomLineItemPercentageChargeDetails)
customLineItemChargeDetails_percentage = Lens.lens (\CustomLineItemChargeDetails' {percentage} -> percentage) (\s@CustomLineItemChargeDetails' {} a -> s {percentage = a} :: CustomLineItemChargeDetails)

-- | The type of the custom line item that indicates whether the charge is a
-- fee or credit.
customLineItemChargeDetails_type :: Lens.Lens' CustomLineItemChargeDetails CustomLineItemType
customLineItemChargeDetails_type = Lens.lens (\CustomLineItemChargeDetails' {type'} -> type') (\s@CustomLineItemChargeDetails' {} a -> s {type' = a} :: CustomLineItemChargeDetails)

instance Prelude.Hashable CustomLineItemChargeDetails where
  hashWithSalt _salt CustomLineItemChargeDetails' {..} =
    _salt `Prelude.hashWithSalt` flat
      `Prelude.hashWithSalt` percentage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CustomLineItemChargeDetails where
  rnf CustomLineItemChargeDetails' {..} =
    Prelude.rnf flat
      `Prelude.seq` Prelude.rnf percentage
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON CustomLineItemChargeDetails where
  toJSON CustomLineItemChargeDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Flat" Core..=) Prelude.<$> flat,
            ("Percentage" Core..=) Prelude.<$> percentage,
            Prelude.Just ("Type" Core..= type')
          ]
      )
