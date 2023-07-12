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
-- Module      : Amazonka.BillingConductor.Types.ListCustomLineItemChargeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListCustomLineItemChargeDetails where

import Amazonka.BillingConductor.Types.CustomLineItemType
import Amazonka.BillingConductor.Types.ListCustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.ListCustomLineItemPercentageChargeDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the charge details of a custom line item.
--
-- /See:/ 'newListCustomLineItemChargeDetails' smart constructor.
data ListCustomLineItemChargeDetails = ListCustomLineItemChargeDetails'
  { -- | A @ListCustomLineItemFlatChargeDetails@ that describes the charge
    -- details of a flat custom line item.
    flat :: Prelude.Maybe ListCustomLineItemFlatChargeDetails,
    -- | A @ListCustomLineItemPercentageChargeDetails@ that describes the charge
    -- details of a percentage custom line item.
    percentage :: Prelude.Maybe ListCustomLineItemPercentageChargeDetails,
    -- | The type of the custom line item that indicates whether the charge is a
    -- @fee@ or @credit@.
    type' :: CustomLineItemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flat', 'listCustomLineItemChargeDetails_flat' - A @ListCustomLineItemFlatChargeDetails@ that describes the charge
-- details of a flat custom line item.
--
-- 'percentage', 'listCustomLineItemChargeDetails_percentage' - A @ListCustomLineItemPercentageChargeDetails@ that describes the charge
-- details of a percentage custom line item.
--
-- 'type'', 'listCustomLineItemChargeDetails_type' - The type of the custom line item that indicates whether the charge is a
-- @fee@ or @credit@.
newListCustomLineItemChargeDetails ::
  -- | 'type''
  CustomLineItemType ->
  ListCustomLineItemChargeDetails
newListCustomLineItemChargeDetails pType_ =
  ListCustomLineItemChargeDetails'
    { flat =
        Prelude.Nothing,
      percentage = Prelude.Nothing,
      type' = pType_
    }

-- | A @ListCustomLineItemFlatChargeDetails@ that describes the charge
-- details of a flat custom line item.
listCustomLineItemChargeDetails_flat :: Lens.Lens' ListCustomLineItemChargeDetails (Prelude.Maybe ListCustomLineItemFlatChargeDetails)
listCustomLineItemChargeDetails_flat = Lens.lens (\ListCustomLineItemChargeDetails' {flat} -> flat) (\s@ListCustomLineItemChargeDetails' {} a -> s {flat = a} :: ListCustomLineItemChargeDetails)

-- | A @ListCustomLineItemPercentageChargeDetails@ that describes the charge
-- details of a percentage custom line item.
listCustomLineItemChargeDetails_percentage :: Lens.Lens' ListCustomLineItemChargeDetails (Prelude.Maybe ListCustomLineItemPercentageChargeDetails)
listCustomLineItemChargeDetails_percentage = Lens.lens (\ListCustomLineItemChargeDetails' {percentage} -> percentage) (\s@ListCustomLineItemChargeDetails' {} a -> s {percentage = a} :: ListCustomLineItemChargeDetails)

-- | The type of the custom line item that indicates whether the charge is a
-- @fee@ or @credit@.
listCustomLineItemChargeDetails_type :: Lens.Lens' ListCustomLineItemChargeDetails CustomLineItemType
listCustomLineItemChargeDetails_type = Lens.lens (\ListCustomLineItemChargeDetails' {type'} -> type') (\s@ListCustomLineItemChargeDetails' {} a -> s {type' = a} :: ListCustomLineItemChargeDetails)

instance
  Data.FromJSON
    ListCustomLineItemChargeDetails
  where
  parseJSON =
    Data.withObject
      "ListCustomLineItemChargeDetails"
      ( \x ->
          ListCustomLineItemChargeDetails'
            Prelude.<$> (x Data..:? "Flat")
            Prelude.<*> (x Data..:? "Percentage")
            Prelude.<*> (x Data..: "Type")
      )

instance
  Prelude.Hashable
    ListCustomLineItemChargeDetails
  where
  hashWithSalt
    _salt
    ListCustomLineItemChargeDetails' {..} =
      _salt
        `Prelude.hashWithSalt` flat
        `Prelude.hashWithSalt` percentage
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    ListCustomLineItemChargeDetails
  where
  rnf ListCustomLineItemChargeDetails' {..} =
    Prelude.rnf flat
      `Prelude.seq` Prelude.rnf percentage
      `Prelude.seq` Prelude.rnf type'
