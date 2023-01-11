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
-- Module      : Amazonka.BillingConductor.Types.ListCustomLineItemFlatChargeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListCustomLineItemFlatChargeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the charge details that are associated with a flat
-- custom line item.
--
-- /See:/ 'newListCustomLineItemFlatChargeDetails' smart constructor.
data ListCustomLineItemFlatChargeDetails = ListCustomLineItemFlatChargeDetails'
  { -- | The custom line item\'s fixed charge value in USD.
    chargeValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemFlatChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chargeValue', 'listCustomLineItemFlatChargeDetails_chargeValue' - The custom line item\'s fixed charge value in USD.
newListCustomLineItemFlatChargeDetails ::
  -- | 'chargeValue'
  Prelude.Double ->
  ListCustomLineItemFlatChargeDetails
newListCustomLineItemFlatChargeDetails pChargeValue_ =
  ListCustomLineItemFlatChargeDetails'
    { chargeValue =
        pChargeValue_
    }

-- | The custom line item\'s fixed charge value in USD.
listCustomLineItemFlatChargeDetails_chargeValue :: Lens.Lens' ListCustomLineItemFlatChargeDetails Prelude.Double
listCustomLineItemFlatChargeDetails_chargeValue = Lens.lens (\ListCustomLineItemFlatChargeDetails' {chargeValue} -> chargeValue) (\s@ListCustomLineItemFlatChargeDetails' {} a -> s {chargeValue = a} :: ListCustomLineItemFlatChargeDetails)

instance
  Data.FromJSON
    ListCustomLineItemFlatChargeDetails
  where
  parseJSON =
    Data.withObject
      "ListCustomLineItemFlatChargeDetails"
      ( \x ->
          ListCustomLineItemFlatChargeDetails'
            Prelude.<$> (x Data..: "ChargeValue")
      )

instance
  Prelude.Hashable
    ListCustomLineItemFlatChargeDetails
  where
  hashWithSalt
    _salt
    ListCustomLineItemFlatChargeDetails' {..} =
      _salt `Prelude.hashWithSalt` chargeValue

instance
  Prelude.NFData
    ListCustomLineItemFlatChargeDetails
  where
  rnf ListCustomLineItemFlatChargeDetails' {..} =
    Prelude.rnf chargeValue
