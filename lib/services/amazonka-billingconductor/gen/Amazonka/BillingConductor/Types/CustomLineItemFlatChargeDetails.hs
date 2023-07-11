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
-- Module      : Amazonka.BillingConductor.Types.CustomLineItemFlatChargeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CustomLineItemFlatChargeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the charge details that are associated with a flat
-- custom line item.
--
-- /See:/ 'newCustomLineItemFlatChargeDetails' smart constructor.
data CustomLineItemFlatChargeDetails = CustomLineItemFlatChargeDetails'
  { -- | The custom line item\'s fixed charge value in USD.
    chargeValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLineItemFlatChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chargeValue', 'customLineItemFlatChargeDetails_chargeValue' - The custom line item\'s fixed charge value in USD.
newCustomLineItemFlatChargeDetails ::
  -- | 'chargeValue'
  Prelude.Double ->
  CustomLineItemFlatChargeDetails
newCustomLineItemFlatChargeDetails pChargeValue_ =
  CustomLineItemFlatChargeDetails'
    { chargeValue =
        pChargeValue_
    }

-- | The custom line item\'s fixed charge value in USD.
customLineItemFlatChargeDetails_chargeValue :: Lens.Lens' CustomLineItemFlatChargeDetails Prelude.Double
customLineItemFlatChargeDetails_chargeValue = Lens.lens (\CustomLineItemFlatChargeDetails' {chargeValue} -> chargeValue) (\s@CustomLineItemFlatChargeDetails' {} a -> s {chargeValue = a} :: CustomLineItemFlatChargeDetails)

instance
  Prelude.Hashable
    CustomLineItemFlatChargeDetails
  where
  hashWithSalt
    _salt
    CustomLineItemFlatChargeDetails' {..} =
      _salt `Prelude.hashWithSalt` chargeValue

instance
  Prelude.NFData
    CustomLineItemFlatChargeDetails
  where
  rnf CustomLineItemFlatChargeDetails' {..} =
    Prelude.rnf chargeValue

instance Data.ToJSON CustomLineItemFlatChargeDetails where
  toJSON CustomLineItemFlatChargeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ChargeValue" Data..= chargeValue)]
      )
