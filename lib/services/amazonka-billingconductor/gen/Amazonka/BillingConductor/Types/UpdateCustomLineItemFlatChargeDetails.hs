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
-- Module      : Amazonka.BillingConductor.Types.UpdateCustomLineItemFlatChargeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.UpdateCustomLineItemFlatChargeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the new charge details that are associated with a
-- flat custom line item.
--
-- /See:/ 'newUpdateCustomLineItemFlatChargeDetails' smart constructor.
data UpdateCustomLineItemFlatChargeDetails = UpdateCustomLineItemFlatChargeDetails'
  { -- | The custom line item\'s new fixed charge value in USD.
    chargeValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomLineItemFlatChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chargeValue', 'updateCustomLineItemFlatChargeDetails_chargeValue' - The custom line item\'s new fixed charge value in USD.
newUpdateCustomLineItemFlatChargeDetails ::
  -- | 'chargeValue'
  Prelude.Double ->
  UpdateCustomLineItemFlatChargeDetails
newUpdateCustomLineItemFlatChargeDetails
  pChargeValue_ =
    UpdateCustomLineItemFlatChargeDetails'
      { chargeValue =
          pChargeValue_
      }

-- | The custom line item\'s new fixed charge value in USD.
updateCustomLineItemFlatChargeDetails_chargeValue :: Lens.Lens' UpdateCustomLineItemFlatChargeDetails Prelude.Double
updateCustomLineItemFlatChargeDetails_chargeValue = Lens.lens (\UpdateCustomLineItemFlatChargeDetails' {chargeValue} -> chargeValue) (\s@UpdateCustomLineItemFlatChargeDetails' {} a -> s {chargeValue = a} :: UpdateCustomLineItemFlatChargeDetails)

instance
  Prelude.Hashable
    UpdateCustomLineItemFlatChargeDetails
  where
  hashWithSalt
    _salt
    UpdateCustomLineItemFlatChargeDetails' {..} =
      _salt `Prelude.hashWithSalt` chargeValue

instance
  Prelude.NFData
    UpdateCustomLineItemFlatChargeDetails
  where
  rnf UpdateCustomLineItemFlatChargeDetails' {..} =
    Prelude.rnf chargeValue

instance
  Data.ToJSON
    UpdateCustomLineItemFlatChargeDetails
  where
  toJSON UpdateCustomLineItemFlatChargeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ChargeValue" Data..= chargeValue)]
      )
