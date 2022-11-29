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
-- Module      : Amazonka.BillingConductor.Types.UpdateCustomLineItemPercentageChargeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.UpdateCustomLineItemPercentageChargeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A representation of the new charge details that are associated with a
-- percentage custom line item.
--
-- /See:/ 'newUpdateCustomLineItemPercentageChargeDetails' smart constructor.
data UpdateCustomLineItemPercentageChargeDetails = UpdateCustomLineItemPercentageChargeDetails'
  { -- | The custom line item\'s new percentage value. This will be multiplied
    -- against the combined value of its associated resources to determine its
    -- charge value.
    percentageValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomLineItemPercentageChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentageValue', 'updateCustomLineItemPercentageChargeDetails_percentageValue' - The custom line item\'s new percentage value. This will be multiplied
-- against the combined value of its associated resources to determine its
-- charge value.
newUpdateCustomLineItemPercentageChargeDetails ::
  -- | 'percentageValue'
  Prelude.Double ->
  UpdateCustomLineItemPercentageChargeDetails
newUpdateCustomLineItemPercentageChargeDetails
  pPercentageValue_ =
    UpdateCustomLineItemPercentageChargeDetails'
      { percentageValue =
          pPercentageValue_
      }

-- | The custom line item\'s new percentage value. This will be multiplied
-- against the combined value of its associated resources to determine its
-- charge value.
updateCustomLineItemPercentageChargeDetails_percentageValue :: Lens.Lens' UpdateCustomLineItemPercentageChargeDetails Prelude.Double
updateCustomLineItemPercentageChargeDetails_percentageValue = Lens.lens (\UpdateCustomLineItemPercentageChargeDetails' {percentageValue} -> percentageValue) (\s@UpdateCustomLineItemPercentageChargeDetails' {} a -> s {percentageValue = a} :: UpdateCustomLineItemPercentageChargeDetails)

instance
  Prelude.Hashable
    UpdateCustomLineItemPercentageChargeDetails
  where
  hashWithSalt
    _salt
    UpdateCustomLineItemPercentageChargeDetails' {..} =
      _salt `Prelude.hashWithSalt` percentageValue

instance
  Prelude.NFData
    UpdateCustomLineItemPercentageChargeDetails
  where
  rnf UpdateCustomLineItemPercentageChargeDetails' {..} =
    Prelude.rnf percentageValue

instance
  Core.ToJSON
    UpdateCustomLineItemPercentageChargeDetails
  where
  toJSON
    UpdateCustomLineItemPercentageChargeDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("PercentageValue" Core..= percentageValue)
            ]
        )
