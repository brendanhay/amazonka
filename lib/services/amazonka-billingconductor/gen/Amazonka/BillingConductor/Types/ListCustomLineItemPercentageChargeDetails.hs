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
-- Module      : Amazonka.BillingConductor.Types.ListCustomLineItemPercentageChargeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListCustomLineItemPercentageChargeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the charge details that are associated with a
-- percentage custom line item.
--
-- /See:/ 'newListCustomLineItemPercentageChargeDetails' smart constructor.
data ListCustomLineItemPercentageChargeDetails = ListCustomLineItemPercentageChargeDetails'
  { -- | The custom line item\'s percentage value. This will be multiplied
    -- against the combined value of its associated resources to determine its
    -- charge value.
    percentageValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemPercentageChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentageValue', 'listCustomLineItemPercentageChargeDetails_percentageValue' - The custom line item\'s percentage value. This will be multiplied
-- against the combined value of its associated resources to determine its
-- charge value.
newListCustomLineItemPercentageChargeDetails ::
  -- | 'percentageValue'
  Prelude.Double ->
  ListCustomLineItemPercentageChargeDetails
newListCustomLineItemPercentageChargeDetails
  pPercentageValue_ =
    ListCustomLineItemPercentageChargeDetails'
      { percentageValue =
          pPercentageValue_
      }

-- | The custom line item\'s percentage value. This will be multiplied
-- against the combined value of its associated resources to determine its
-- charge value.
listCustomLineItemPercentageChargeDetails_percentageValue :: Lens.Lens' ListCustomLineItemPercentageChargeDetails Prelude.Double
listCustomLineItemPercentageChargeDetails_percentageValue = Lens.lens (\ListCustomLineItemPercentageChargeDetails' {percentageValue} -> percentageValue) (\s@ListCustomLineItemPercentageChargeDetails' {} a -> s {percentageValue = a} :: ListCustomLineItemPercentageChargeDetails)

instance
  Data.FromJSON
    ListCustomLineItemPercentageChargeDetails
  where
  parseJSON =
    Data.withObject
      "ListCustomLineItemPercentageChargeDetails"
      ( \x ->
          ListCustomLineItemPercentageChargeDetails'
            Prelude.<$> (x Data..: "PercentageValue")
      )

instance
  Prelude.Hashable
    ListCustomLineItemPercentageChargeDetails
  where
  hashWithSalt
    _salt
    ListCustomLineItemPercentageChargeDetails' {..} =
      _salt `Prelude.hashWithSalt` percentageValue

instance
  Prelude.NFData
    ListCustomLineItemPercentageChargeDetails
  where
  rnf ListCustomLineItemPercentageChargeDetails' {..} =
    Prelude.rnf percentageValue
