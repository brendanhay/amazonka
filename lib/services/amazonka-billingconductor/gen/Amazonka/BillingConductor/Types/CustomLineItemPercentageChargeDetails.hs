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
-- Module      : Amazonka.BillingConductor.Types.CustomLineItemPercentageChargeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CustomLineItemPercentageChargeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of the charge details that are associated with a
-- percentage custom line item.
--
-- /See:/ 'newCustomLineItemPercentageChargeDetails' smart constructor.
data CustomLineItemPercentageChargeDetails = CustomLineItemPercentageChargeDetails'
  { -- | A list of resource ARNs to associate to the percentage custom line item.
    associatedValues :: Prelude.Maybe [Prelude.Text],
    -- | The custom line item\'s percentage value. This will be multiplied
    -- against the combined value of its associated resources to determine its
    -- charge value.
    percentageValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLineItemPercentageChargeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedValues', 'customLineItemPercentageChargeDetails_associatedValues' - A list of resource ARNs to associate to the percentage custom line item.
--
-- 'percentageValue', 'customLineItemPercentageChargeDetails_percentageValue' - The custom line item\'s percentage value. This will be multiplied
-- against the combined value of its associated resources to determine its
-- charge value.
newCustomLineItemPercentageChargeDetails ::
  -- | 'percentageValue'
  Prelude.Double ->
  CustomLineItemPercentageChargeDetails
newCustomLineItemPercentageChargeDetails
  pPercentageValue_ =
    CustomLineItemPercentageChargeDetails'
      { associatedValues =
          Prelude.Nothing,
        percentageValue = pPercentageValue_
      }

-- | A list of resource ARNs to associate to the percentage custom line item.
customLineItemPercentageChargeDetails_associatedValues :: Lens.Lens' CustomLineItemPercentageChargeDetails (Prelude.Maybe [Prelude.Text])
customLineItemPercentageChargeDetails_associatedValues = Lens.lens (\CustomLineItemPercentageChargeDetails' {associatedValues} -> associatedValues) (\s@CustomLineItemPercentageChargeDetails' {} a -> s {associatedValues = a} :: CustomLineItemPercentageChargeDetails) Prelude.. Lens.mapping Lens.coerced

-- | The custom line item\'s percentage value. This will be multiplied
-- against the combined value of its associated resources to determine its
-- charge value.
customLineItemPercentageChargeDetails_percentageValue :: Lens.Lens' CustomLineItemPercentageChargeDetails Prelude.Double
customLineItemPercentageChargeDetails_percentageValue = Lens.lens (\CustomLineItemPercentageChargeDetails' {percentageValue} -> percentageValue) (\s@CustomLineItemPercentageChargeDetails' {} a -> s {percentageValue = a} :: CustomLineItemPercentageChargeDetails)

instance
  Prelude.Hashable
    CustomLineItemPercentageChargeDetails
  where
  hashWithSalt
    _salt
    CustomLineItemPercentageChargeDetails' {..} =
      _salt `Prelude.hashWithSalt` associatedValues
        `Prelude.hashWithSalt` percentageValue

instance
  Prelude.NFData
    CustomLineItemPercentageChargeDetails
  where
  rnf CustomLineItemPercentageChargeDetails' {..} =
    Prelude.rnf associatedValues
      `Prelude.seq` Prelude.rnf percentageValue

instance
  Data.ToJSON
    CustomLineItemPercentageChargeDetails
  where
  toJSON CustomLineItemPercentageChargeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociatedValues" Data..=)
              Prelude.<$> associatedValues,
            Prelude.Just
              ("PercentageValue" Data..= percentageValue)
          ]
      )
