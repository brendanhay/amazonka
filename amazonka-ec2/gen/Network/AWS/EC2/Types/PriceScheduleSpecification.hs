{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.PriceScheduleSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PriceScheduleSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'newPriceScheduleSpecification' smart constructor.
data PriceScheduleSpecification = PriceScheduleSpecification'
  { -- | The currency for transacting the Reserved Instance resale. At this time,
    -- the only supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The number of months remaining in the reservation. For example, 2 is the
    -- second to the last month before the capacity reservation expires.
    term :: Prelude.Maybe Prelude.Integer,
    -- | The fixed price for the term.
    price :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PriceScheduleSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'priceScheduleSpecification_currencyCode' - The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is @USD@.
--
-- 'term', 'priceScheduleSpecification_term' - The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
--
-- 'price', 'priceScheduleSpecification_price' - The fixed price for the term.
newPriceScheduleSpecification ::
  PriceScheduleSpecification
newPriceScheduleSpecification =
  PriceScheduleSpecification'
    { currencyCode =
        Prelude.Nothing,
      term = Prelude.Nothing,
      price = Prelude.Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is @USD@.
priceScheduleSpecification_currencyCode :: Lens.Lens' PriceScheduleSpecification (Prelude.Maybe CurrencyCodeValues)
priceScheduleSpecification_currencyCode = Lens.lens (\PriceScheduleSpecification' {currencyCode} -> currencyCode) (\s@PriceScheduleSpecification' {} a -> s {currencyCode = a} :: PriceScheduleSpecification)

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
priceScheduleSpecification_term :: Lens.Lens' PriceScheduleSpecification (Prelude.Maybe Prelude.Integer)
priceScheduleSpecification_term = Lens.lens (\PriceScheduleSpecification' {term} -> term) (\s@PriceScheduleSpecification' {} a -> s {term = a} :: PriceScheduleSpecification)

-- | The fixed price for the term.
priceScheduleSpecification_price :: Lens.Lens' PriceScheduleSpecification (Prelude.Maybe Prelude.Double)
priceScheduleSpecification_price = Lens.lens (\PriceScheduleSpecification' {price} -> price) (\s@PriceScheduleSpecification' {} a -> s {price = a} :: PriceScheduleSpecification)

instance Prelude.Hashable PriceScheduleSpecification

instance Prelude.NFData PriceScheduleSpecification

instance Prelude.ToQuery PriceScheduleSpecification where
  toQuery PriceScheduleSpecification' {..} =
    Prelude.mconcat
      [ "CurrencyCode" Prelude.=: currencyCode,
        "Term" Prelude.=: term,
        "Price" Prelude.=: price
      ]
