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
-- Module      : Network.AWS.EC2.Types.PriceSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PriceSchedule where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the price for a Reserved Instance.
--
-- /See:/ 'newPriceSchedule' smart constructor.
data PriceSchedule = PriceSchedule'
  { -- | The currency for transacting the Reserved Instance resale. At this time,
    -- the only supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The current price schedule, as determined by the term remaining for the
    -- Reserved Instance in the listing.
    --
    -- A specific price schedule is always in effect, but only one price
    -- schedule can be active at any time. Take, for example, a Reserved
    -- Instance listing that has five months remaining in its term. When you
    -- specify price schedules for five months and two months, this means that
    -- schedule 1, covering the first three months of the remaining term, will
    -- be active during months 5, 4, and 3. Then schedule 2, covering the last
    -- two months of the term, will be active for months 2 and 1.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The number of months remaining in the reservation. For example, 2 is the
    -- second to the last month before the capacity reservation expires.
    term :: Prelude.Maybe Prelude.Integer,
    -- | The fixed price for the term.
    price :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PriceSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'priceSchedule_currencyCode' - The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is @USD@.
--
-- 'active', 'priceSchedule_active' - The current price schedule, as determined by the term remaining for the
-- Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price
-- schedule can be active at any time. Take, for example, a Reserved
-- Instance listing that has five months remaining in its term. When you
-- specify price schedules for five months and two months, this means that
-- schedule 1, covering the first three months of the remaining term, will
-- be active during months 5, 4, and 3. Then schedule 2, covering the last
-- two months of the term, will be active for months 2 and 1.
--
-- 'term', 'priceSchedule_term' - The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
--
-- 'price', 'priceSchedule_price' - The fixed price for the term.
newPriceSchedule ::
  PriceSchedule
newPriceSchedule =
  PriceSchedule'
    { currencyCode = Prelude.Nothing,
      active = Prelude.Nothing,
      term = Prelude.Nothing,
      price = Prelude.Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time,
-- the only supported currency is @USD@.
priceSchedule_currencyCode :: Lens.Lens' PriceSchedule (Prelude.Maybe CurrencyCodeValues)
priceSchedule_currencyCode = Lens.lens (\PriceSchedule' {currencyCode} -> currencyCode) (\s@PriceSchedule' {} a -> s {currencyCode = a} :: PriceSchedule)

-- | The current price schedule, as determined by the term remaining for the
-- Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price
-- schedule can be active at any time. Take, for example, a Reserved
-- Instance listing that has five months remaining in its term. When you
-- specify price schedules for five months and two months, this means that
-- schedule 1, covering the first three months of the remaining term, will
-- be active during months 5, 4, and 3. Then schedule 2, covering the last
-- two months of the term, will be active for months 2 and 1.
priceSchedule_active :: Lens.Lens' PriceSchedule (Prelude.Maybe Prelude.Bool)
priceSchedule_active = Lens.lens (\PriceSchedule' {active} -> active) (\s@PriceSchedule' {} a -> s {active = a} :: PriceSchedule)

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
priceSchedule_term :: Lens.Lens' PriceSchedule (Prelude.Maybe Prelude.Integer)
priceSchedule_term = Lens.lens (\PriceSchedule' {term} -> term) (\s@PriceSchedule' {} a -> s {term = a} :: PriceSchedule)

-- | The fixed price for the term.
priceSchedule_price :: Lens.Lens' PriceSchedule (Prelude.Maybe Prelude.Double)
priceSchedule_price = Lens.lens (\PriceSchedule' {price} -> price) (\s@PriceSchedule' {} a -> s {price = a} :: PriceSchedule)

instance Prelude.FromXML PriceSchedule where
  parseXML x =
    PriceSchedule'
      Prelude.<$> (x Prelude..@? "currencyCode")
      Prelude.<*> (x Prelude..@? "active")
      Prelude.<*> (x Prelude..@? "term")
      Prelude.<*> (x Prelude..@? "price")

instance Prelude.Hashable PriceSchedule

instance Prelude.NFData PriceSchedule
