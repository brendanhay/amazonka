{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PriceSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PriceSchedule where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the price for a Reserved Instance.
--
--
--
-- /See:/ 'priceSchedule' smart constructor.
data PriceSchedule = PriceSchedule'
  { _psCurrencyCode ::
      !(Maybe CurrencyCodeValues),
    _psTerm :: !(Maybe Integer),
    _psActive :: !(Maybe Bool),
    _psPrice :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PriceSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCurrencyCode' - The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- * 'psTerm' - The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- * 'psActive' - The current price schedule, as determined by the term remaining for the Reserved Instance in the listing. A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
--
-- * 'psPrice' - The fixed price for the term.
priceSchedule ::
  PriceSchedule
priceSchedule =
  PriceSchedule'
    { _psCurrencyCode = Nothing,
      _psTerm = Nothing,
      _psActive = Nothing,
      _psPrice = Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
psCurrencyCode :: Lens' PriceSchedule (Maybe CurrencyCodeValues)
psCurrencyCode = lens _psCurrencyCode (\s a -> s {_psCurrencyCode = a})

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm = lens _psTerm (\s a -> s {_psTerm = a})

-- | The current price schedule, as determined by the term remaining for the Reserved Instance in the listing. A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
psActive :: Lens' PriceSchedule (Maybe Bool)
psActive = lens _psActive (\s a -> s {_psActive = a})

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice = lens _psPrice (\s a -> s {_psPrice = a})

instance FromXML PriceSchedule where
  parseXML x =
    PriceSchedule'
      <$> (x .@? "currencyCode")
      <*> (x .@? "term")
      <*> (x .@? "active")
      <*> (x .@? "price")

instance Hashable PriceSchedule

instance NFData PriceSchedule
