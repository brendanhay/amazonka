{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PriceScheduleSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PriceScheduleSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the price for a Reserved Instance.
--
--
--
-- /See:/ 'priceScheduleSpecification' smart constructor.
data PriceScheduleSpecification = PriceScheduleSpecification'
  { _pssCurrencyCode ::
      !(Maybe CurrencyCodeValues),
    _pssTerm :: !(Maybe Integer),
    _pssPrice :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PriceScheduleSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pssCurrencyCode' - The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- * 'pssTerm' - The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- * 'pssPrice' - The fixed price for the term.
priceScheduleSpecification ::
  PriceScheduleSpecification
priceScheduleSpecification =
  PriceScheduleSpecification'
    { _pssCurrencyCode = Nothing,
      _pssTerm = Nothing,
      _pssPrice = Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe CurrencyCodeValues)
pssCurrencyCode = lens _pssCurrencyCode (\s a -> s {_pssCurrencyCode = a})

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm = lens _pssTerm (\s a -> s {_pssTerm = a})

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice = lens _pssPrice (\s a -> s {_pssPrice = a})

instance Hashable PriceScheduleSpecification

instance NFData PriceScheduleSpecification

instance ToQuery PriceScheduleSpecification where
  toQuery PriceScheduleSpecification' {..} =
    mconcat
      [ "CurrencyCode" =: _pssCurrencyCode,
        "Term" =: _pssTerm,
        "Price" =: _pssPrice
      ]
