{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceLimitPrice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceLimitPrice where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the limit price of a Reserved Instance offering.
--
--
--
-- /See:/ 'reservedInstanceLimitPrice' smart constructor.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice'
  { _rilpAmount ::
      !(Maybe Double),
    _rilpCurrencyCode ::
      !(Maybe CurrencyCodeValues)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstanceLimitPrice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rilpAmount' - Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
--
-- * 'rilpCurrencyCode' - The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
reservedInstanceLimitPrice ::
  ReservedInstanceLimitPrice
reservedInstanceLimitPrice =
  ReservedInstanceLimitPrice'
    { _rilpAmount = Nothing,
      _rilpCurrencyCode = Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
rilpAmount :: Lens' ReservedInstanceLimitPrice (Maybe Double)
rilpAmount = lens _rilpAmount (\s a -> s {_rilpAmount = a})

-- | The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe CurrencyCodeValues)
rilpCurrencyCode = lens _rilpCurrencyCode (\s a -> s {_rilpCurrencyCode = a})

instance Hashable ReservedInstanceLimitPrice

instance NFData ReservedInstanceLimitPrice

instance ToQuery ReservedInstanceLimitPrice where
  toQuery ReservedInstanceLimitPrice' {..} =
    mconcat
      ["Amount" =: _rilpAmount, "CurrencyCode" =: _rilpCurrencyCode]
