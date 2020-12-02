{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PricingDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PricingDetail where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Reserved Instance offering.
--
--
--
-- /See:/ 'pricingDetail' smart constructor.
data PricingDetail = PricingDetail'
  { _pdCount :: !(Maybe Int),
    _pdPrice :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PricingDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdCount' - The number of reservations available for the price.
--
-- * 'pdPrice' - The price per instance.
pricingDetail ::
  PricingDetail
pricingDetail =
  PricingDetail' {_pdCount = Nothing, _pdPrice = Nothing}

-- | The number of reservations available for the price.
pdCount :: Lens' PricingDetail (Maybe Int)
pdCount = lens _pdCount (\s a -> s {_pdCount = a})

-- | The price per instance.
pdPrice :: Lens' PricingDetail (Maybe Double)
pdPrice = lens _pdPrice (\s a -> s {_pdPrice = a})

instance FromXML PricingDetail where
  parseXML x = PricingDetail' <$> (x .@? "count") <*> (x .@? "price")

instance Hashable PricingDetail

instance NFData PricingDetail
