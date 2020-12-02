{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMarketOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.MarketType
import Network.AWS.EC2.Types.SpotMarketOptions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the market (purchasing) option for the instances.
--
--
--
-- /See:/ 'instanceMarketOptionsRequest' smart constructor.
data InstanceMarketOptionsRequest = InstanceMarketOptionsRequest'
  { _imorMarketType ::
      !(Maybe MarketType),
    _imorSpotOptions ::
      !(Maybe SpotMarketOptions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceMarketOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imorMarketType' - The market type.
--
-- * 'imorSpotOptions' - The options for Spot Instances.
instanceMarketOptionsRequest ::
  InstanceMarketOptionsRequest
instanceMarketOptionsRequest =
  InstanceMarketOptionsRequest'
    { _imorMarketType = Nothing,
      _imorSpotOptions = Nothing
    }

-- | The market type.
imorMarketType :: Lens' InstanceMarketOptionsRequest (Maybe MarketType)
imorMarketType = lens _imorMarketType (\s a -> s {_imorMarketType = a})

-- | The options for Spot Instances.
imorSpotOptions :: Lens' InstanceMarketOptionsRequest (Maybe SpotMarketOptions)
imorSpotOptions = lens _imorSpotOptions (\s a -> s {_imorSpotOptions = a})

instance Hashable InstanceMarketOptionsRequest

instance NFData InstanceMarketOptionsRequest

instance ToQuery InstanceMarketOptionsRequest where
  toQuery InstanceMarketOptionsRequest' {..} =
    mconcat
      [ "MarketType" =: _imorMarketType,
        "SpotOptions" =: _imorSpotOptions
      ]
