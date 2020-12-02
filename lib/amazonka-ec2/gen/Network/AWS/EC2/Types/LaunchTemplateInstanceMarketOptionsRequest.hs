{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Network.AWS.EC2.Types.MarketType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The market (purchasing) option for the instances.
--
--
--
-- /See:/ 'launchTemplateInstanceMarketOptionsRequest' smart constructor.
data LaunchTemplateInstanceMarketOptionsRequest = LaunchTemplateInstanceMarketOptionsRequest'
  { _ltimorMarketType ::
      !( Maybe
           MarketType
       ),
    _ltimorSpotOptions ::
      !( Maybe
           LaunchTemplateSpotMarketOptionsRequest
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LaunchTemplateInstanceMarketOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltimorMarketType' - The market type.
--
-- * 'ltimorSpotOptions' - The options for Spot Instances.
launchTemplateInstanceMarketOptionsRequest ::
  LaunchTemplateInstanceMarketOptionsRequest
launchTemplateInstanceMarketOptionsRequest =
  LaunchTemplateInstanceMarketOptionsRequest'
    { _ltimorMarketType =
        Nothing,
      _ltimorSpotOptions = Nothing
    }

-- | The market type.
ltimorMarketType :: Lens' LaunchTemplateInstanceMarketOptionsRequest (Maybe MarketType)
ltimorMarketType = lens _ltimorMarketType (\s a -> s {_ltimorMarketType = a})

-- | The options for Spot Instances.
ltimorSpotOptions :: Lens' LaunchTemplateInstanceMarketOptionsRequest (Maybe LaunchTemplateSpotMarketOptionsRequest)
ltimorSpotOptions = lens _ltimorSpotOptions (\s a -> s {_ltimorSpotOptions = a})

instance Hashable LaunchTemplateInstanceMarketOptionsRequest

instance NFData LaunchTemplateInstanceMarketOptionsRequest

instance ToQuery LaunchTemplateInstanceMarketOptionsRequest where
  toQuery LaunchTemplateInstanceMarketOptionsRequest' {..} =
    mconcat
      [ "MarketType" =: _ltimorMarketType,
        "SpotOptions" =: _ltimorSpotOptions
      ]
