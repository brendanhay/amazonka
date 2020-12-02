{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
import Network.AWS.EC2.Types.MarketType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The market (purchasing) option for the instances.
--
--
--
-- /See:/ 'launchTemplateInstanceMarketOptions' smart constructor.
data LaunchTemplateInstanceMarketOptions = LaunchTemplateInstanceMarketOptions'
  { _ltimoMarketType ::
      !(Maybe MarketType),
    _ltimoSpotOptions ::
      !( Maybe
           LaunchTemplateSpotMarketOptions
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateInstanceMarketOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltimoMarketType' - The market type.
--
-- * 'ltimoSpotOptions' - The options for Spot Instances.
launchTemplateInstanceMarketOptions ::
  LaunchTemplateInstanceMarketOptions
launchTemplateInstanceMarketOptions =
  LaunchTemplateInstanceMarketOptions'
    { _ltimoMarketType = Nothing,
      _ltimoSpotOptions = Nothing
    }

-- | The market type.
ltimoMarketType :: Lens' LaunchTemplateInstanceMarketOptions (Maybe MarketType)
ltimoMarketType = lens _ltimoMarketType (\s a -> s {_ltimoMarketType = a})

-- | The options for Spot Instances.
ltimoSpotOptions :: Lens' LaunchTemplateInstanceMarketOptions (Maybe LaunchTemplateSpotMarketOptions)
ltimoSpotOptions = lens _ltimoSpotOptions (\s a -> s {_ltimoSpotOptions = a})

instance FromXML LaunchTemplateInstanceMarketOptions where
  parseXML x =
    LaunchTemplateInstanceMarketOptions'
      <$> (x .@? "marketType") <*> (x .@? "spotOptions")

instance Hashable LaunchTemplateInstanceMarketOptions

instance NFData LaunchTemplateInstanceMarketOptions
