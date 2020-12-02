{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotCapacityRebalance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotCapacityRebalance where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReplacementStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#spot-fleet-capacity-rebalance Capacity rebalancing> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
--
-- /See:/ 'spotCapacityRebalance' smart constructor.
newtype SpotCapacityRebalance = SpotCapacityRebalance'
  { _scrReplacementStrategy ::
      Maybe ReplacementStrategy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotCapacityRebalance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrReplacementStrategy' - The replacement strategy to use. Only available for fleets of type @maintain@ . You must specify a value, otherwise you get an error. To allow Spot Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for a Spot Instance in the fleet, specify @launch@ .
spotCapacityRebalance ::
  SpotCapacityRebalance
spotCapacityRebalance =
  SpotCapacityRebalance' {_scrReplacementStrategy = Nothing}

-- | The replacement strategy to use. Only available for fleets of type @maintain@ . You must specify a value, otherwise you get an error. To allow Spot Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for a Spot Instance in the fleet, specify @launch@ .
scrReplacementStrategy :: Lens' SpotCapacityRebalance (Maybe ReplacementStrategy)
scrReplacementStrategy = lens _scrReplacementStrategy (\s a -> s {_scrReplacementStrategy = a})

instance FromXML SpotCapacityRebalance where
  parseXML x =
    SpotCapacityRebalance' <$> (x .@? "replacementStrategy")

instance Hashable SpotCapacityRebalance

instance NFData SpotCapacityRebalance

instance ToQuery SpotCapacityRebalance where
  toQuery SpotCapacityRebalance' {..} =
    mconcat ["ReplacementStrategy" =: _scrReplacementStrategy]
