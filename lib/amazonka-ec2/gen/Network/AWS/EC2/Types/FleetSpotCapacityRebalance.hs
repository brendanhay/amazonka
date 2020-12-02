{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotCapacityRebalance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotCapacityRebalance where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetReplacementStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
--
--
-- /See:/ 'fleetSpotCapacityRebalance' smart constructor.
newtype FleetSpotCapacityRebalance = FleetSpotCapacityRebalance'
  { _fscrReplacementStrategy ::
      Maybe FleetReplacementStrategy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetSpotCapacityRebalance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fscrReplacementStrategy' - To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . Only available for fleets of type @maintain@ .
fleetSpotCapacityRebalance ::
  FleetSpotCapacityRebalance
fleetSpotCapacityRebalance =
  FleetSpotCapacityRebalance' {_fscrReplacementStrategy = Nothing}

-- | To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . Only available for fleets of type @maintain@ .
fscrReplacementStrategy :: Lens' FleetSpotCapacityRebalance (Maybe FleetReplacementStrategy)
fscrReplacementStrategy = lens _fscrReplacementStrategy (\s a -> s {_fscrReplacementStrategy = a})

instance FromXML FleetSpotCapacityRebalance where
  parseXML x =
    FleetSpotCapacityRebalance' <$> (x .@? "replacementStrategy")

instance Hashable FleetSpotCapacityRebalance

instance NFData FleetSpotCapacityRebalance
