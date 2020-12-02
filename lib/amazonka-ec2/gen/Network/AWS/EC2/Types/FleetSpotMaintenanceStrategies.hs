{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetSpotCapacityRebalance
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
--
--
-- /See:/ 'fleetSpotMaintenanceStrategies' smart constructor.
newtype FleetSpotMaintenanceStrategies = FleetSpotMaintenanceStrategies'
  { _fsmsCapacityRebalance ::
      Maybe
        FleetSpotCapacityRebalance
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetSpotMaintenanceStrategies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsmsCapacityRebalance' - The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
fleetSpotMaintenanceStrategies ::
  FleetSpotMaintenanceStrategies
fleetSpotMaintenanceStrategies =
  FleetSpotMaintenanceStrategies' {_fsmsCapacityRebalance = Nothing}

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
fsmsCapacityRebalance :: Lens' FleetSpotMaintenanceStrategies (Maybe FleetSpotCapacityRebalance)
fsmsCapacityRebalance = lens _fsmsCapacityRebalance (\s a -> s {_fsmsCapacityRebalance = a})

instance FromXML FleetSpotMaintenanceStrategies where
  parseXML x =
    FleetSpotMaintenanceStrategies' <$> (x .@? "capacityRebalance")

instance Hashable FleetSpotMaintenanceStrategies

instance NFData FleetSpotMaintenanceStrategies
