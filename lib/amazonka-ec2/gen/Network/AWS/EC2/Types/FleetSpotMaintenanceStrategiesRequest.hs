{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
--
--
-- /See:/ 'fleetSpotMaintenanceStrategiesRequest' smart constructor.
newtype FleetSpotMaintenanceStrategiesRequest = FleetSpotMaintenanceStrategiesRequest'
  { _fsmsrCapacityRebalance ::
      Maybe
        FleetSpotCapacityRebalanceRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetSpotMaintenanceStrategiesRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsmsrCapacityRebalance' - The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
fleetSpotMaintenanceStrategiesRequest ::
  FleetSpotMaintenanceStrategiesRequest
fleetSpotMaintenanceStrategiesRequest =
  FleetSpotMaintenanceStrategiesRequest'
    { _fsmsrCapacityRebalance =
        Nothing
    }

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
fsmsrCapacityRebalance :: Lens' FleetSpotMaintenanceStrategiesRequest (Maybe FleetSpotCapacityRebalanceRequest)
fsmsrCapacityRebalance = lens _fsmsrCapacityRebalance (\s a -> s {_fsmsrCapacityRebalance = a})

instance Hashable FleetSpotMaintenanceStrategiesRequest

instance NFData FleetSpotMaintenanceStrategiesRequest

instance ToQuery FleetSpotMaintenanceStrategiesRequest where
  toQuery FleetSpotMaintenanceStrategiesRequest' {..} =
    mconcat ["CapacityRebalance" =: _fsmsrCapacityRebalance]
