{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of Spot Instances in an EC2 Fleet request.
--
--
--
-- /See:/ 'spotOptionsRequest' smart constructor.
data SpotOptionsRequest = SpotOptionsRequest'
  { _sorInstanceInterruptionBehavior ::
      !(Maybe SpotInstanceInterruptionBehavior),
    _sorSingleAvailabilityZone :: !(Maybe Bool),
    _sorMaxTotalPrice :: !(Maybe Text),
    _sorMinTargetCapacity :: !(Maybe Int),
    _sorInstancePoolsToUseCount :: !(Maybe Int),
    _sorMaintenanceStrategies ::
      !(Maybe FleetSpotMaintenanceStrategiesRequest),
    _sorSingleInstanceType :: !(Maybe Bool),
    _sorAllocationStrategy ::
      !(Maybe SpotAllocationStrategy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sorInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'sorSingleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- * 'sorMaxTotalPrice' - The maximum amount per hour for Spot Instances that you're willing to pay.
--
-- * 'sorMinTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- * 'sorInstancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- * 'sorMaintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- * 'sorSingleInstanceType' - Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- * 'sorAllocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet. If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy. If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify. If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
spotOptionsRequest ::
  SpotOptionsRequest
spotOptionsRequest =
  SpotOptionsRequest'
    { _sorInstanceInterruptionBehavior = Nothing,
      _sorSingleAvailabilityZone = Nothing,
      _sorMaxTotalPrice = Nothing,
      _sorMinTargetCapacity = Nothing,
      _sorInstancePoolsToUseCount = Nothing,
      _sorMaintenanceStrategies = Nothing,
      _sorSingleInstanceType = Nothing,
      _sorAllocationStrategy = Nothing
    }

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
sorInstanceInterruptionBehavior :: Lens' SpotOptionsRequest (Maybe SpotInstanceInterruptionBehavior)
sorInstanceInterruptionBehavior = lens _sorInstanceInterruptionBehavior (\s a -> s {_sorInstanceInterruptionBehavior = a})

-- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
sorSingleAvailabilityZone :: Lens' SpotOptionsRequest (Maybe Bool)
sorSingleAvailabilityZone = lens _sorSingleAvailabilityZone (\s a -> s {_sorSingleAvailabilityZone = a})

-- | The maximum amount per hour for Spot Instances that you're willing to pay.
sorMaxTotalPrice :: Lens' SpotOptionsRequest (Maybe Text)
sorMaxTotalPrice = lens _sorMaxTotalPrice (\s a -> s {_sorMaxTotalPrice = a})

-- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
sorMinTargetCapacity :: Lens' SpotOptionsRequest (Maybe Int)
sorMinTargetCapacity = lens _sorMinTargetCapacity (\s a -> s {_sorMinTargetCapacity = a})

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
sorInstancePoolsToUseCount :: Lens' SpotOptionsRequest (Maybe Int)
sorInstancePoolsToUseCount = lens _sorInstancePoolsToUseCount (\s a -> s {_sorInstancePoolsToUseCount = a})

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
sorMaintenanceStrategies :: Lens' SpotOptionsRequest (Maybe FleetSpotMaintenanceStrategiesRequest)
sorMaintenanceStrategies = lens _sorMaintenanceStrategies (\s a -> s {_sorMaintenanceStrategies = a})

-- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
sorSingleInstanceType :: Lens' SpotOptionsRequest (Maybe Bool)
sorSingleInstanceType = lens _sorSingleInstanceType (\s a -> s {_sorSingleInstanceType = a})

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet. If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy. If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify. If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
sorAllocationStrategy :: Lens' SpotOptionsRequest (Maybe SpotAllocationStrategy)
sorAllocationStrategy = lens _sorAllocationStrategy (\s a -> s {_sorAllocationStrategy = a})

instance Hashable SpotOptionsRequest

instance NFData SpotOptionsRequest

instance ToQuery SpotOptionsRequest where
  toQuery SpotOptionsRequest' {..} =
    mconcat
      [ "InstanceInterruptionBehavior"
          =: _sorInstanceInterruptionBehavior,
        "SingleAvailabilityZone" =: _sorSingleAvailabilityZone,
        "MaxTotalPrice" =: _sorMaxTotalPrice,
        "MinTargetCapacity" =: _sorMinTargetCapacity,
        "InstancePoolsToUseCount" =: _sorInstancePoolsToUseCount,
        "MaintenanceStrategies" =: _sorMaintenanceStrategies,
        "SingleInstanceType" =: _sorSingleInstanceType,
        "AllocationStrategy" =: _sorAllocationStrategy
      ]
