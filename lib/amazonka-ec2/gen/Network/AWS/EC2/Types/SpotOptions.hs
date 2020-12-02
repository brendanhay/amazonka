{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
--
--
-- /See:/ 'spotOptions' smart constructor.
data SpotOptions = SpotOptions'
  { _soInstanceInterruptionBehavior ::
      !(Maybe SpotInstanceInterruptionBehavior),
    _soSingleAvailabilityZone :: !(Maybe Bool),
    _soMaxTotalPrice :: !(Maybe Text),
    _soMinTargetCapacity :: !(Maybe Int),
    _soInstancePoolsToUseCount :: !(Maybe Int),
    _soMaintenanceStrategies ::
      !(Maybe FleetSpotMaintenanceStrategies),
    _soSingleInstanceType :: !(Maybe Bool),
    _soAllocationStrategy :: !(Maybe SpotAllocationStrategy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'soSingleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- * 'soMaxTotalPrice' - The maximum amount per hour for Spot Instances that you're willing to pay.
--
-- * 'soMinTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- * 'soInstancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot capacity. Valid only when __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- * 'soMaintenanceStrategies' - The strategies for managing your workloads on your Spot Instances that will be interrupted. Currently only the capacity rebalance strategy is available.
--
-- * 'soSingleInstanceType' - Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- * 'soAllocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet. If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy. If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify. If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
spotOptions ::
  SpotOptions
spotOptions =
  SpotOptions'
    { _soInstanceInterruptionBehavior = Nothing,
      _soSingleAvailabilityZone = Nothing,
      _soMaxTotalPrice = Nothing,
      _soMinTargetCapacity = Nothing,
      _soInstancePoolsToUseCount = Nothing,
      _soMaintenanceStrategies = Nothing,
      _soSingleInstanceType = Nothing,
      _soAllocationStrategy = Nothing
    }

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
soInstanceInterruptionBehavior :: Lens' SpotOptions (Maybe SpotInstanceInterruptionBehavior)
soInstanceInterruptionBehavior = lens _soInstanceInterruptionBehavior (\s a -> s {_soInstanceInterruptionBehavior = a})

-- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
soSingleAvailabilityZone :: Lens' SpotOptions (Maybe Bool)
soSingleAvailabilityZone = lens _soSingleAvailabilityZone (\s a -> s {_soSingleAvailabilityZone = a})

-- | The maximum amount per hour for Spot Instances that you're willing to pay.
soMaxTotalPrice :: Lens' SpotOptions (Maybe Text)
soMaxTotalPrice = lens _soMaxTotalPrice (\s a -> s {_soMaxTotalPrice = a})

-- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
soMinTargetCapacity :: Lens' SpotOptions (Maybe Int)
soMinTargetCapacity = lens _soMinTargetCapacity (\s a -> s {_soMinTargetCapacity = a})

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
soInstancePoolsToUseCount :: Lens' SpotOptions (Maybe Int)
soInstancePoolsToUseCount = lens _soInstancePoolsToUseCount (\s a -> s {_soInstancePoolsToUseCount = a})

-- | The strategies for managing your workloads on your Spot Instances that will be interrupted. Currently only the capacity rebalance strategy is available.
soMaintenanceStrategies :: Lens' SpotOptions (Maybe FleetSpotMaintenanceStrategies)
soMaintenanceStrategies = lens _soMaintenanceStrategies (\s a -> s {_soMaintenanceStrategies = a})

-- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
soSingleInstanceType :: Lens' SpotOptions (Maybe Bool)
soSingleInstanceType = lens _soSingleInstanceType (\s a -> s {_soSingleInstanceType = a})

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet. If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy. If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify. If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
soAllocationStrategy :: Lens' SpotOptions (Maybe SpotAllocationStrategy)
soAllocationStrategy = lens _soAllocationStrategy (\s a -> s {_soAllocationStrategy = a})

instance FromXML SpotOptions where
  parseXML x =
    SpotOptions'
      <$> (x .@? "instanceInterruptionBehavior")
      <*> (x .@? "singleAvailabilityZone")
      <*> (x .@? "maxTotalPrice")
      <*> (x .@? "minTargetCapacity")
      <*> (x .@? "instancePoolsToUseCount")
      <*> (x .@? "maintenanceStrategies")
      <*> (x .@? "singleInstanceType")
      <*> (x .@? "allocationStrategy")

instance Hashable SpotOptions

instance NFData SpotOptions
