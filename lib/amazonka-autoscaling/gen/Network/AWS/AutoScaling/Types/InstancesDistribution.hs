{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstancesDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstancesDistribution where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instances distribution for an Auto Scaling group with a 'MixedInstancesPolicy' .
--
--
-- The instances distribution specifies the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities.
--
-- When you update @SpotAllocationStrategy@ , @SpotInstancePools@ , or @SpotMaxPrice@ , this update action does not deploy any changes across the running Amazon EC2 instances in the group. Your existing Spot Instances continue to run as long as the maximum price for those instances is higher than the current Spot price. When scale out occurs, Amazon EC2 Auto Scaling launches instances based on the new settings. When scale in occurs, Amazon EC2 Auto Scaling terminates instances according to the group's termination policies.
--
--
-- /See:/ 'instancesDistribution' smart constructor.
data InstancesDistribution = InstancesDistribution'
  { _idSpotAllocationStrategy ::
      !(Maybe Text),
    _idSpotInstancePools :: !(Maybe Int),
    _idSpotMaxPrice :: !(Maybe Text),
    _idOnDemandBaseCapacity :: !(Maybe Int),
    _idOnDemandAllocationStrategy :: !(Maybe Text),
    _idOnDemandPercentageAboveBaseCapacity ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancesDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idSpotAllocationStrategy' - Indicates how to allocate instances across Spot Instance pools. If the allocation strategy is @capacity-optimized@ (recommended), the Auto Scaling group launches instances using Spot pools that are optimally chosen based on the available Spot capacity. If the allocation strategy is @lowest-price@ , the Auto Scaling group launches instances using the Spot pools with the lowest price, and evenly allocates your instances across the number of Spot pools that you specify. Defaults to @lowest-price@ if not specified.
--
-- * 'idSpotInstancePools' - The number of Spot Instance pools across which to allocate your Spot Instances. The Spot pools are determined from the different instance types in the overrides. Valid only when the Spot allocation strategy is @lowest-price@ . Value must be in the range of 1 to 20. Defaults to 2 if not specified.
--
-- * 'idSpotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance. If you leave the value at its default (empty), Amazon EC2 Auto Scaling uses the On-Demand price as the maximum Spot price. To remove a value that you previously set, include the property but specify an empty string ("") for the value.
--
-- * 'idOnDemandBaseCapacity' - The minimum amount of the Auto Scaling group's capacity that must be fulfilled by On-Demand Instances. This base portion is provisioned first as your group scales. Defaults to 0 if not specified. If you specify weights for the instance types in the overrides, set the value of @OnDemandBaseCapacity@ in terms of the number of capacity units, and not the number of instances.
--
-- * 'idOnDemandAllocationStrategy' - Indicates how to allocate instance types to fulfill On-Demand capacity. The only valid value is @prioritized@ , which is also the default value. This strategy uses the order of instance types in the overrides to define the launch priority of each instance type. The first instance type in the array is prioritized higher than the last. If all your On-Demand capacity cannot be fulfilled using your highest priority instance, then the Auto Scaling groups launches the remaining capacity using the second priority instance type, and so on.
--
-- * 'idOnDemandPercentageAboveBaseCapacity' - Controls the percentages of On-Demand Instances and Spot Instances for your additional capacity beyond @OnDemandBaseCapacity@ . Expressed as a number (for example, 20 specifies 20% On-Demand Instances, 80% Spot Instances). Defaults to 100 if not specified. If set to 100, only On-Demand Instances are provisioned.
instancesDistribution ::
  InstancesDistribution
instancesDistribution =
  InstancesDistribution'
    { _idSpotAllocationStrategy = Nothing,
      _idSpotInstancePools = Nothing,
      _idSpotMaxPrice = Nothing,
      _idOnDemandBaseCapacity = Nothing,
      _idOnDemandAllocationStrategy = Nothing,
      _idOnDemandPercentageAboveBaseCapacity = Nothing
    }

-- | Indicates how to allocate instances across Spot Instance pools. If the allocation strategy is @capacity-optimized@ (recommended), the Auto Scaling group launches instances using Spot pools that are optimally chosen based on the available Spot capacity. If the allocation strategy is @lowest-price@ , the Auto Scaling group launches instances using the Spot pools with the lowest price, and evenly allocates your instances across the number of Spot pools that you specify. Defaults to @lowest-price@ if not specified.
idSpotAllocationStrategy :: Lens' InstancesDistribution (Maybe Text)
idSpotAllocationStrategy = lens _idSpotAllocationStrategy (\s a -> s {_idSpotAllocationStrategy = a})

-- | The number of Spot Instance pools across which to allocate your Spot Instances. The Spot pools are determined from the different instance types in the overrides. Valid only when the Spot allocation strategy is @lowest-price@ . Value must be in the range of 1 to 20. Defaults to 2 if not specified.
idSpotInstancePools :: Lens' InstancesDistribution (Maybe Int)
idSpotInstancePools = lens _idSpotInstancePools (\s a -> s {_idSpotInstancePools = a})

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. If you leave the value at its default (empty), Amazon EC2 Auto Scaling uses the On-Demand price as the maximum Spot price. To remove a value that you previously set, include the property but specify an empty string ("") for the value.
idSpotMaxPrice :: Lens' InstancesDistribution (Maybe Text)
idSpotMaxPrice = lens _idSpotMaxPrice (\s a -> s {_idSpotMaxPrice = a})

-- | The minimum amount of the Auto Scaling group's capacity that must be fulfilled by On-Demand Instances. This base portion is provisioned first as your group scales. Defaults to 0 if not specified. If you specify weights for the instance types in the overrides, set the value of @OnDemandBaseCapacity@ in terms of the number of capacity units, and not the number of instances.
idOnDemandBaseCapacity :: Lens' InstancesDistribution (Maybe Int)
idOnDemandBaseCapacity = lens _idOnDemandBaseCapacity (\s a -> s {_idOnDemandBaseCapacity = a})

-- | Indicates how to allocate instance types to fulfill On-Demand capacity. The only valid value is @prioritized@ , which is also the default value. This strategy uses the order of instance types in the overrides to define the launch priority of each instance type. The first instance type in the array is prioritized higher than the last. If all your On-Demand capacity cannot be fulfilled using your highest priority instance, then the Auto Scaling groups launches the remaining capacity using the second priority instance type, and so on.
idOnDemandAllocationStrategy :: Lens' InstancesDistribution (Maybe Text)
idOnDemandAllocationStrategy = lens _idOnDemandAllocationStrategy (\s a -> s {_idOnDemandAllocationStrategy = a})

-- | Controls the percentages of On-Demand Instances and Spot Instances for your additional capacity beyond @OnDemandBaseCapacity@ . Expressed as a number (for example, 20 specifies 20% On-Demand Instances, 80% Spot Instances). Defaults to 100 if not specified. If set to 100, only On-Demand Instances are provisioned.
idOnDemandPercentageAboveBaseCapacity :: Lens' InstancesDistribution (Maybe Int)
idOnDemandPercentageAboveBaseCapacity = lens _idOnDemandPercentageAboveBaseCapacity (\s a -> s {_idOnDemandPercentageAboveBaseCapacity = a})

instance FromXML InstancesDistribution where
  parseXML x =
    InstancesDistribution'
      <$> (x .@? "SpotAllocationStrategy")
      <*> (x .@? "SpotInstancePools")
      <*> (x .@? "SpotMaxPrice")
      <*> (x .@? "OnDemandBaseCapacity")
      <*> (x .@? "OnDemandAllocationStrategy")
      <*> (x .@? "OnDemandPercentageAboveBaseCapacity")

instance Hashable InstancesDistribution

instance NFData InstancesDistribution

instance ToQuery InstancesDistribution where
  toQuery InstancesDistribution' {..} =
    mconcat
      [ "SpotAllocationStrategy" =: _idSpotAllocationStrategy,
        "SpotInstancePools" =: _idSpotInstancePools,
        "SpotMaxPrice" =: _idSpotMaxPrice,
        "OnDemandBaseCapacity" =: _idOnDemandBaseCapacity,
        "OnDemandAllocationStrategy" =: _idOnDemandAllocationStrategy,
        "OnDemandPercentageAboveBaseCapacity"
          =: _idOnDemandPercentageAboveBaseCapacity
      ]
