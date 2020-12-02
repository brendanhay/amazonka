{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateOverrides where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.PlacementResponse
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes overrides for a launch template.
--
--
--
-- /See:/ 'fleetLaunchTemplateOverrides' smart constructor.
data FleetLaunchTemplateOverrides = FleetLaunchTemplateOverrides'
  { _fltoPriority ::
      !(Maybe Double),
    _fltoWeightedCapacity ::
      !(Maybe Double),
    _fltoSubnetId :: !(Maybe Text),
    _fltoInstanceType ::
      !(Maybe InstanceType),
    _fltoAvailabilityZone ::
      !(Maybe Text),
    _fltoPlacement ::
      !(Maybe PlacementResponse),
    _fltoMaxPrice :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetLaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltoPriority' - The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the override has the lowest priority.
--
-- * 'fltoWeightedCapacity' - The number of units provided by the specified instance type.
--
-- * 'fltoSubnetId' - The ID of the subnet in which to launch the instances.
--
-- * 'fltoInstanceType' - The instance type.
--
-- * 'fltoAvailabilityZone' - The Availability Zone in which to launch the instances.
--
-- * 'fltoPlacement' - The location where the instance launched, if applicable.
--
-- * 'fltoMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance.
fleetLaunchTemplateOverrides ::
  FleetLaunchTemplateOverrides
fleetLaunchTemplateOverrides =
  FleetLaunchTemplateOverrides'
    { _fltoPriority = Nothing,
      _fltoWeightedCapacity = Nothing,
      _fltoSubnetId = Nothing,
      _fltoInstanceType = Nothing,
      _fltoAvailabilityZone = Nothing,
      _fltoPlacement = Nothing,
      _fltoMaxPrice = Nothing
    }

-- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the override has the lowest priority.
fltoPriority :: Lens' FleetLaunchTemplateOverrides (Maybe Double)
fltoPriority = lens _fltoPriority (\s a -> s {_fltoPriority = a})

-- | The number of units provided by the specified instance type.
fltoWeightedCapacity :: Lens' FleetLaunchTemplateOverrides (Maybe Double)
fltoWeightedCapacity = lens _fltoWeightedCapacity (\s a -> s {_fltoWeightedCapacity = a})

-- | The ID of the subnet in which to launch the instances.
fltoSubnetId :: Lens' FleetLaunchTemplateOverrides (Maybe Text)
fltoSubnetId = lens _fltoSubnetId (\s a -> s {_fltoSubnetId = a})

-- | The instance type.
fltoInstanceType :: Lens' FleetLaunchTemplateOverrides (Maybe InstanceType)
fltoInstanceType = lens _fltoInstanceType (\s a -> s {_fltoInstanceType = a})

-- | The Availability Zone in which to launch the instances.
fltoAvailabilityZone :: Lens' FleetLaunchTemplateOverrides (Maybe Text)
fltoAvailabilityZone = lens _fltoAvailabilityZone (\s a -> s {_fltoAvailabilityZone = a})

-- | The location where the instance launched, if applicable.
fltoPlacement :: Lens' FleetLaunchTemplateOverrides (Maybe PlacementResponse)
fltoPlacement = lens _fltoPlacement (\s a -> s {_fltoPlacement = a})

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
fltoMaxPrice :: Lens' FleetLaunchTemplateOverrides (Maybe Text)
fltoMaxPrice = lens _fltoMaxPrice (\s a -> s {_fltoMaxPrice = a})

instance FromXML FleetLaunchTemplateOverrides where
  parseXML x =
    FleetLaunchTemplateOverrides'
      <$> (x .@? "priority")
      <*> (x .@? "weightedCapacity")
      <*> (x .@? "subnetId")
      <*> (x .@? "instanceType")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "placement")
      <*> (x .@? "maxPrice")

instance Hashable FleetLaunchTemplateOverrides

instance NFData FleetLaunchTemplateOverrides
