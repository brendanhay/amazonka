{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetConfig where

import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
import Network.AWS.EMR.Types.InstanceFleetType
import Network.AWS.EMR.Types.InstanceTypeConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration that defines an instance fleet.
--
--
--
-- /See:/ 'instanceFleetConfig' smart constructor.
data InstanceFleetConfig = InstanceFleetConfig'
  { _ifcInstanceTypeConfigs ::
      !(Maybe [InstanceTypeConfig]),
    _ifcTargetOnDemandCapacity :: !(Maybe Nat),
    _ifcName :: !(Maybe Text),
    _ifcTargetSpotCapacity :: !(Maybe Nat),
    _ifcLaunchSpecifications ::
      !(Maybe InstanceFleetProvisioningSpecifications),
    _ifcInstanceFleetType :: !InstanceFleetType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceFleetConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifcInstanceTypeConfigs' - The instance type configurations that define the EC2 instances in the instance fleet.
--
-- * 'ifcTargetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- * 'ifcName' - The friendly name of the instance fleet.
--
-- * 'ifcTargetSpotCapacity' - The target capacity of Spot units for the instance fleet, which determines how many Spot Instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- * 'ifcLaunchSpecifications' - The launch specification for the instance fleet.
--
-- * 'ifcInstanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
instanceFleetConfig ::
  -- | 'ifcInstanceFleetType'
  InstanceFleetType ->
  InstanceFleetConfig
instanceFleetConfig pInstanceFleetType_ =
  InstanceFleetConfig'
    { _ifcInstanceTypeConfigs = Nothing,
      _ifcTargetOnDemandCapacity = Nothing,
      _ifcName = Nothing,
      _ifcTargetSpotCapacity = Nothing,
      _ifcLaunchSpecifications = Nothing,
      _ifcInstanceFleetType = pInstanceFleetType_
    }

-- | The instance type configurations that define the EC2 instances in the instance fleet.
ifcInstanceTypeConfigs :: Lens' InstanceFleetConfig [InstanceTypeConfig]
ifcInstanceTypeConfigs = lens _ifcInstanceTypeConfigs (\s a -> s {_ifcInstanceTypeConfigs = a}) . _Default . _Coerce

-- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
ifcTargetOnDemandCapacity :: Lens' InstanceFleetConfig (Maybe Natural)
ifcTargetOnDemandCapacity = lens _ifcTargetOnDemandCapacity (\s a -> s {_ifcTargetOnDemandCapacity = a}) . mapping _Nat

-- | The friendly name of the instance fleet.
ifcName :: Lens' InstanceFleetConfig (Maybe Text)
ifcName = lens _ifcName (\s a -> s {_ifcName = a})

-- | The target capacity of Spot units for the instance fleet, which determines how many Spot Instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
ifcTargetSpotCapacity :: Lens' InstanceFleetConfig (Maybe Natural)
ifcTargetSpotCapacity = lens _ifcTargetSpotCapacity (\s a -> s {_ifcTargetSpotCapacity = a}) . mapping _Nat

-- | The launch specification for the instance fleet.
ifcLaunchSpecifications :: Lens' InstanceFleetConfig (Maybe InstanceFleetProvisioningSpecifications)
ifcLaunchSpecifications = lens _ifcLaunchSpecifications (\s a -> s {_ifcLaunchSpecifications = a})

-- | The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
ifcInstanceFleetType :: Lens' InstanceFleetConfig InstanceFleetType
ifcInstanceFleetType = lens _ifcInstanceFleetType (\s a -> s {_ifcInstanceFleetType = a})

instance Hashable InstanceFleetConfig

instance NFData InstanceFleetConfig

instance ToJSON InstanceFleetConfig where
  toJSON InstanceFleetConfig' {..} =
    object
      ( catMaybes
          [ ("InstanceTypeConfigs" .=) <$> _ifcInstanceTypeConfigs,
            ("TargetOnDemandCapacity" .=) <$> _ifcTargetOnDemandCapacity,
            ("Name" .=) <$> _ifcName,
            ("TargetSpotCapacity" .=) <$> _ifcTargetSpotCapacity,
            ("LaunchSpecifications" .=) <$> _ifcLaunchSpecifications,
            Just ("InstanceFleetType" .= _ifcInstanceFleetType)
          ]
      )
