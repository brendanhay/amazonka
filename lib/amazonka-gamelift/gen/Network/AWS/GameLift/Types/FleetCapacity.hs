{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetCapacity where

import Network.AWS.GameLift.Types.EC2InstanceCounts
import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the fleet's capacity. Fleet capacity is measured in EC2 instances. By default, new fleets have a capacity of one instance, but can be updated as needed. The maximum number of instances for a fleet is determined by the fleet's instance type.
--
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * 'DescribeFleetAttributes'
--
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- /See:/ 'fleetCapacity' smart constructor.
data FleetCapacity = FleetCapacity'
  { _fcInstanceType ::
      !(Maybe EC2InstanceType),
    _fcFleetId :: !(Maybe Text),
    _fcInstanceCounts :: !(Maybe EC2InstanceCounts)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcInstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- * 'fcFleetId' - A unique identifier for a fleet.
--
-- * 'fcInstanceCounts' - Current status of fleet capacity.
fleetCapacity ::
  FleetCapacity
fleetCapacity =
  FleetCapacity'
    { _fcInstanceType = Nothing,
      _fcFleetId = Nothing,
      _fcInstanceCounts = Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
fcInstanceType :: Lens' FleetCapacity (Maybe EC2InstanceType)
fcInstanceType = lens _fcInstanceType (\s a -> s {_fcInstanceType = a})

-- | A unique identifier for a fleet.
fcFleetId :: Lens' FleetCapacity (Maybe Text)
fcFleetId = lens _fcFleetId (\s a -> s {_fcFleetId = a})

-- | Current status of fleet capacity.
fcInstanceCounts :: Lens' FleetCapacity (Maybe EC2InstanceCounts)
fcInstanceCounts = lens _fcInstanceCounts (\s a -> s {_fcInstanceCounts = a})

instance FromJSON FleetCapacity where
  parseJSON =
    withObject
      "FleetCapacity"
      ( \x ->
          FleetCapacity'
            <$> (x .:? "InstanceType")
            <*> (x .:? "FleetId")
            <*> (x .:? "InstanceCounts")
      )

instance Hashable FleetCapacity

instance NFData FleetCapacity
