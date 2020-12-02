{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceDefinition where

import Network.AWS.GameLift.Types.GameServerGroupInstanceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
--
-- An allowed instance type for a 'GameServerGroup' . All game server groups must have at least two instance types defined for it. GameLift FleetIQ periodically evaluates each defined instance type for viability. It then updates the Auto Scaling group with the list of viable instance types.
--
--
-- /See:/ 'instanceDefinition' smart constructor.
data InstanceDefinition = InstanceDefinition'
  { _idWeightedCapacity ::
      !(Maybe Text),
    _idInstanceType :: !GameServerGroupInstanceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idWeightedCapacity' - Instance weighting that indicates how much this instance type contributes to the total capacity of a game server group. Instance weights are used by GameLift FleetIQ to calculate the instance type's cost per unit hour and better identify the most cost-effective options. For detailed information on weighting instance capacity, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting> in the /Amazon EC2 Auto Scaling User Guide/ . Default value is "1".
--
-- * 'idInstanceType' - An EC2 instance type designation.
instanceDefinition ::
  -- | 'idInstanceType'
  GameServerGroupInstanceType ->
  InstanceDefinition
instanceDefinition pInstanceType_ =
  InstanceDefinition'
    { _idWeightedCapacity = Nothing,
      _idInstanceType = pInstanceType_
    }

-- | Instance weighting that indicates how much this instance type contributes to the total capacity of a game server group. Instance weights are used by GameLift FleetIQ to calculate the instance type's cost per unit hour and better identify the most cost-effective options. For detailed information on weighting instance capacity, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting> in the /Amazon EC2 Auto Scaling User Guide/ . Default value is "1".
idWeightedCapacity :: Lens' InstanceDefinition (Maybe Text)
idWeightedCapacity = lens _idWeightedCapacity (\s a -> s {_idWeightedCapacity = a})

-- | An EC2 instance type designation.
idInstanceType :: Lens' InstanceDefinition GameServerGroupInstanceType
idInstanceType = lens _idInstanceType (\s a -> s {_idInstanceType = a})

instance FromJSON InstanceDefinition where
  parseJSON =
    withObject
      "InstanceDefinition"
      ( \x ->
          InstanceDefinition'
            <$> (x .:? "WeightedCapacity") <*> (x .: "InstanceType")
      )

instance Hashable InstanceDefinition

instance NFData InstanceDefinition

instance ToJSON InstanceDefinition where
  toJSON InstanceDefinition' {..} =
    object
      ( catMaybes
          [ ("WeightedCapacity" .=) <$> _idWeightedCapacity,
            Just ("InstanceType" .= _idInstanceType)
          ]
      )
