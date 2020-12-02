{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Updates GameLift FleetIQ-specific properties for a game server group. Many Auto Scaling group properties are updated on the Auto Scaling group directly, including the launch template, Auto Scaling policies, and maximum/minimum/desired instance counts.
--
-- To update the game server group, specify the game server group ID and provide the updated values. Before applying the updates, the new values are validated to ensure that GameLift FleetIQ can continue to perform instance balancing activity. If successful, a 'GameServerGroup' object is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
--
-- __Related operations__
--
--     * 'CreateGameServerGroup'
--
--     * 'ListGameServerGroups'
--
--     * 'DescribeGameServerGroup'
--
--     * 'UpdateGameServerGroup'
--
--     * 'DeleteGameServerGroup'
--
--     * 'ResumeGameServerGroup'
--
--     * 'SuspendGameServerGroup'
--
--     * 'DescribeGameServerInstances'
module Network.AWS.GameLift.UpdateGameServerGroup
  ( -- * Creating a Request
    updateGameServerGroup,
    UpdateGameServerGroup,

    -- * Request Lenses
    ugsgInstanceDefinitions,
    ugsgBalancingStrategy,
    ugsgGameServerProtectionPolicy,
    ugsgRoleARN,
    ugsgGameServerGroupName,

    -- * Destructuring the Response
    updateGameServerGroupResponse,
    UpdateGameServerGroupResponse,

    -- * Response Lenses
    ugsgrsGameServerGroup,
    ugsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGameServerGroup' smart constructor.
data UpdateGameServerGroup = UpdateGameServerGroup'
  { _ugsgInstanceDefinitions ::
      !(Maybe (List1 InstanceDefinition)),
    _ugsgBalancingStrategy ::
      !(Maybe BalancingStrategy),
    _ugsgGameServerProtectionPolicy ::
      !(Maybe GameServerProtectionPolicy),
    _ugsgRoleARN :: !(Maybe Text),
    _ugsgGameServerGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGameServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsgInstanceDefinitions' - An updated list of EC2 instance types to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
--
-- * 'ugsgBalancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
--
-- * 'ugsgGameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
--
-- * 'ugsgRoleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- * 'ugsgGameServerGroupName' - A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
updateGameServerGroup ::
  -- | 'ugsgGameServerGroupName'
  Text ->
  UpdateGameServerGroup
updateGameServerGroup pGameServerGroupName_ =
  UpdateGameServerGroup'
    { _ugsgInstanceDefinitions = Nothing,
      _ugsgBalancingStrategy = Nothing,
      _ugsgGameServerProtectionPolicy = Nothing,
      _ugsgRoleARN = Nothing,
      _ugsgGameServerGroupName = pGameServerGroupName_
    }

-- | An updated list of EC2 instance types to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
ugsgInstanceDefinitions :: Lens' UpdateGameServerGroup (Maybe (NonEmpty InstanceDefinition))
ugsgInstanceDefinitions = lens _ugsgInstanceDefinitions (\s a -> s {_ugsgInstanceDefinitions = a}) . mapping _List1

-- | Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
ugsgBalancingStrategy :: Lens' UpdateGameServerGroup (Maybe BalancingStrategy)
ugsgBalancingStrategy = lens _ugsgBalancingStrategy (\s a -> s {_ugsgBalancingStrategy = a})

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
ugsgGameServerProtectionPolicy :: Lens' UpdateGameServerGroup (Maybe GameServerProtectionPolicy)
ugsgGameServerProtectionPolicy = lens _ugsgGameServerProtectionPolicy (\s a -> s {_ugsgGameServerProtectionPolicy = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
ugsgRoleARN :: Lens' UpdateGameServerGroup (Maybe Text)
ugsgRoleARN = lens _ugsgRoleARN (\s a -> s {_ugsgRoleARN = a})

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
ugsgGameServerGroupName :: Lens' UpdateGameServerGroup Text
ugsgGameServerGroupName = lens _ugsgGameServerGroupName (\s a -> s {_ugsgGameServerGroupName = a})

instance AWSRequest UpdateGameServerGroup where
  type Rs UpdateGameServerGroup = UpdateGameServerGroupResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          UpdateGameServerGroupResponse'
            <$> (x .?> "GameServerGroup") <*> (pure (fromEnum s))
      )

instance Hashable UpdateGameServerGroup

instance NFData UpdateGameServerGroup

instance ToHeaders UpdateGameServerGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.UpdateGameServerGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateGameServerGroup where
  toJSON UpdateGameServerGroup' {..} =
    object
      ( catMaybes
          [ ("InstanceDefinitions" .=) <$> _ugsgInstanceDefinitions,
            ("BalancingStrategy" .=) <$> _ugsgBalancingStrategy,
            ("GameServerProtectionPolicy" .=)
              <$> _ugsgGameServerProtectionPolicy,
            ("RoleArn" .=) <$> _ugsgRoleARN,
            Just ("GameServerGroupName" .= _ugsgGameServerGroupName)
          ]
      )

instance ToPath UpdateGameServerGroup where
  toPath = const "/"

instance ToQuery UpdateGameServerGroup where
  toQuery = const mempty

-- | /See:/ 'updateGameServerGroupResponse' smart constructor.
data UpdateGameServerGroupResponse = UpdateGameServerGroupResponse'
  { _ugsgrsGameServerGroup ::
      !(Maybe GameServerGroup),
    _ugsgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGameServerGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsgrsGameServerGroup' - An object that describes the game server group resource with updated properties.
--
-- * 'ugsgrsResponseStatus' - -- | The response status code.
updateGameServerGroupResponse ::
  -- | 'ugsgrsResponseStatus'
  Int ->
  UpdateGameServerGroupResponse
updateGameServerGroupResponse pResponseStatus_ =
  UpdateGameServerGroupResponse'
    { _ugsgrsGameServerGroup = Nothing,
      _ugsgrsResponseStatus = pResponseStatus_
    }

-- | An object that describes the game server group resource with updated properties.
ugsgrsGameServerGroup :: Lens' UpdateGameServerGroupResponse (Maybe GameServerGroup)
ugsgrsGameServerGroup = lens _ugsgrsGameServerGroup (\s a -> s {_ugsgrsGameServerGroup = a})

-- | -- | The response status code.
ugsgrsResponseStatus :: Lens' UpdateGameServerGroupResponse Int
ugsgrsResponseStatus = lens _ugsgrsResponseStatus (\s a -> s {_ugsgrsResponseStatus = a})

instance NFData UpdateGameServerGroupResponse
