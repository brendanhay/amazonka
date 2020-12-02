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
-- Module      : Network.AWS.GameLift.CreateGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
--
-- Creates a GameLift FleetIQ game server group for managing game hosting on a collection of Amazon EC2 instances for game hosting. This operation creates the game server group, creates an Auto Scaling group in your AWS account, and establishes a link between the two groups. You can view the status of your game server groups in the GameLift console. Game server group metrics and events are emitted to Amazon CloudWatch.
--
-- Before creating a new game server group, you must have the following:
--
--     * An Amazon EC2 launch template that specifies how to launch Amazon EC2 instances with your game server build. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an Instance from a Launch Template> in the /Amazon EC2 User Guide/ .
--
--     * An IAM role that extends limited access to your AWS account to allow GameLift FleetIQ to create and interact with the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gsg-iam-permissions-roles.html Create IAM roles for cross-service interaction> in the /GameLift FleetIQ Developer Guide/ .
--
--
--
-- To create a new game server group, specify a unique group name, IAM role and Amazon EC2 launch template, and provide a list of instance types that can be used in the group. You must also set initial maximum and minimum limits on the group's instance count. You can optionally set an Auto Scaling policy with target tracking based on a GameLift FleetIQ metric.
--
-- Once the game server group and corresponding Auto Scaling group are created, you have full access to change the Auto Scaling group's configuration as needed. Several properties that are set when creating a game server group, including maximum/minimum size and auto-scaling policy settings, must be updated directly in the Auto Scaling group. Keep in mind that some Auto Scaling group properties are periodically updated by GameLift FleetIQ as part of its balancing activities to optimize for availability and cost.
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
module Network.AWS.GameLift.CreateGameServerGroup
  ( -- * Creating a Request
    createGameServerGroup,
    CreateGameServerGroup,

    -- * Request Lenses
    cgsgVPCSubnets,
    cgsgBalancingStrategy,
    cgsgAutoScalingPolicy,
    cgsgGameServerProtectionPolicy,
    cgsgTags,
    cgsgGameServerGroupName,
    cgsgRoleARN,
    cgsgMinSize,
    cgsgMaxSize,
    cgsgLaunchTemplate,
    cgsgInstanceDefinitions,

    -- * Destructuring the Response
    createGameServerGroupResponse,
    CreateGameServerGroupResponse,

    -- * Response Lenses
    cgsgrsGameServerGroup,
    cgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGameServerGroup' smart constructor.
data CreateGameServerGroup = CreateGameServerGroup'
  { _cgsgVPCSubnets ::
      !(Maybe (List1 Text)),
    _cgsgBalancingStrategy ::
      !(Maybe BalancingStrategy),
    _cgsgAutoScalingPolicy ::
      !(Maybe GameServerGroupAutoScalingPolicy),
    _cgsgGameServerProtectionPolicy ::
      !(Maybe GameServerProtectionPolicy),
    _cgsgTags :: !(Maybe [Tag]),
    _cgsgGameServerGroupName :: !Text,
    _cgsgRoleARN :: !Text,
    _cgsgMinSize :: !Nat,
    _cgsgMaxSize :: !Nat,
    _cgsgLaunchTemplate ::
      !LaunchTemplateSpecification,
    _cgsgInstanceDefinitions ::
      !(List1 InstanceDefinition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGameServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsgVPCSubnets' - A list of virtual private cloud (VPC) subnets to use with instances in the game server group. By default, all GameLift FleetIQ-supported Availability Zones are used. You can use this parameter to specify VPCs that you've set up. This property cannot be updated after the game server group is created, and the corresponding Auto Scaling group will always use the property value that is set with this request, even if the Auto Scaling group is updated directly.
--
-- * 'cgsgBalancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
--
-- * 'cgsgAutoScalingPolicy' - Configuration settings to define a scaling policy for the Auto Scaling group that is optimized for game hosting. The scaling policy uses the metric @"PercentUtilizedGameServers"@ to maintain a buffer of idle game servers that can immediately accommodate new games and players. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- * 'cgsgGameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
--
-- * 'cgsgTags' - A list of labels to assign to the new game server group resource. Tags are developer-defined key-value pairs. Tagging AWS resources is useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags, respectively. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- * 'cgsgGameServerGroupName' - An identifier for the new game server group. This value is used to generate unique ARN identifiers for the EC2 Auto Scaling group and the GameLift FleetIQ game server group. The name must be unique per Region per AWS account.
--
-- * 'cgsgRoleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- * 'cgsgMinSize' - The minimum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale down the group below this minimum. In production, this value should be set to at least 1. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- * 'cgsgMaxSize' - The maximum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale up the group above this maximum. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- * 'cgsgLaunchTemplate' - The EC2 launch template that contains configuration settings and game server code to be deployed to all instances in the game server group. You can specify the template using either the template name or ID. For help with creating a launch template, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ . After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- * 'cgsgInstanceDefinitions' - The EC2 instance types and sizes to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
createGameServerGroup ::
  -- | 'cgsgGameServerGroupName'
  Text ->
  -- | 'cgsgRoleARN'
  Text ->
  -- | 'cgsgMinSize'
  Natural ->
  -- | 'cgsgMaxSize'
  Natural ->
  -- | 'cgsgLaunchTemplate'
  LaunchTemplateSpecification ->
  -- | 'cgsgInstanceDefinitions'
  NonEmpty InstanceDefinition ->
  CreateGameServerGroup
createGameServerGroup
  pGameServerGroupName_
  pRoleARN_
  pMinSize_
  pMaxSize_
  pLaunchTemplate_
  pInstanceDefinitions_ =
    CreateGameServerGroup'
      { _cgsgVPCSubnets = Nothing,
        _cgsgBalancingStrategy = Nothing,
        _cgsgAutoScalingPolicy = Nothing,
        _cgsgGameServerProtectionPolicy = Nothing,
        _cgsgTags = Nothing,
        _cgsgGameServerGroupName = pGameServerGroupName_,
        _cgsgRoleARN = pRoleARN_,
        _cgsgMinSize = _Nat # pMinSize_,
        _cgsgMaxSize = _Nat # pMaxSize_,
        _cgsgLaunchTemplate = pLaunchTemplate_,
        _cgsgInstanceDefinitions = _List1 # pInstanceDefinitions_
      }

-- | A list of virtual private cloud (VPC) subnets to use with instances in the game server group. By default, all GameLift FleetIQ-supported Availability Zones are used. You can use this parameter to specify VPCs that you've set up. This property cannot be updated after the game server group is created, and the corresponding Auto Scaling group will always use the property value that is set with this request, even if the Auto Scaling group is updated directly.
cgsgVPCSubnets :: Lens' CreateGameServerGroup (Maybe (NonEmpty Text))
cgsgVPCSubnets = lens _cgsgVPCSubnets (\s a -> s {_cgsgVPCSubnets = a}) . mapping _List1

-- | Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
cgsgBalancingStrategy :: Lens' CreateGameServerGroup (Maybe BalancingStrategy)
cgsgBalancingStrategy = lens _cgsgBalancingStrategy (\s a -> s {_cgsgBalancingStrategy = a})

-- | Configuration settings to define a scaling policy for the Auto Scaling group that is optimized for game hosting. The scaling policy uses the metric @"PercentUtilizedGameServers"@ to maintain a buffer of idle game servers that can immediately accommodate new games and players. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
cgsgAutoScalingPolicy :: Lens' CreateGameServerGroup (Maybe GameServerGroupAutoScalingPolicy)
cgsgAutoScalingPolicy = lens _cgsgAutoScalingPolicy (\s a -> s {_cgsgAutoScalingPolicy = a})

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
cgsgGameServerProtectionPolicy :: Lens' CreateGameServerGroup (Maybe GameServerProtectionPolicy)
cgsgGameServerProtectionPolicy = lens _cgsgGameServerProtectionPolicy (\s a -> s {_cgsgGameServerProtectionPolicy = a})

-- | A list of labels to assign to the new game server group resource. Tags are developer-defined key-value pairs. Tagging AWS resources is useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags, respectively. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
cgsgTags :: Lens' CreateGameServerGroup [Tag]
cgsgTags = lens _cgsgTags (\s a -> s {_cgsgTags = a}) . _Default . _Coerce

-- | An identifier for the new game server group. This value is used to generate unique ARN identifiers for the EC2 Auto Scaling group and the GameLift FleetIQ game server group. The name must be unique per Region per AWS account.
cgsgGameServerGroupName :: Lens' CreateGameServerGroup Text
cgsgGameServerGroupName = lens _cgsgGameServerGroupName (\s a -> s {_cgsgGameServerGroupName = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
cgsgRoleARN :: Lens' CreateGameServerGroup Text
cgsgRoleARN = lens _cgsgRoleARN (\s a -> s {_cgsgRoleARN = a})

-- | The minimum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale down the group below this minimum. In production, this value should be set to at least 1. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
cgsgMinSize :: Lens' CreateGameServerGroup Natural
cgsgMinSize = lens _cgsgMinSize (\s a -> s {_cgsgMinSize = a}) . _Nat

-- | The maximum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale up the group above this maximum. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
cgsgMaxSize :: Lens' CreateGameServerGroup Natural
cgsgMaxSize = lens _cgsgMaxSize (\s a -> s {_cgsgMaxSize = a}) . _Nat

-- | The EC2 launch template that contains configuration settings and game server code to be deployed to all instances in the game server group. You can specify the template using either the template name or ID. For help with creating a launch template, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ . After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
cgsgLaunchTemplate :: Lens' CreateGameServerGroup LaunchTemplateSpecification
cgsgLaunchTemplate = lens _cgsgLaunchTemplate (\s a -> s {_cgsgLaunchTemplate = a})

-- | The EC2 instance types and sizes to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
cgsgInstanceDefinitions :: Lens' CreateGameServerGroup (NonEmpty InstanceDefinition)
cgsgInstanceDefinitions = lens _cgsgInstanceDefinitions (\s a -> s {_cgsgInstanceDefinitions = a}) . _List1

instance AWSRequest CreateGameServerGroup where
  type Rs CreateGameServerGroup = CreateGameServerGroupResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          CreateGameServerGroupResponse'
            <$> (x .?> "GameServerGroup") <*> (pure (fromEnum s))
      )

instance Hashable CreateGameServerGroup

instance NFData CreateGameServerGroup

instance ToHeaders CreateGameServerGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.CreateGameServerGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateGameServerGroup where
  toJSON CreateGameServerGroup' {..} =
    object
      ( catMaybes
          [ ("VpcSubnets" .=) <$> _cgsgVPCSubnets,
            ("BalancingStrategy" .=) <$> _cgsgBalancingStrategy,
            ("AutoScalingPolicy" .=) <$> _cgsgAutoScalingPolicy,
            ("GameServerProtectionPolicy" .=)
              <$> _cgsgGameServerProtectionPolicy,
            ("Tags" .=) <$> _cgsgTags,
            Just ("GameServerGroupName" .= _cgsgGameServerGroupName),
            Just ("RoleArn" .= _cgsgRoleARN),
            Just ("MinSize" .= _cgsgMinSize),
            Just ("MaxSize" .= _cgsgMaxSize),
            Just ("LaunchTemplate" .= _cgsgLaunchTemplate),
            Just ("InstanceDefinitions" .= _cgsgInstanceDefinitions)
          ]
      )

instance ToPath CreateGameServerGroup where
  toPath = const "/"

instance ToQuery CreateGameServerGroup where
  toQuery = const mempty

-- | /See:/ 'createGameServerGroupResponse' smart constructor.
data CreateGameServerGroupResponse = CreateGameServerGroupResponse'
  { _cgsgrsGameServerGroup ::
      !(Maybe GameServerGroup),
    _cgsgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGameServerGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsgrsGameServerGroup' - The newly created game server group object, including the new ARN value for the GameLift FleetIQ game server group and the object's status. The EC2 Auto Scaling group ARN is initially null, since the group has not yet been created. This value is added once the game server group status reaches @ACTIVE@ .
--
-- * 'cgsgrsResponseStatus' - -- | The response status code.
createGameServerGroupResponse ::
  -- | 'cgsgrsResponseStatus'
  Int ->
  CreateGameServerGroupResponse
createGameServerGroupResponse pResponseStatus_ =
  CreateGameServerGroupResponse'
    { _cgsgrsGameServerGroup = Nothing,
      _cgsgrsResponseStatus = pResponseStatus_
    }

-- | The newly created game server group object, including the new ARN value for the GameLift FleetIQ game server group and the object's status. The EC2 Auto Scaling group ARN is initially null, since the group has not yet been created. This value is added once the game server group status reaches @ACTIVE@ .
cgsgrsGameServerGroup :: Lens' CreateGameServerGroupResponse (Maybe GameServerGroup)
cgsgrsGameServerGroup = lens _cgsgrsGameServerGroup (\s a -> s {_cgsgrsGameServerGroup = a})

-- | -- | The response status code.
cgsgrsResponseStatus :: Lens' CreateGameServerGroupResponse Int
cgsgrsResponseStatus = lens _cgsgrsResponseStatus (\s a -> s {_cgsgrsResponseStatus = a})

instance NFData CreateGameServerGroupResponse
