{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Creates a GameLift FleetIQ game server group for managing game hosting on a collection of Amazon EC2 instances for game hosting. This operation creates the game server group, creates an Auto Scaling group in your AWS account, and establishes a link between the two groups. You can view the status of your game server groups in the GameLift console. Game server group metrics and events are emitted to Amazon CloudWatch.
-- Before creating a new game server group, you must have the following:
--
--     * An Amazon EC2 launch template that specifies how to launch Amazon EC2 instances with your game server build. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an Instance from a Launch Template> in the /Amazon EC2 User Guide/ .
--
--
--     * An IAM role that extends limited access to your AWS account to allow GameLift FleetIQ to create and interact with the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gsg-iam-permissions-roles.html Create IAM roles for cross-service interaction> in the /GameLift FleetIQ Developer Guide/ .
--
--
-- To create a new game server group, specify a unique group name, IAM role and Amazon EC2 launch template, and provide a list of instance types that can be used in the group. You must also set initial maximum and minimum limits on the group's instance count. You can optionally set an Auto Scaling policy with target tracking based on a GameLift FleetIQ metric.
-- Once the game server group and corresponding Auto Scaling group are created, you have full access to change the Auto Scaling group's configuration as needed. Several properties that are set when creating a game server group, including maximum/minimum size and auto-scaling policy settings, must be updated directly in the Auto Scaling group. Keep in mind that some Auto Scaling group properties are periodically updated by GameLift FleetIQ as part of its balancing activities to optimize for availability and cost.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
-- __Related operations__
--
--     * 'CreateGameServerGroup'
--
--
--     * 'ListGameServerGroups'
--
--
--     * 'DescribeGameServerGroup'
--
--
--     * 'UpdateGameServerGroup'
--
--
--     * 'DeleteGameServerGroup'
--
--
--     * 'ResumeGameServerGroup'
--
--
--     * 'SuspendGameServerGroup'
--
--
--     * 'DescribeGameServerInstances'
module Network.AWS.GameLift.CreateGameServerGroup
  ( -- * Creating a request
    CreateGameServerGroup (..),
    mkCreateGameServerGroup,

    -- ** Request lenses
    cgsgInstanceDefinitions,
    cgsgVPCSubnets,
    cgsgBalancingStrategy,
    cgsgGameServerGroupName,
    cgsgMaxSize,
    cgsgMinSize,
    cgsgAutoScalingPolicy,
    cgsgLaunchTemplate,
    cgsgGameServerProtectionPolicy,
    cgsgTags,
    cgsgRoleARN,

    -- * Destructuring the response
    CreateGameServerGroupResponse (..),
    mkCreateGameServerGroupResponse,

    -- ** Response lenses
    cgsgrsGameServerGroup,
    cgsgrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGameServerGroup' smart constructor.
data CreateGameServerGroup = CreateGameServerGroup'
  { -- | The EC2 instance types and sizes to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
    instanceDefinitions :: Lude.NonEmpty InstanceDefinition,
    -- | A list of virtual private cloud (VPC) subnets to use with instances in the game server group. By default, all GameLift FleetIQ-supported Availability Zones are used. You can use this parameter to specify VPCs that you've set up. This property cannot be updated after the game server group is created, and the corresponding Auto Scaling group will always use the property value that is set with this request, even if the Auto Scaling group is updated directly.
    vpcSubnets :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:
    --
    --
    --     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.
    --
    --
    --     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.
    --
    --
    --     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
    balancingStrategy :: Lude.Maybe BalancingStrategy,
    -- | An identifier for the new game server group. This value is used to generate unique ARN identifiers for the EC2 Auto Scaling group and the GameLift FleetIQ game server group. The name must be unique per Region per AWS account.
    gameServerGroupName :: Lude.Text,
    -- | The maximum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale up the group above this maximum. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    maxSize :: Lude.Natural,
    -- | The minimum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale down the group below this minimum. In production, this value should be set to at least 1. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    minSize :: Lude.Natural,
    -- | Configuration settings to define a scaling policy for the Auto Scaling group that is optimized for game hosting. The scaling policy uses the metric @"PercentUtilizedGameServers"@ to maintain a buffer of idle game servers that can immediately accommodate new games and players. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    autoScalingPolicy :: Lude.Maybe GameServerGroupAutoScalingPolicy,
    -- | The EC2 launch template that contains configuration settings and game server code to be deployed to all instances in the game server group. You can specify the template using either the template name or ID. For help with creating a launch template, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ . After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    launchTemplate :: LaunchTemplateSpecification,
    -- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
    gameServerProtectionPolicy :: Lude.Maybe GameServerProtectionPolicy,
    -- | A list of labels to assign to the new game server group resource. Tags are developer-defined key-value pairs. Tagging AWS resources is useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags, respectively. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGameServerGroup' with the minimum fields required to make a request.
--
-- * 'instanceDefinitions' - The EC2 instance types and sizes to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
-- * 'vpcSubnets' - A list of virtual private cloud (VPC) subnets to use with instances in the game server group. By default, all GameLift FleetIQ-supported Availability Zones are used. You can use this parameter to specify VPCs that you've set up. This property cannot be updated after the game server group is created, and the corresponding Auto Scaling group will always use the property value that is set with this request, even if the Auto Scaling group is updated directly.
-- * 'balancingStrategy' - Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:
--
--
--     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.
--
--
--     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.
--
--
--     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
--
--
-- * 'gameServerGroupName' - An identifier for the new game server group. This value is used to generate unique ARN identifiers for the EC2 Auto Scaling group and the GameLift FleetIQ game server group. The name must be unique per Region per AWS account.
-- * 'maxSize' - The maximum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale up the group above this maximum. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
-- * 'minSize' - The minimum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale down the group below this minimum. In production, this value should be set to at least 1. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
-- * 'autoScalingPolicy' - Configuration settings to define a scaling policy for the Auto Scaling group that is optimized for game hosting. The scaling policy uses the metric @"PercentUtilizedGameServers"@ to maintain a buffer of idle game servers that can immediately accommodate new games and players. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
-- * 'launchTemplate' - The EC2 launch template that contains configuration settings and game server code to be deployed to all instances in the game server group. You can specify the template using either the template name or ID. For help with creating a launch template, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ . After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
-- * 'gameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
-- * 'tags' - A list of labels to assign to the new game server group resource. Tags are developer-defined key-value pairs. Tagging AWS resources is useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags, respectively. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
-- * 'roleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
mkCreateGameServerGroup ::
  -- | 'instanceDefinitions'
  Lude.NonEmpty InstanceDefinition ->
  -- | 'gameServerGroupName'
  Lude.Text ->
  -- | 'maxSize'
  Lude.Natural ->
  -- | 'minSize'
  Lude.Natural ->
  -- | 'launchTemplate'
  LaunchTemplateSpecification ->
  -- | 'roleARN'
  Lude.Text ->
  CreateGameServerGroup
mkCreateGameServerGroup
  pInstanceDefinitions_
  pGameServerGroupName_
  pMaxSize_
  pMinSize_
  pLaunchTemplate_
  pRoleARN_ =
    CreateGameServerGroup'
      { instanceDefinitions =
          pInstanceDefinitions_,
        vpcSubnets = Lude.Nothing,
        balancingStrategy = Lude.Nothing,
        gameServerGroupName = pGameServerGroupName_,
        maxSize = pMaxSize_,
        minSize = pMinSize_,
        autoScalingPolicy = Lude.Nothing,
        launchTemplate = pLaunchTemplate_,
        gameServerProtectionPolicy = Lude.Nothing,
        tags = Lude.Nothing,
        roleARN = pRoleARN_
      }

-- | The EC2 instance types and sizes to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
--
-- /Note:/ Consider using 'instanceDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgInstanceDefinitions :: Lens.Lens' CreateGameServerGroup (Lude.NonEmpty InstanceDefinition)
cgsgInstanceDefinitions = Lens.lens (instanceDefinitions :: CreateGameServerGroup -> Lude.NonEmpty InstanceDefinition) (\s a -> s {instanceDefinitions = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgInstanceDefinitions "Use generic-lens or generic-optics with 'instanceDefinitions' instead." #-}

-- | A list of virtual private cloud (VPC) subnets to use with instances in the game server group. By default, all GameLift FleetIQ-supported Availability Zones are used. You can use this parameter to specify VPCs that you've set up. This property cannot be updated after the game server group is created, and the corresponding Auto Scaling group will always use the property value that is set with this request, even if the Auto Scaling group is updated directly.
--
-- /Note:/ Consider using 'vpcSubnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgVPCSubnets :: Lens.Lens' CreateGameServerGroup (Lude.Maybe (Lude.NonEmpty Lude.Text))
cgsgVPCSubnets = Lens.lens (vpcSubnets :: CreateGameServerGroup -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {vpcSubnets = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgVPCSubnets "Use generic-lens or generic-optics with 'vpcSubnets' instead." #-}

-- | Indicates how GameLift FleetIQ balances the use of Spot Instances and On-Demand Instances in the game server group. Method options include the following:
--
--
--     * @SPOT_ONLY@ - Only Spot Instances are used in the game server group. If Spot Instances are unavailable or not viable for game hosting, the game server group provides no hosting capacity until Spot Instances can again be used. Until then, no new instances are started, and the existing nonviable Spot Instances are terminated (after current gameplay ends) and are not replaced.
--
--
--     * @SPOT_PREFERRED@ - (default value) Spot Instances are used whenever available in the game server group. If Spot Instances are unavailable, the game server group continues to provide hosting capacity by falling back to On-Demand Instances. Existing nonviable Spot Instances are terminated (after current gameplay ends) and are replaced with new On-Demand Instances.
--
--
--     * @ON_DEMAND_ONLY@ - Only On-Demand Instances are used in the game server group. No Spot Instances are used, even when available, while this balancing strategy is in force.
--
--
--
-- /Note:/ Consider using 'balancingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgBalancingStrategy :: Lens.Lens' CreateGameServerGroup (Lude.Maybe BalancingStrategy)
cgsgBalancingStrategy = Lens.lens (balancingStrategy :: CreateGameServerGroup -> Lude.Maybe BalancingStrategy) (\s a -> s {balancingStrategy = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgBalancingStrategy "Use generic-lens or generic-optics with 'balancingStrategy' instead." #-}

-- | An identifier for the new game server group. This value is used to generate unique ARN identifiers for the EC2 Auto Scaling group and the GameLift FleetIQ game server group. The name must be unique per Region per AWS account.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgGameServerGroupName :: Lens.Lens' CreateGameServerGroup Lude.Text
cgsgGameServerGroupName = Lens.lens (gameServerGroupName :: CreateGameServerGroup -> Lude.Text) (\s a -> s {gameServerGroupName = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The maximum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale up the group above this maximum. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgMaxSize :: Lens.Lens' CreateGameServerGroup Lude.Natural
cgsgMaxSize = Lens.lens (maxSize :: CreateGameServerGroup -> Lude.Natural) (\s a -> s {maxSize = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The minimum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale down the group below this minimum. In production, this value should be set to at least 1. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgMinSize :: Lens.Lens' CreateGameServerGroup Lude.Natural
cgsgMinSize = Lens.lens (minSize :: CreateGameServerGroup -> Lude.Natural) (\s a -> s {minSize = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | Configuration settings to define a scaling policy for the Auto Scaling group that is optimized for game hosting. The scaling policy uses the metric @"PercentUtilizedGameServers"@ to maintain a buffer of idle game servers that can immediately accommodate new games and players. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgAutoScalingPolicy :: Lens.Lens' CreateGameServerGroup (Lude.Maybe GameServerGroupAutoScalingPolicy)
cgsgAutoScalingPolicy = Lens.lens (autoScalingPolicy :: CreateGameServerGroup -> Lude.Maybe GameServerGroupAutoScalingPolicy) (\s a -> s {autoScalingPolicy = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

-- | The EC2 launch template that contains configuration settings and game server code to be deployed to all instances in the game server group. You can specify the template using either the template name or ID. For help with creating a launch template, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ . After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgLaunchTemplate :: Lens.Lens' CreateGameServerGroup LaunchTemplateSpecification
cgsgLaunchTemplate = Lens.lens (launchTemplate :: CreateGameServerGroup -> LaunchTemplateSpecification) (\s a -> s {launchTemplate = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
--
-- /Note:/ Consider using 'gameServerProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgGameServerProtectionPolicy :: Lens.Lens' CreateGameServerGroup (Lude.Maybe GameServerProtectionPolicy)
cgsgGameServerProtectionPolicy = Lens.lens (gameServerProtectionPolicy :: CreateGameServerGroup -> Lude.Maybe GameServerProtectionPolicy) (\s a -> s {gameServerProtectionPolicy = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgGameServerProtectionPolicy "Use generic-lens or generic-optics with 'gameServerProtectionPolicy' instead." #-}

-- | A list of labels to assign to the new game server group resource. Tags are developer-defined key-value pairs. Tagging AWS resources is useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags, respectively. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgTags :: Lens.Lens' CreateGameServerGroup (Lude.Maybe [Tag])
cgsgTags = Lens.lens (tags :: CreateGameServerGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgRoleARN :: Lens.Lens' CreateGameServerGroup Lude.Text
cgsgRoleARN = Lens.lens (roleARN :: CreateGameServerGroup -> Lude.Text) (\s a -> s {roleARN = a} :: CreateGameServerGroup)
{-# DEPRECATED cgsgRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateGameServerGroup where
  type Rs CreateGameServerGroup = CreateGameServerGroupResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGameServerGroupResponse'
            Lude.<$> (x Lude..?> "GameServerGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGameServerGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateGameServerGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGameServerGroup where
  toJSON CreateGameServerGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceDefinitions" Lude..= instanceDefinitions),
            ("VpcSubnets" Lude..=) Lude.<$> vpcSubnets,
            ("BalancingStrategy" Lude..=) Lude.<$> balancingStrategy,
            Lude.Just ("GameServerGroupName" Lude..= gameServerGroupName),
            Lude.Just ("MaxSize" Lude..= maxSize),
            Lude.Just ("MinSize" Lude..= minSize),
            ("AutoScalingPolicy" Lude..=) Lude.<$> autoScalingPolicy,
            Lude.Just ("LaunchTemplate" Lude..= launchTemplate),
            ("GameServerProtectionPolicy" Lude..=)
              Lude.<$> gameServerProtectionPolicy,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateGameServerGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGameServerGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGameServerGroupResponse' smart constructor.
data CreateGameServerGroupResponse = CreateGameServerGroupResponse'
  { -- | The newly created game server group object, including the new ARN value for the GameLift FleetIQ game server group and the object's status. The EC2 Auto Scaling group ARN is initially null, since the group has not yet been created. This value is added once the game server group status reaches @ACTIVE@ .
    gameServerGroup :: Lude.Maybe GameServerGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGameServerGroupResponse' with the minimum fields required to make a request.
--
-- * 'gameServerGroup' - The newly created game server group object, including the new ARN value for the GameLift FleetIQ game server group and the object's status. The EC2 Auto Scaling group ARN is initially null, since the group has not yet been created. This value is added once the game server group status reaches @ACTIVE@ .
-- * 'responseStatus' - The response status code.
mkCreateGameServerGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGameServerGroupResponse
mkCreateGameServerGroupResponse pResponseStatus_ =
  CreateGameServerGroupResponse'
    { gameServerGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created game server group object, including the new ARN value for the GameLift FleetIQ game server group and the object's status. The EC2 Auto Scaling group ARN is initially null, since the group has not yet been created. This value is added once the game server group status reaches @ACTIVE@ .
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgrsGameServerGroup :: Lens.Lens' CreateGameServerGroupResponse (Lude.Maybe GameServerGroup)
cgsgrsGameServerGroup = Lens.lens (gameServerGroup :: CreateGameServerGroupResponse -> Lude.Maybe GameServerGroup) (\s a -> s {gameServerGroup = a} :: CreateGameServerGroupResponse)
{-# DEPRECATED cgsgrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgrsResponseStatus :: Lens.Lens' CreateGameServerGroupResponse Lude.Int
cgsgrsResponseStatus = Lens.lens (responseStatus :: CreateGameServerGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGameServerGroupResponse)
{-# DEPRECATED cgsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
