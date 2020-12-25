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
    cgsgGameServerGroupName,
    cgsgRoleArn,
    cgsgMinSize,
    cgsgMaxSize,
    cgsgLaunchTemplate,
    cgsgInstanceDefinitions,
    cgsgAutoScalingPolicy,
    cgsgBalancingStrategy,
    cgsgGameServerProtectionPolicy,
    cgsgTags,
    cgsgVpcSubnets,

    -- * Destructuring the response
    CreateGameServerGroupResponse (..),
    mkCreateGameServerGroupResponse,

    -- ** Response lenses
    cgsgrrsGameServerGroup,
    cgsgrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGameServerGroup' smart constructor.
data CreateGameServerGroup = CreateGameServerGroup'
  { -- | An identifier for the new game server group. This value is used to generate unique ARN identifiers for the EC2 Auto Scaling group and the GameLift FleetIQ game server group. The name must be unique per Region per AWS account.
    gameServerGroupName :: Types.GameServerGroupName,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
    roleArn :: Types.IamRoleArn,
    -- | The minimum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale down the group below this minimum. In production, this value should be set to at least 1. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    minSize :: Core.Natural,
    -- | The maximum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale up the group above this maximum. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    maxSize :: Core.Natural,
    -- | The EC2 launch template that contains configuration settings and game server code to be deployed to all instances in the game server group. You can specify the template using either the template name or ID. For help with creating a launch template, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ . After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    launchTemplate :: Types.LaunchTemplateSpecification,
    -- | The EC2 instance types and sizes to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
    instanceDefinitions :: Core.NonEmpty Types.InstanceDefinition,
    -- | Configuration settings to define a scaling policy for the Auto Scaling group that is optimized for game hosting. The scaling policy uses the metric @"PercentUtilizedGameServers"@ to maintain a buffer of idle game servers that can immediately accommodate new games and players. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
    autoScalingPolicy :: Core.Maybe Types.GameServerGroupAutoScalingPolicy,
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
    balancingStrategy :: Core.Maybe Types.BalancingStrategy,
    -- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
    gameServerProtectionPolicy :: Core.Maybe Types.GameServerProtectionPolicy,
    -- | A list of labels to assign to the new game server group resource. Tags are developer-defined key-value pairs. Tagging AWS resources is useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags, respectively. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Core.Maybe [Types.Tag],
    -- | A list of virtual private cloud (VPC) subnets to use with instances in the game server group. By default, all GameLift FleetIQ-supported Availability Zones are used. You can use this parameter to specify VPCs that you've set up. This property cannot be updated after the game server group is created, and the corresponding Auto Scaling group will always use the property value that is set with this request, even if the Auto Scaling group is updated directly.
    vpcSubnets :: Core.Maybe (Core.NonEmpty Types.VpcSubnet)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGameServerGroup' value with any optional fields omitted.
mkCreateGameServerGroup ::
  -- | 'gameServerGroupName'
  Types.GameServerGroupName ->
  -- | 'roleArn'
  Types.IamRoleArn ->
  -- | 'minSize'
  Core.Natural ->
  -- | 'maxSize'
  Core.Natural ->
  -- | 'launchTemplate'
  Types.LaunchTemplateSpecification ->
  -- | 'instanceDefinitions'
  Core.NonEmpty Types.InstanceDefinition ->
  CreateGameServerGroup
mkCreateGameServerGroup
  gameServerGroupName
  roleArn
  minSize
  maxSize
  launchTemplate
  instanceDefinitions =
    CreateGameServerGroup'
      { gameServerGroupName,
        roleArn,
        minSize,
        maxSize,
        launchTemplate,
        instanceDefinitions,
        autoScalingPolicy = Core.Nothing,
        balancingStrategy = Core.Nothing,
        gameServerProtectionPolicy = Core.Nothing,
        tags = Core.Nothing,
        vpcSubnets = Core.Nothing
      }

-- | An identifier for the new game server group. This value is used to generate unique ARN identifiers for the EC2 Auto Scaling group and the GameLift FleetIQ game server group. The name must be unique per Region per AWS account.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgGameServerGroupName :: Lens.Lens' CreateGameServerGroup Types.GameServerGroupName
cgsgGameServerGroupName = Lens.field @"gameServerGroupName"
{-# DEPRECATED cgsgGameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgRoleArn :: Lens.Lens' CreateGameServerGroup Types.IamRoleArn
cgsgRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cgsgRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The minimum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale down the group below this minimum. In production, this value should be set to at least 1. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgMinSize :: Lens.Lens' CreateGameServerGroup Core.Natural
cgsgMinSize = Lens.field @"minSize"
{-# DEPRECATED cgsgMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The maximum number of instances allowed in the EC2 Auto Scaling group. During automatic scaling events, GameLift FleetIQ and EC2 do not scale up the group above this maximum. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgMaxSize :: Lens.Lens' CreateGameServerGroup Core.Natural
cgsgMaxSize = Lens.field @"maxSize"
{-# DEPRECATED cgsgMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The EC2 launch template that contains configuration settings and game server code to be deployed to all instances in the game server group. You can specify the template using either the template name or ID. For help with creating a launch template, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a Launch Template for an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ . After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgLaunchTemplate :: Lens.Lens' CreateGameServerGroup Types.LaunchTemplateSpecification
cgsgLaunchTemplate = Lens.field @"launchTemplate"
{-# DEPRECATED cgsgLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The EC2 instance types and sizes to use in the Auto Scaling group. The instance definitions must specify at least two different instance types that are supported by GameLift FleetIQ. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ . You can optionally specify capacity weighting for each instance type. If no weight value is specified for an instance type, it is set to the default value "1". For more information about capacity weighting, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the Amazon EC2 Auto Scaling User Guide.
--
-- /Note:/ Consider using 'instanceDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgInstanceDefinitions :: Lens.Lens' CreateGameServerGroup (Core.NonEmpty Types.InstanceDefinition)
cgsgInstanceDefinitions = Lens.field @"instanceDefinitions"
{-# DEPRECATED cgsgInstanceDefinitions "Use generic-lens or generic-optics with 'instanceDefinitions' instead." #-}

-- | Configuration settings to define a scaling policy for the Auto Scaling group that is optimized for game hosting. The scaling policy uses the metric @"PercentUtilizedGameServers"@ to maintain a buffer of idle game servers that can immediately accommodate new games and players. After the Auto Scaling group is created, update this value directly in the Auto Scaling group using the AWS console or APIs.
--
-- /Note:/ Consider using 'autoScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgAutoScalingPolicy :: Lens.Lens' CreateGameServerGroup (Core.Maybe Types.GameServerGroupAutoScalingPolicy)
cgsgAutoScalingPolicy = Lens.field @"autoScalingPolicy"
{-# DEPRECATED cgsgAutoScalingPolicy "Use generic-lens or generic-optics with 'autoScalingPolicy' instead." #-}

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
cgsgBalancingStrategy :: Lens.Lens' CreateGameServerGroup (Core.Maybe Types.BalancingStrategy)
cgsgBalancingStrategy = Lens.field @"balancingStrategy"
{-# DEPRECATED cgsgBalancingStrategy "Use generic-lens or generic-optics with 'balancingStrategy' instead." #-}

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running might be terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running except in the event of a forced game server group deletion (see ). An exception to this is with Spot Instances, which can be terminated by AWS regardless of protection status. This property is set to @NO_PROTECTION@ by default.
--
-- /Note:/ Consider using 'gameServerProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgGameServerProtectionPolicy :: Lens.Lens' CreateGameServerGroup (Core.Maybe Types.GameServerProtectionPolicy)
cgsgGameServerProtectionPolicy = Lens.field @"gameServerProtectionPolicy"
{-# DEPRECATED cgsgGameServerProtectionPolicy "Use generic-lens or generic-optics with 'gameServerProtectionPolicy' instead." #-}

-- | A list of labels to assign to the new game server group resource. Tags are developer-defined key-value pairs. Tagging AWS resources is useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags, respectively. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgTags :: Lens.Lens' CreateGameServerGroup (Core.Maybe [Types.Tag])
cgsgTags = Lens.field @"tags"
{-# DEPRECATED cgsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of virtual private cloud (VPC) subnets to use with instances in the game server group. By default, all GameLift FleetIQ-supported Availability Zones are used. You can use this parameter to specify VPCs that you've set up. This property cannot be updated after the game server group is created, and the corresponding Auto Scaling group will always use the property value that is set with this request, even if the Auto Scaling group is updated directly.
--
-- /Note:/ Consider using 'vpcSubnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgVpcSubnets :: Lens.Lens' CreateGameServerGroup (Core.Maybe (Core.NonEmpty Types.VpcSubnet))
cgsgVpcSubnets = Lens.field @"vpcSubnets"
{-# DEPRECATED cgsgVpcSubnets "Use generic-lens or generic-optics with 'vpcSubnets' instead." #-}

instance Core.FromJSON CreateGameServerGroup where
  toJSON CreateGameServerGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
            Core.Just ("RoleArn" Core..= roleArn),
            Core.Just ("MinSize" Core..= minSize),
            Core.Just ("MaxSize" Core..= maxSize),
            Core.Just ("LaunchTemplate" Core..= launchTemplate),
            Core.Just ("InstanceDefinitions" Core..= instanceDefinitions),
            ("AutoScalingPolicy" Core..=) Core.<$> autoScalingPolicy,
            ("BalancingStrategy" Core..=) Core.<$> balancingStrategy,
            ("GameServerProtectionPolicy" Core..=)
              Core.<$> gameServerProtectionPolicy,
            ("Tags" Core..=) Core.<$> tags,
            ("VpcSubnets" Core..=) Core.<$> vpcSubnets
          ]
      )

instance Core.AWSRequest CreateGameServerGroup where
  type Rs CreateGameServerGroup = CreateGameServerGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.CreateGameServerGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGameServerGroupResponse'
            Core.<$> (x Core..:? "GameServerGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGameServerGroupResponse' smart constructor.
data CreateGameServerGroupResponse = CreateGameServerGroupResponse'
  { -- | The newly created game server group object, including the new ARN value for the GameLift FleetIQ game server group and the object's status. The EC2 Auto Scaling group ARN is initially null, since the group has not yet been created. This value is added once the game server group status reaches @ACTIVE@ .
    gameServerGroup :: Core.Maybe Types.GameServerGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateGameServerGroupResponse' value with any optional fields omitted.
mkCreateGameServerGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGameServerGroupResponse
mkCreateGameServerGroupResponse responseStatus =
  CreateGameServerGroupResponse'
    { gameServerGroup = Core.Nothing,
      responseStatus
    }

-- | The newly created game server group object, including the new ARN value for the GameLift FleetIQ game server group and the object's status. The EC2 Auto Scaling group ARN is initially null, since the group has not yet been created. This value is added once the game server group status reaches @ACTIVE@ .
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgrrsGameServerGroup :: Lens.Lens' CreateGameServerGroupResponse (Core.Maybe Types.GameServerGroup)
cgsgrrsGameServerGroup = Lens.field @"gameServerGroup"
{-# DEPRECATED cgsgrrsGameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsgrrsResponseStatus :: Lens.Lens' CreateGameServerGroupResponse Core.Int
cgsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
