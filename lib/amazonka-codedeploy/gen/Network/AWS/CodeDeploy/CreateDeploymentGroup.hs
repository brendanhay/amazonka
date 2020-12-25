{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment group to which application revisions are deployed.
module Network.AWS.CodeDeploy.CreateDeploymentGroup
  ( -- * Creating a request
    CreateDeploymentGroup (..),
    mkCreateDeploymentGroup,

    -- ** Request lenses
    cdgApplicationName,
    cdgDeploymentGroupName,
    cdgServiceRoleArn,
    cdgAlarmConfiguration,
    cdgAutoRollbackConfiguration,
    cdgAutoScalingGroups,
    cdgBlueGreenDeploymentConfiguration,
    cdgDeploymentConfigName,
    cdgDeploymentStyle,
    cdgEc2TagFilters,
    cdgEc2TagSet,
    cdgEcsServices,
    cdgLoadBalancerInfo,
    cdgOnPremisesInstanceTagFilters,
    cdgOnPremisesTagSet,
    cdgTags,
    cdgTriggerConfigurations,

    -- * Destructuring the response
    CreateDeploymentGroupResponse (..),
    mkCreateDeploymentGroupResponse,

    -- ** Response lenses
    cdgrrsDeploymentGroupId,
    cdgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateDeploymentGroup@ operation.
--
-- /See:/ 'mkCreateDeploymentGroup' smart constructor.
data CreateDeploymentGroup = CreateDeploymentGroup'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Types.ApplicationName,
    -- | The name of a new deployment group for the specified application.
    deploymentGroupName :: Types.DeploymentGroupName,
    -- | A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to act on the user's behalf when interacting with AWS services.
    serviceRoleArn :: Types.ServiceRoleArn,
    -- | Information to add about Amazon CloudWatch alarms when the deployment group is created.
    alarmConfiguration :: Core.Maybe Types.AlarmConfiguration,
    -- | Configuration information for an automatic rollback that is added when a deployment group is created.
    autoRollbackConfiguration :: Core.Maybe Types.AutoRollbackConfiguration,
    -- | A list of associated Amazon EC2 Auto Scaling groups.
    autoScalingGroups :: Core.Maybe [Types.AutoScalingGroupName],
    -- | Information about blue/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Core.Maybe Types.BlueGreenDeploymentConfiguration,
    -- | If specified, the deployment configuration name can be either one of the predefined configurations provided with AWS CodeDeploy or a custom deployment configuration that you create by calling the create deployment configuration operation.
    --
    -- @CodeDeployDefault.OneAtATime@ is the default deployment configuration. It is used if a configuration isn't specified for the deployment or deployment group.
    -- For more information about the predefined deployment configurations in AWS CodeDeploy, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy> in the /AWS CodeDeploy User Guide/ .
    deploymentConfigName :: Core.Maybe Types.DeploymentConfigName,
    -- | Information about the type of deployment, in-place or blue/green, that you want to run and whether to route deployment traffic behind a load balancer.
    deploymentStyle :: Core.Maybe Types.DeploymentStyle,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags. Cannot be used in the same call as ec2TagSet.
    ec2TagFilters :: Core.Maybe [Types.EC2TagFilter],
    -- | Information about groups of tags applied to EC2 instances. The deployment group includes only EC2 instances identified by all the tag groups. Cannot be used in the same call as @ec2TagFilters@ .
    ec2TagSet :: Core.Maybe Types.EC2TagSet,
    -- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
    ecsServices :: Core.Maybe [Types.ECSService],
    -- | Information about the load balancer used in a deployment.
    loadBalancerInfo :: Core.Maybe Types.LoadBalancerInfo,
    -- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags. Cannot be used in the same call as @OnPremisesTagSet@ .
    onPremisesInstanceTagFilters :: Core.Maybe [Types.TagFilter],
    -- | Information about groups of tags applied to on-premises instances. The deployment group includes only on-premises instances identified by all of the tag groups. Cannot be used in the same call as @onPremisesInstanceTagFilters@ .
    onPremisesTagSet :: Core.Maybe Types.OnPremisesTagSet,
    -- | The metadata that you apply to CodeDeploy deployment groups to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
    tags :: Core.Maybe [Types.Tag],
    -- | Information about triggers to create when the deployment group is created. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event> in the /AWS CodeDeploy User Guide/ .
    triggerConfigurations :: Core.Maybe [Types.TriggerConfig]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeploymentGroup' value with any optional fields omitted.
mkCreateDeploymentGroup ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'deploymentGroupName'
  Types.DeploymentGroupName ->
  -- | 'serviceRoleArn'
  Types.ServiceRoleArn ->
  CreateDeploymentGroup
mkCreateDeploymentGroup
  applicationName
  deploymentGroupName
  serviceRoleArn =
    CreateDeploymentGroup'
      { applicationName,
        deploymentGroupName,
        serviceRoleArn,
        alarmConfiguration = Core.Nothing,
        autoRollbackConfiguration = Core.Nothing,
        autoScalingGroups = Core.Nothing,
        blueGreenDeploymentConfiguration = Core.Nothing,
        deploymentConfigName = Core.Nothing,
        deploymentStyle = Core.Nothing,
        ec2TagFilters = Core.Nothing,
        ec2TagSet = Core.Nothing,
        ecsServices = Core.Nothing,
        loadBalancerInfo = Core.Nothing,
        onPremisesInstanceTagFilters = Core.Nothing,
        onPremisesTagSet = Core.Nothing,
        tags = Core.Nothing,
        triggerConfigurations = Core.Nothing
      }

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgApplicationName :: Lens.Lens' CreateDeploymentGroup Types.ApplicationName
cdgApplicationName = Lens.field @"applicationName"
{-# DEPRECATED cdgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of a new deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgDeploymentGroupName :: Lens.Lens' CreateDeploymentGroup Types.DeploymentGroupName
cdgDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# DEPRECATED cdgDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

-- | A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to act on the user's behalf when interacting with AWS services.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgServiceRoleArn :: Lens.Lens' CreateDeploymentGroup Types.ServiceRoleArn
cdgServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED cdgServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | Information to add about Amazon CloudWatch alarms when the deployment group is created.
--
-- /Note:/ Consider using 'alarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAlarmConfiguration :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.AlarmConfiguration)
cdgAlarmConfiguration = Lens.field @"alarmConfiguration"
{-# DEPRECATED cdgAlarmConfiguration "Use generic-lens or generic-optics with 'alarmConfiguration' instead." #-}

-- | Configuration information for an automatic rollback that is added when a deployment group is created.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAutoRollbackConfiguration :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.AutoRollbackConfiguration)
cdgAutoRollbackConfiguration = Lens.field @"autoRollbackConfiguration"
{-# DEPRECATED cdgAutoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead." #-}

-- | A list of associated Amazon EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAutoScalingGroups :: Lens.Lens' CreateDeploymentGroup (Core.Maybe [Types.AutoScalingGroupName])
cdgAutoScalingGroups = Lens.field @"autoScalingGroups"
{-# DEPRECATED cdgAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | Information about blue/green deployment options for a deployment group.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgBlueGreenDeploymentConfiguration :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.BlueGreenDeploymentConfiguration)
cdgBlueGreenDeploymentConfiguration = Lens.field @"blueGreenDeploymentConfiguration"
{-# DEPRECATED cdgBlueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead." #-}

-- | If specified, the deployment configuration name can be either one of the predefined configurations provided with AWS CodeDeploy or a custom deployment configuration that you create by calling the create deployment configuration operation.
--
-- @CodeDeployDefault.OneAtATime@ is the default deployment configuration. It is used if a configuration isn't specified for the deployment or deployment group.
-- For more information about the predefined deployment configurations in AWS CodeDeploy, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgDeploymentConfigName :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.DeploymentConfigName)
cdgDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# DEPRECATED cdgDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

-- | Information about the type of deployment, in-place or blue/green, that you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgDeploymentStyle :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.DeploymentStyle)
cdgDeploymentStyle = Lens.field @"deploymentStyle"
{-# DEPRECATED cdgDeploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead." #-}

-- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags. Cannot be used in the same call as ec2TagSet.
--
-- /Note:/ Consider using 'ec2TagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgEc2TagFilters :: Lens.Lens' CreateDeploymentGroup (Core.Maybe [Types.EC2TagFilter])
cdgEc2TagFilters = Lens.field @"ec2TagFilters"
{-# DEPRECATED cdgEc2TagFilters "Use generic-lens or generic-optics with 'ec2TagFilters' instead." #-}

-- | Information about groups of tags applied to EC2 instances. The deployment group includes only EC2 instances identified by all the tag groups. Cannot be used in the same call as @ec2TagFilters@ .
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgEc2TagSet :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.EC2TagSet)
cdgEc2TagSet = Lens.field @"ec2TagSet"
{-# DEPRECATED cdgEc2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead." #-}

-- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
--
-- /Note:/ Consider using 'ecsServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgEcsServices :: Lens.Lens' CreateDeploymentGroup (Core.Maybe [Types.ECSService])
cdgEcsServices = Lens.field @"ecsServices"
{-# DEPRECATED cdgEcsServices "Use generic-lens or generic-optics with 'ecsServices' instead." #-}

-- | Information about the load balancer used in a deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgLoadBalancerInfo :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.LoadBalancerInfo)
cdgLoadBalancerInfo = Lens.field @"loadBalancerInfo"
{-# DEPRECATED cdgLoadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead." #-}

-- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags. Cannot be used in the same call as @OnPremisesTagSet@ .
--
-- /Note:/ Consider using 'onPremisesInstanceTagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgOnPremisesInstanceTagFilters :: Lens.Lens' CreateDeploymentGroup (Core.Maybe [Types.TagFilter])
cdgOnPremisesInstanceTagFilters = Lens.field @"onPremisesInstanceTagFilters"
{-# DEPRECATED cdgOnPremisesInstanceTagFilters "Use generic-lens or generic-optics with 'onPremisesInstanceTagFilters' instead." #-}

-- | Information about groups of tags applied to on-premises instances. The deployment group includes only on-premises instances identified by all of the tag groups. Cannot be used in the same call as @onPremisesInstanceTagFilters@ .
--
-- /Note:/ Consider using 'onPremisesTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgOnPremisesTagSet :: Lens.Lens' CreateDeploymentGroup (Core.Maybe Types.OnPremisesTagSet)
cdgOnPremisesTagSet = Lens.field @"onPremisesTagSet"
{-# DEPRECATED cdgOnPremisesTagSet "Use generic-lens or generic-optics with 'onPremisesTagSet' instead." #-}

-- | The metadata that you apply to CodeDeploy deployment groups to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgTags :: Lens.Lens' CreateDeploymentGroup (Core.Maybe [Types.Tag])
cdgTags = Lens.field @"tags"
{-# DEPRECATED cdgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Information about triggers to create when the deployment group is created. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'triggerConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgTriggerConfigurations :: Lens.Lens' CreateDeploymentGroup (Core.Maybe [Types.TriggerConfig])
cdgTriggerConfigurations = Lens.field @"triggerConfigurations"
{-# DEPRECATED cdgTriggerConfigurations "Use generic-lens or generic-optics with 'triggerConfigurations' instead." #-}

instance Core.FromJSON CreateDeploymentGroup where
  toJSON CreateDeploymentGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("applicationName" Core..= applicationName),
            Core.Just ("deploymentGroupName" Core..= deploymentGroupName),
            Core.Just ("serviceRoleArn" Core..= serviceRoleArn),
            ("alarmConfiguration" Core..=) Core.<$> alarmConfiguration,
            ("autoRollbackConfiguration" Core..=)
              Core.<$> autoRollbackConfiguration,
            ("autoScalingGroups" Core..=) Core.<$> autoScalingGroups,
            ("blueGreenDeploymentConfiguration" Core..=)
              Core.<$> blueGreenDeploymentConfiguration,
            ("deploymentConfigName" Core..=) Core.<$> deploymentConfigName,
            ("deploymentStyle" Core..=) Core.<$> deploymentStyle,
            ("ec2TagFilters" Core..=) Core.<$> ec2TagFilters,
            ("ec2TagSet" Core..=) Core.<$> ec2TagSet,
            ("ecsServices" Core..=) Core.<$> ecsServices,
            ("loadBalancerInfo" Core..=) Core.<$> loadBalancerInfo,
            ("onPremisesInstanceTagFilters" Core..=)
              Core.<$> onPremisesInstanceTagFilters,
            ("onPremisesTagSet" Core..=) Core.<$> onPremisesTagSet,
            ("tags" Core..=) Core.<$> tags,
            ("triggerConfigurations" Core..=) Core.<$> triggerConfigurations
          ]
      )

instance Core.AWSRequest CreateDeploymentGroup where
  type Rs CreateDeploymentGroup = CreateDeploymentGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.CreateDeploymentGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentGroupResponse'
            Core.<$> (x Core..:? "deploymentGroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @CreateDeploymentGroup@ operation.
--
-- /See:/ 'mkCreateDeploymentGroupResponse' smart constructor.
data CreateDeploymentGroupResponse = CreateDeploymentGroupResponse'
  { -- | A unique deployment group ID.
    deploymentGroupId :: Core.Maybe Types.DeploymentGroupId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeploymentGroupResponse' value with any optional fields omitted.
mkCreateDeploymentGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDeploymentGroupResponse
mkCreateDeploymentGroupResponse responseStatus =
  CreateDeploymentGroupResponse'
    { deploymentGroupId = Core.Nothing,
      responseStatus
    }

-- | A unique deployment group ID.
--
-- /Note:/ Consider using 'deploymentGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgrrsDeploymentGroupId :: Lens.Lens' CreateDeploymentGroupResponse (Core.Maybe Types.DeploymentGroupId)
cdgrrsDeploymentGroupId = Lens.field @"deploymentGroupId"
{-# DEPRECATED cdgrrsDeploymentGroupId "Use generic-lens or generic-optics with 'deploymentGroupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgrrsResponseStatus :: Lens.Lens' CreateDeploymentGroupResponse Core.Int
cdgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
