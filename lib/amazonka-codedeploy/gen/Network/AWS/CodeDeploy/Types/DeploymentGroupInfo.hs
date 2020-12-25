{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
  ( DeploymentGroupInfo (..),

    -- * Smart constructor
    mkDeploymentGroupInfo,

    -- * Lenses
    dgiAlarmConfiguration,
    dgiApplicationName,
    dgiAutoRollbackConfiguration,
    dgiAutoScalingGroups,
    dgiBlueGreenDeploymentConfiguration,
    dgiComputePlatform,
    dgiDeploymentConfigName,
    dgiDeploymentGroupId,
    dgiDeploymentGroupName,
    dgiDeploymentStyle,
    dgiEc2TagFilters,
    dgiEc2TagSet,
    dgiEcsServices,
    dgiLastAttemptedDeployment,
    dgiLastSuccessfulDeployment,
    dgiLoadBalancerInfo,
    dgiOnPremisesInstanceTagFilters,
    dgiOnPremisesTagSet,
    dgiServiceRoleArn,
    dgiTargetRevision,
    dgiTriggerConfigurations,
  )
where

import qualified Network.AWS.CodeDeploy.Types.AlarmConfiguration as Types
import qualified Network.AWS.CodeDeploy.Types.ApplicationName as Types
import qualified Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration as Types
import qualified Network.AWS.CodeDeploy.Types.AutoScalingGroup as Types
import qualified Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration as Types
import qualified Network.AWS.CodeDeploy.Types.ComputePlatform as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentConfigName as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentGroupId as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentGroupName as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentStyle as Types
import qualified Network.AWS.CodeDeploy.Types.EC2TagFilter as Types
import qualified Network.AWS.CodeDeploy.Types.EC2TagSet as Types
import qualified Network.AWS.CodeDeploy.Types.ECSService as Types
import qualified Network.AWS.CodeDeploy.Types.LastDeploymentInfo as Types
import qualified Network.AWS.CodeDeploy.Types.LoadBalancerInfo as Types
import qualified Network.AWS.CodeDeploy.Types.OnPremisesTagSet as Types
import qualified Network.AWS.CodeDeploy.Types.RevisionLocation as Types
import qualified Network.AWS.CodeDeploy.Types.ServiceRoleArn as Types
import qualified Network.AWS.CodeDeploy.Types.TagFilter as Types
import qualified Network.AWS.CodeDeploy.Types.TriggerConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a deployment group.
--
-- /See:/ 'mkDeploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { -- | A list of alarms associated with the deployment group.
    alarmConfiguration :: Core.Maybe Types.AlarmConfiguration,
    -- | The application name.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | Information about the automatic rollback configuration associated with the deployment group.
    autoRollbackConfiguration :: Core.Maybe Types.AutoRollbackConfiguration,
    -- | A list of associated Auto Scaling groups.
    autoScalingGroups :: Core.Maybe [Types.AutoScalingGroup],
    -- | Information about blue/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Core.Maybe Types.BlueGreenDeploymentConfiguration,
    -- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
    computePlatform :: Core.Maybe Types.ComputePlatform,
    -- | The deployment configuration name.
    deploymentConfigName :: Core.Maybe Types.DeploymentConfigName,
    -- | The deployment group ID.
    deploymentGroupId :: Core.Maybe Types.DeploymentGroupId,
    -- | The deployment group name.
    deploymentGroupName :: Core.Maybe Types.DeploymentGroupName,
    -- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
    deploymentStyle :: Core.Maybe Types.DeploymentStyle,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
    ec2TagFilters :: Core.Maybe [Types.EC2TagFilter],
    -- | Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
    ec2TagSet :: Core.Maybe Types.EC2TagSet,
    -- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
    ecsServices :: Core.Maybe [Types.ECSService],
    -- | Information about the most recent attempted deployment to the deployment group.
    lastAttemptedDeployment :: Core.Maybe Types.LastDeploymentInfo,
    -- | Information about the most recent successful deployment to the deployment group.
    lastSuccessfulDeployment :: Core.Maybe Types.LastDeploymentInfo,
    -- | Information about the load balancer to use in a deployment.
    loadBalancerInfo :: Core.Maybe Types.LoadBalancerInfo,
    -- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
    onPremisesInstanceTagFilters :: Core.Maybe [Types.TagFilter],
    -- | Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
    onPremisesTagSet :: Core.Maybe Types.OnPremisesTagSet,
    -- | A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
    serviceRoleArn :: Core.Maybe Types.ServiceRoleArn,
    -- | Information about the deployment group's target revision, including type and location.
    targetRevision :: Core.Maybe Types.RevisionLocation,
    -- | Information about triggers associated with the deployment group.
    triggerConfigurations :: Core.Maybe [Types.TriggerConfig]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeploymentGroupInfo' value with any optional fields omitted.
mkDeploymentGroupInfo ::
  DeploymentGroupInfo
mkDeploymentGroupInfo =
  DeploymentGroupInfo'
    { alarmConfiguration = Core.Nothing,
      applicationName = Core.Nothing,
      autoRollbackConfiguration = Core.Nothing,
      autoScalingGroups = Core.Nothing,
      blueGreenDeploymentConfiguration = Core.Nothing,
      computePlatform = Core.Nothing,
      deploymentConfigName = Core.Nothing,
      deploymentGroupId = Core.Nothing,
      deploymentGroupName = Core.Nothing,
      deploymentStyle = Core.Nothing,
      ec2TagFilters = Core.Nothing,
      ec2TagSet = Core.Nothing,
      ecsServices = Core.Nothing,
      lastAttemptedDeployment = Core.Nothing,
      lastSuccessfulDeployment = Core.Nothing,
      loadBalancerInfo = Core.Nothing,
      onPremisesInstanceTagFilters = Core.Nothing,
      onPremisesTagSet = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      targetRevision = Core.Nothing,
      triggerConfigurations = Core.Nothing
    }

-- | A list of alarms associated with the deployment group.
--
-- /Note:/ Consider using 'alarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAlarmConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.AlarmConfiguration)
dgiAlarmConfiguration = Lens.field @"alarmConfiguration"
{-# DEPRECATED dgiAlarmConfiguration "Use generic-lens or generic-optics with 'alarmConfiguration' instead." #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiApplicationName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.ApplicationName)
dgiApplicationName = Lens.field @"applicationName"
{-# DEPRECATED dgiApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information about the automatic rollback configuration associated with the deployment group.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAutoRollbackConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.AutoRollbackConfiguration)
dgiAutoRollbackConfiguration = Lens.field @"autoRollbackConfiguration"
{-# DEPRECATED dgiAutoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead." #-}

-- | A list of associated Auto Scaling groups.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAutoScalingGroups :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.AutoScalingGroup])
dgiAutoScalingGroups = Lens.field @"autoScalingGroups"
{-# DEPRECATED dgiAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | Information about blue/green deployment options for a deployment group.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiBlueGreenDeploymentConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.BlueGreenDeploymentConfiguration)
dgiBlueGreenDeploymentConfiguration = Lens.field @"blueGreenDeploymentConfiguration"
{-# DEPRECATED dgiBlueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead." #-}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiComputePlatform :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.ComputePlatform)
dgiComputePlatform = Lens.field @"computePlatform"
{-# DEPRECATED dgiComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | The deployment configuration name.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentConfigName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentConfigName)
dgiDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# DEPRECATED dgiDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

-- | The deployment group ID.
--
-- /Note:/ Consider using 'deploymentGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentGroupId :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentGroupId)
dgiDeploymentGroupId = Lens.field @"deploymentGroupId"
{-# DEPRECATED dgiDeploymentGroupId "Use generic-lens or generic-optics with 'deploymentGroupId' instead." #-}

-- | The deployment group name.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentGroupName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentGroupName)
dgiDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# DEPRECATED dgiDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentStyle :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentStyle)
dgiDeploymentStyle = Lens.field @"deploymentStyle"
{-# DEPRECATED dgiDeploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead." #-}

-- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
--
-- /Note:/ Consider using 'ec2TagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEc2TagFilters :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.EC2TagFilter])
dgiEc2TagFilters = Lens.field @"ec2TagFilters"
{-# DEPRECATED dgiEc2TagFilters "Use generic-lens or generic-optics with 'ec2TagFilters' instead." #-}

-- | Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEc2TagSet :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.EC2TagSet)
dgiEc2TagSet = Lens.field @"ec2TagSet"
{-# DEPRECATED dgiEc2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead." #-}

-- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
--
-- /Note:/ Consider using 'ecsServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEcsServices :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.ECSService])
dgiEcsServices = Lens.field @"ecsServices"
{-# DEPRECATED dgiEcsServices "Use generic-lens or generic-optics with 'ecsServices' instead." #-}

-- | Information about the most recent attempted deployment to the deployment group.
--
-- /Note:/ Consider using 'lastAttemptedDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLastAttemptedDeployment :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.LastDeploymentInfo)
dgiLastAttemptedDeployment = Lens.field @"lastAttemptedDeployment"
{-# DEPRECATED dgiLastAttemptedDeployment "Use generic-lens or generic-optics with 'lastAttemptedDeployment' instead." #-}

-- | Information about the most recent successful deployment to the deployment group.
--
-- /Note:/ Consider using 'lastSuccessfulDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLastSuccessfulDeployment :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.LastDeploymentInfo)
dgiLastSuccessfulDeployment = Lens.field @"lastSuccessfulDeployment"
{-# DEPRECATED dgiLastSuccessfulDeployment "Use generic-lens or generic-optics with 'lastSuccessfulDeployment' instead." #-}

-- | Information about the load balancer to use in a deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLoadBalancerInfo :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.LoadBalancerInfo)
dgiLoadBalancerInfo = Lens.field @"loadBalancerInfo"
{-# DEPRECATED dgiLoadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead." #-}

-- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
--
-- /Note:/ Consider using 'onPremisesInstanceTagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiOnPremisesInstanceTagFilters :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.TagFilter])
dgiOnPremisesInstanceTagFilters = Lens.field @"onPremisesInstanceTagFilters"
{-# DEPRECATED dgiOnPremisesInstanceTagFilters "Use generic-lens or generic-optics with 'onPremisesInstanceTagFilters' instead." #-}

-- | Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
--
-- /Note:/ Consider using 'onPremisesTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiOnPremisesTagSet :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.OnPremisesTagSet)
dgiOnPremisesTagSet = Lens.field @"onPremisesTagSet"
{-# DEPRECATED dgiOnPremisesTagSet "Use generic-lens or generic-optics with 'onPremisesTagSet' instead." #-}

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiServiceRoleArn :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.ServiceRoleArn)
dgiServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED dgiServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | Information about the deployment group's target revision, including type and location.
--
-- /Note:/ Consider using 'targetRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiTargetRevision :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.RevisionLocation)
dgiTargetRevision = Lens.field @"targetRevision"
{-# DEPRECATED dgiTargetRevision "Use generic-lens or generic-optics with 'targetRevision' instead." #-}

-- | Information about triggers associated with the deployment group.
--
-- /Note:/ Consider using 'triggerConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiTriggerConfigurations :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.TriggerConfig])
dgiTriggerConfigurations = Lens.field @"triggerConfigurations"
{-# DEPRECATED dgiTriggerConfigurations "Use generic-lens or generic-optics with 'triggerConfigurations' instead." #-}

instance Core.FromJSON DeploymentGroupInfo where
  parseJSON =
    Core.withObject "DeploymentGroupInfo" Core.$
      \x ->
        DeploymentGroupInfo'
          Core.<$> (x Core..:? "alarmConfiguration")
          Core.<*> (x Core..:? "applicationName")
          Core.<*> (x Core..:? "autoRollbackConfiguration")
          Core.<*> (x Core..:? "autoScalingGroups")
          Core.<*> (x Core..:? "blueGreenDeploymentConfiguration")
          Core.<*> (x Core..:? "computePlatform")
          Core.<*> (x Core..:? "deploymentConfigName")
          Core.<*> (x Core..:? "deploymentGroupId")
          Core.<*> (x Core..:? "deploymentGroupName")
          Core.<*> (x Core..:? "deploymentStyle")
          Core.<*> (x Core..:? "ec2TagFilters")
          Core.<*> (x Core..:? "ec2TagSet")
          Core.<*> (x Core..:? "ecsServices")
          Core.<*> (x Core..:? "lastAttemptedDeployment")
          Core.<*> (x Core..:? "lastSuccessfulDeployment")
          Core.<*> (x Core..:? "loadBalancerInfo")
          Core.<*> (x Core..:? "onPremisesInstanceTagFilters")
          Core.<*> (x Core..:? "onPremisesTagSet")
          Core.<*> (x Core..:? "serviceRoleArn")
          Core.<*> (x Core..:? "targetRevision")
          Core.<*> (x Core..:? "triggerConfigurations")
