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
    dgiServiceRoleARN,
    dgiEc2TagSet,
    dgiDeploymentConfigName,
    dgiLastAttemptedDeployment,
    dgiOnPremisesTagSet,
    dgiComputePlatform,
    dgiTargetRevision,
    dgiEc2TagFilters,
    dgiEcsServices,
    dgiBlueGreenDeploymentConfiguration,
    dgiLoadBalancerInfo,
    dgiOnPremisesInstanceTagFilters,
    dgiLastSuccessfulDeployment,
    dgiApplicationName,
    dgiAlarmConfiguration,
    dgiTriggerConfigurations,
    dgiDeploymentGroupId,
    dgiAutoScalingGroups,
    dgiDeploymentStyle,
    dgiAutoRollbackConfiguration,
    dgiDeploymentGroupName,
  )
where

import Network.AWS.CodeDeploy.Types.AlarmConfiguration
import Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
import Network.AWS.CodeDeploy.Types.AutoScalingGroup
import Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Network.AWS.CodeDeploy.Types.ComputePlatform
import Network.AWS.CodeDeploy.Types.DeploymentStyle
import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagSet
import Network.AWS.CodeDeploy.Types.ECSService
import Network.AWS.CodeDeploy.Types.LastDeploymentInfo
import Network.AWS.CodeDeploy.Types.LoadBalancerInfo
import Network.AWS.CodeDeploy.Types.OnPremisesTagSet
import Network.AWS.CodeDeploy.Types.RevisionLocation
import Network.AWS.CodeDeploy.Types.TagFilter
import Network.AWS.CodeDeploy.Types.TriggerConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a deployment group.
--
-- /See:/ 'mkDeploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { -- | A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
    serviceRoleARN :: Lude.Maybe Lude.Text,
    -- | Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
    ec2TagSet :: Lude.Maybe EC2TagSet,
    -- | The deployment configuration name.
    deploymentConfigName :: Lude.Maybe Lude.Text,
    -- | Information about the most recent attempted deployment to the deployment group.
    lastAttemptedDeployment :: Lude.Maybe LastDeploymentInfo,
    -- | Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
    onPremisesTagSet :: Lude.Maybe OnPremisesTagSet,
    -- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
    computePlatform :: Lude.Maybe ComputePlatform,
    -- | Information about the deployment group's target revision, including type and location.
    targetRevision :: Lude.Maybe RevisionLocation,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
    ec2TagFilters :: Lude.Maybe [EC2TagFilter],
    -- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
    ecsServices :: Lude.Maybe [ECSService],
    -- | Information about blue/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Lude.Maybe BlueGreenDeploymentConfiguration,
    -- | Information about the load balancer to use in a deployment.
    loadBalancerInfo :: Lude.Maybe LoadBalancerInfo,
    -- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
    onPremisesInstanceTagFilters :: Lude.Maybe [TagFilter],
    -- | Information about the most recent successful deployment to the deployment group.
    lastSuccessfulDeployment :: Lude.Maybe LastDeploymentInfo,
    -- | The application name.
    applicationName :: Lude.Maybe Lude.Text,
    -- | A list of alarms associated with the deployment group.
    alarmConfiguration :: Lude.Maybe AlarmConfiguration,
    -- | Information about triggers associated with the deployment group.
    triggerConfigurations :: Lude.Maybe [TriggerConfig],
    -- | The deployment group ID.
    deploymentGroupId :: Lude.Maybe Lude.Text,
    -- | A list of associated Auto Scaling groups.
    autoScalingGroups :: Lude.Maybe [AutoScalingGroup],
    -- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
    deploymentStyle :: Lude.Maybe DeploymentStyle,
    -- | Information about the automatic rollback configuration associated with the deployment group.
    autoRollbackConfiguration :: Lude.Maybe AutoRollbackConfiguration,
    -- | The deployment group name.
    deploymentGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentGroupInfo' with the minimum fields required to make a request.
--
-- * 'serviceRoleARN' - A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
-- * 'ec2TagSet' - Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
-- * 'deploymentConfigName' - The deployment configuration name.
-- * 'lastAttemptedDeployment' - Information about the most recent attempted deployment to the deployment group.
-- * 'onPremisesTagSet' - Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
-- * 'computePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
-- * 'targetRevision' - Information about the deployment group's target revision, including type and location.
-- * 'ec2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
-- * 'ecsServices' - The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
-- * 'blueGreenDeploymentConfiguration' - Information about blue/green deployment options for a deployment group.
-- * 'loadBalancerInfo' - Information about the load balancer to use in a deployment.
-- * 'onPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
-- * 'lastSuccessfulDeployment' - Information about the most recent successful deployment to the deployment group.
-- * 'applicationName' - The application name.
-- * 'alarmConfiguration' - A list of alarms associated with the deployment group.
-- * 'triggerConfigurations' - Information about triggers associated with the deployment group.
-- * 'deploymentGroupId' - The deployment group ID.
-- * 'autoScalingGroups' - A list of associated Auto Scaling groups.
-- * 'deploymentStyle' - Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
-- * 'autoRollbackConfiguration' - Information about the automatic rollback configuration associated with the deployment group.
-- * 'deploymentGroupName' - The deployment group name.
mkDeploymentGroupInfo ::
  DeploymentGroupInfo
mkDeploymentGroupInfo =
  DeploymentGroupInfo'
    { serviceRoleARN = Lude.Nothing,
      ec2TagSet = Lude.Nothing,
      deploymentConfigName = Lude.Nothing,
      lastAttemptedDeployment = Lude.Nothing,
      onPremisesTagSet = Lude.Nothing,
      computePlatform = Lude.Nothing,
      targetRevision = Lude.Nothing,
      ec2TagFilters = Lude.Nothing,
      ecsServices = Lude.Nothing,
      blueGreenDeploymentConfiguration = Lude.Nothing,
      loadBalancerInfo = Lude.Nothing,
      onPremisesInstanceTagFilters = Lude.Nothing,
      lastSuccessfulDeployment = Lude.Nothing,
      applicationName = Lude.Nothing,
      alarmConfiguration = Lude.Nothing,
      triggerConfigurations = Lude.Nothing,
      deploymentGroupId = Lude.Nothing,
      autoScalingGroups = Lude.Nothing,
      deploymentStyle = Lude.Nothing,
      autoRollbackConfiguration = Lude.Nothing,
      deploymentGroupName = Lude.Nothing
    }

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiServiceRoleARN :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe Lude.Text)
dgiServiceRoleARN = Lens.lens (serviceRoleARN :: DeploymentGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEc2TagSet :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe EC2TagSet)
dgiEc2TagSet = Lens.lens (ec2TagSet :: DeploymentGroupInfo -> Lude.Maybe EC2TagSet) (\s a -> s {ec2TagSet = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiEc2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead." #-}

-- | The deployment configuration name.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentConfigName :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe Lude.Text)
dgiDeploymentConfigName = Lens.lens (deploymentConfigName :: DeploymentGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigName = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

-- | Information about the most recent attempted deployment to the deployment group.
--
-- /Note:/ Consider using 'lastAttemptedDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLastAttemptedDeployment :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe LastDeploymentInfo)
dgiLastAttemptedDeployment = Lens.lens (lastAttemptedDeployment :: DeploymentGroupInfo -> Lude.Maybe LastDeploymentInfo) (\s a -> s {lastAttemptedDeployment = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiLastAttemptedDeployment "Use generic-lens or generic-optics with 'lastAttemptedDeployment' instead." #-}

-- | Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
--
-- /Note:/ Consider using 'onPremisesTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiOnPremisesTagSet :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe OnPremisesTagSet)
dgiOnPremisesTagSet = Lens.lens (onPremisesTagSet :: DeploymentGroupInfo -> Lude.Maybe OnPremisesTagSet) (\s a -> s {onPremisesTagSet = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiOnPremisesTagSet "Use generic-lens or generic-optics with 'onPremisesTagSet' instead." #-}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiComputePlatform :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe ComputePlatform)
dgiComputePlatform = Lens.lens (computePlatform :: DeploymentGroupInfo -> Lude.Maybe ComputePlatform) (\s a -> s {computePlatform = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | Information about the deployment group's target revision, including type and location.
--
-- /Note:/ Consider using 'targetRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiTargetRevision :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe RevisionLocation)
dgiTargetRevision = Lens.lens (targetRevision :: DeploymentGroupInfo -> Lude.Maybe RevisionLocation) (\s a -> s {targetRevision = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiTargetRevision "Use generic-lens or generic-optics with 'targetRevision' instead." #-}

-- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
--
-- /Note:/ Consider using 'ec2TagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEc2TagFilters :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe [EC2TagFilter])
dgiEc2TagFilters = Lens.lens (ec2TagFilters :: DeploymentGroupInfo -> Lude.Maybe [EC2TagFilter]) (\s a -> s {ec2TagFilters = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiEc2TagFilters "Use generic-lens or generic-optics with 'ec2TagFilters' instead." #-}

-- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
--
-- /Note:/ Consider using 'ecsServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEcsServices :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe [ECSService])
dgiEcsServices = Lens.lens (ecsServices :: DeploymentGroupInfo -> Lude.Maybe [ECSService]) (\s a -> s {ecsServices = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiEcsServices "Use generic-lens or generic-optics with 'ecsServices' instead." #-}

-- | Information about blue/green deployment options for a deployment group.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiBlueGreenDeploymentConfiguration :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe BlueGreenDeploymentConfiguration)
dgiBlueGreenDeploymentConfiguration = Lens.lens (blueGreenDeploymentConfiguration :: DeploymentGroupInfo -> Lude.Maybe BlueGreenDeploymentConfiguration) (\s a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiBlueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead." #-}

-- | Information about the load balancer to use in a deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLoadBalancerInfo :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe LoadBalancerInfo)
dgiLoadBalancerInfo = Lens.lens (loadBalancerInfo :: DeploymentGroupInfo -> Lude.Maybe LoadBalancerInfo) (\s a -> s {loadBalancerInfo = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiLoadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead." #-}

-- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
--
-- /Note:/ Consider using 'onPremisesInstanceTagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiOnPremisesInstanceTagFilters :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe [TagFilter])
dgiOnPremisesInstanceTagFilters = Lens.lens (onPremisesInstanceTagFilters :: DeploymentGroupInfo -> Lude.Maybe [TagFilter]) (\s a -> s {onPremisesInstanceTagFilters = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiOnPremisesInstanceTagFilters "Use generic-lens or generic-optics with 'onPremisesInstanceTagFilters' instead." #-}

-- | Information about the most recent successful deployment to the deployment group.
--
-- /Note:/ Consider using 'lastSuccessfulDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLastSuccessfulDeployment :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe LastDeploymentInfo)
dgiLastSuccessfulDeployment = Lens.lens (lastSuccessfulDeployment :: DeploymentGroupInfo -> Lude.Maybe LastDeploymentInfo) (\s a -> s {lastSuccessfulDeployment = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiLastSuccessfulDeployment "Use generic-lens or generic-optics with 'lastSuccessfulDeployment' instead." #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiApplicationName :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe Lude.Text)
dgiApplicationName = Lens.lens (applicationName :: DeploymentGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A list of alarms associated with the deployment group.
--
-- /Note:/ Consider using 'alarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAlarmConfiguration :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe AlarmConfiguration)
dgiAlarmConfiguration = Lens.lens (alarmConfiguration :: DeploymentGroupInfo -> Lude.Maybe AlarmConfiguration) (\s a -> s {alarmConfiguration = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiAlarmConfiguration "Use generic-lens or generic-optics with 'alarmConfiguration' instead." #-}

-- | Information about triggers associated with the deployment group.
--
-- /Note:/ Consider using 'triggerConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiTriggerConfigurations :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe [TriggerConfig])
dgiTriggerConfigurations = Lens.lens (triggerConfigurations :: DeploymentGroupInfo -> Lude.Maybe [TriggerConfig]) (\s a -> s {triggerConfigurations = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiTriggerConfigurations "Use generic-lens or generic-optics with 'triggerConfigurations' instead." #-}

-- | The deployment group ID.
--
-- /Note:/ Consider using 'deploymentGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentGroupId :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe Lude.Text)
dgiDeploymentGroupId = Lens.lens (deploymentGroupId :: DeploymentGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentGroupId = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiDeploymentGroupId "Use generic-lens or generic-optics with 'deploymentGroupId' instead." #-}

-- | A list of associated Auto Scaling groups.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAutoScalingGroups :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe [AutoScalingGroup])
dgiAutoScalingGroups = Lens.lens (autoScalingGroups :: DeploymentGroupInfo -> Lude.Maybe [AutoScalingGroup]) (\s a -> s {autoScalingGroups = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentStyle :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe DeploymentStyle)
dgiDeploymentStyle = Lens.lens (deploymentStyle :: DeploymentGroupInfo -> Lude.Maybe DeploymentStyle) (\s a -> s {deploymentStyle = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiDeploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead." #-}

-- | Information about the automatic rollback configuration associated with the deployment group.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAutoRollbackConfiguration :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe AutoRollbackConfiguration)
dgiAutoRollbackConfiguration = Lens.lens (autoRollbackConfiguration :: DeploymentGroupInfo -> Lude.Maybe AutoRollbackConfiguration) (\s a -> s {autoRollbackConfiguration = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiAutoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead." #-}

-- | The deployment group name.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentGroupName :: Lens.Lens' DeploymentGroupInfo (Lude.Maybe Lude.Text)
dgiDeploymentGroupName = Lens.lens (deploymentGroupName :: DeploymentGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentGroupName = a} :: DeploymentGroupInfo)
{-# DEPRECATED dgiDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

instance Lude.FromJSON DeploymentGroupInfo where
  parseJSON =
    Lude.withObject
      "DeploymentGroupInfo"
      ( \x ->
          DeploymentGroupInfo'
            Lude.<$> (x Lude..:? "serviceRoleArn")
            Lude.<*> (x Lude..:? "ec2TagSet")
            Lude.<*> (x Lude..:? "deploymentConfigName")
            Lude.<*> (x Lude..:? "lastAttemptedDeployment")
            Lude.<*> (x Lude..:? "onPremisesTagSet")
            Lude.<*> (x Lude..:? "computePlatform")
            Lude.<*> (x Lude..:? "targetRevision")
            Lude.<*> (x Lude..:? "ec2TagFilters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ecsServices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "blueGreenDeploymentConfiguration")
            Lude.<*> (x Lude..:? "loadBalancerInfo")
            Lude.<*> (x Lude..:? "onPremisesInstanceTagFilters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lastSuccessfulDeployment")
            Lude.<*> (x Lude..:? "applicationName")
            Lude.<*> (x Lude..:? "alarmConfiguration")
            Lude.<*> (x Lude..:? "triggerConfigurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "deploymentGroupId")
            Lude.<*> (x Lude..:? "autoScalingGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "deploymentStyle")
            Lude.<*> (x Lude..:? "autoRollbackConfiguration")
            Lude.<*> (x Lude..:? "deploymentGroupName")
      )
