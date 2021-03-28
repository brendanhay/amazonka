{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
  ( DeploymentGroupInfo (..)
  -- * Smart constructor
  , mkDeploymentGroupInfo
  -- * Lenses
  , dgiAlarmConfiguration
  , dgiApplicationName
  , dgiAutoRollbackConfiguration
  , dgiAutoScalingGroups
  , dgiBlueGreenDeploymentConfiguration
  , dgiComputePlatform
  , dgiDeploymentConfigName
  , dgiDeploymentGroupId
  , dgiDeploymentGroupName
  , dgiDeploymentStyle
  , dgiEc2TagFilters
  , dgiEc2TagSet
  , dgiEcsServices
  , dgiLastAttemptedDeployment
  , dgiLastSuccessfulDeployment
  , dgiLoadBalancerInfo
  , dgiOnPremisesInstanceTagFilters
  , dgiOnPremisesTagSet
  , dgiServiceRoleArn
  , dgiTargetRevision
  , dgiTriggerConfigurations
  ) where

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
  { alarmConfiguration :: Core.Maybe Types.AlarmConfiguration
    -- ^ A list of alarms associated with the deployment group.
  , applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The application name.
  , autoRollbackConfiguration :: Core.Maybe Types.AutoRollbackConfiguration
    -- ^ Information about the automatic rollback configuration associated with the deployment group.
  , autoScalingGroups :: Core.Maybe [Types.AutoScalingGroup]
    -- ^ A list of associated Auto Scaling groups.
  , blueGreenDeploymentConfiguration :: Core.Maybe Types.BlueGreenDeploymentConfiguration
    -- ^ Information about blue/green deployment options for a deployment group.
  , computePlatform :: Core.Maybe Types.ComputePlatform
    -- ^ The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
  , deploymentConfigName :: Core.Maybe Types.DeploymentConfigName
    -- ^ The deployment configuration name.
  , deploymentGroupId :: Core.Maybe Types.DeploymentGroupId
    -- ^ The deployment group ID.
  , deploymentGroupName :: Core.Maybe Types.DeploymentGroupName
    -- ^ The deployment group name.
  , deploymentStyle :: Core.Maybe Types.DeploymentStyle
    -- ^ Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
  , ec2TagFilters :: Core.Maybe [Types.EC2TagFilter]
    -- ^ The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
  , ec2TagSet :: Core.Maybe Types.EC2TagSet
    -- ^ Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
  , ecsServices :: Core.Maybe [Types.ECSService]
    -- ^ The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ . 
  , lastAttemptedDeployment :: Core.Maybe Types.LastDeploymentInfo
    -- ^ Information about the most recent attempted deployment to the deployment group.
  , lastSuccessfulDeployment :: Core.Maybe Types.LastDeploymentInfo
    -- ^ Information about the most recent successful deployment to the deployment group.
  , loadBalancerInfo :: Core.Maybe Types.LoadBalancerInfo
    -- ^ Information about the load balancer to use in a deployment.
  , onPremisesInstanceTagFilters :: Core.Maybe [Types.TagFilter]
    -- ^ The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
  , onPremisesTagSet :: Core.Maybe Types.OnPremisesTagSet
    -- ^ Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
  , serviceRoleArn :: Core.Maybe Types.ServiceRoleArn
    -- ^ A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
  , targetRevision :: Core.Maybe Types.RevisionLocation
    -- ^ Information about the deployment group's target revision, including type and location.
  , triggerConfigurations :: Core.Maybe [Types.TriggerConfig]
    -- ^ Information about triggers associated with the deployment group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeploymentGroupInfo' value with any optional fields omitted.
mkDeploymentGroupInfo
    :: DeploymentGroupInfo
mkDeploymentGroupInfo
  = DeploymentGroupInfo'{alarmConfiguration = Core.Nothing,
                         applicationName = Core.Nothing,
                         autoRollbackConfiguration = Core.Nothing,
                         autoScalingGroups = Core.Nothing,
                         blueGreenDeploymentConfiguration = Core.Nothing,
                         computePlatform = Core.Nothing,
                         deploymentConfigName = Core.Nothing,
                         deploymentGroupId = Core.Nothing,
                         deploymentGroupName = Core.Nothing, deploymentStyle = Core.Nothing,
                         ec2TagFilters = Core.Nothing, ec2TagSet = Core.Nothing,
                         ecsServices = Core.Nothing, lastAttemptedDeployment = Core.Nothing,
                         lastSuccessfulDeployment = Core.Nothing,
                         loadBalancerInfo = Core.Nothing,
                         onPremisesInstanceTagFilters = Core.Nothing,
                         onPremisesTagSet = Core.Nothing, serviceRoleArn = Core.Nothing,
                         targetRevision = Core.Nothing,
                         triggerConfigurations = Core.Nothing}

-- | A list of alarms associated with the deployment group.
--
-- /Note:/ Consider using 'alarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAlarmConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.AlarmConfiguration)
dgiAlarmConfiguration = Lens.field @"alarmConfiguration"
{-# INLINEABLE dgiAlarmConfiguration #-}
{-# DEPRECATED alarmConfiguration "Use generic-lens or generic-optics with 'alarmConfiguration' instead"  #-}

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiApplicationName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.ApplicationName)
dgiApplicationName = Lens.field @"applicationName"
{-# INLINEABLE dgiApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Information about the automatic rollback configuration associated with the deployment group.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAutoRollbackConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.AutoRollbackConfiguration)
dgiAutoRollbackConfiguration = Lens.field @"autoRollbackConfiguration"
{-# INLINEABLE dgiAutoRollbackConfiguration #-}
{-# DEPRECATED autoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead"  #-}

-- | A list of associated Auto Scaling groups.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiAutoScalingGroups :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.AutoScalingGroup])
dgiAutoScalingGroups = Lens.field @"autoScalingGroups"
{-# INLINEABLE dgiAutoScalingGroups #-}
{-# DEPRECATED autoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead"  #-}

-- | Information about blue/green deployment options for a deployment group.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiBlueGreenDeploymentConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.BlueGreenDeploymentConfiguration)
dgiBlueGreenDeploymentConfiguration = Lens.field @"blueGreenDeploymentConfiguration"
{-# INLINEABLE dgiBlueGreenDeploymentConfiguration #-}
{-# DEPRECATED blueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead"  #-}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiComputePlatform :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.ComputePlatform)
dgiComputePlatform = Lens.field @"computePlatform"
{-# INLINEABLE dgiComputePlatform #-}
{-# DEPRECATED computePlatform "Use generic-lens or generic-optics with 'computePlatform' instead"  #-}

-- | The deployment configuration name.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentConfigName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentConfigName)
dgiDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# INLINEABLE dgiDeploymentConfigName #-}
{-# DEPRECATED deploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead"  #-}

-- | The deployment group ID.
--
-- /Note:/ Consider using 'deploymentGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentGroupId :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentGroupId)
dgiDeploymentGroupId = Lens.field @"deploymentGroupId"
{-# INLINEABLE dgiDeploymentGroupId #-}
{-# DEPRECATED deploymentGroupId "Use generic-lens or generic-optics with 'deploymentGroupId' instead"  #-}

-- | The deployment group name.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentGroupName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentGroupName)
dgiDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# INLINEABLE dgiDeploymentGroupName #-}
{-# DEPRECATED deploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead"  #-}

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiDeploymentStyle :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.DeploymentStyle)
dgiDeploymentStyle = Lens.field @"deploymentStyle"
{-# INLINEABLE dgiDeploymentStyle #-}
{-# DEPRECATED deploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead"  #-}

-- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags.
--
-- /Note:/ Consider using 'ec2TagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEc2TagFilters :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.EC2TagFilter])
dgiEc2TagFilters = Lens.field @"ec2TagFilters"
{-# INLINEABLE dgiEc2TagFilters #-}
{-# DEPRECATED ec2TagFilters "Use generic-lens or generic-optics with 'ec2TagFilters' instead"  #-}

-- | Information about groups of tags applied to an EC2 instance. The deployment group includes only EC2 instances identified by all of the tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEc2TagSet :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.EC2TagSet)
dgiEc2TagSet = Lens.field @"ec2TagSet"
{-# INLINEABLE dgiEc2TagSet #-}
{-# DEPRECATED ec2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead"  #-}

-- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ . 
--
-- /Note:/ Consider using 'ecsServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiEcsServices :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.ECSService])
dgiEcsServices = Lens.field @"ecsServices"
{-# INLINEABLE dgiEcsServices #-}
{-# DEPRECATED ecsServices "Use generic-lens or generic-optics with 'ecsServices' instead"  #-}

-- | Information about the most recent attempted deployment to the deployment group.
--
-- /Note:/ Consider using 'lastAttemptedDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLastAttemptedDeployment :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.LastDeploymentInfo)
dgiLastAttemptedDeployment = Lens.field @"lastAttemptedDeployment"
{-# INLINEABLE dgiLastAttemptedDeployment #-}
{-# DEPRECATED lastAttemptedDeployment "Use generic-lens or generic-optics with 'lastAttemptedDeployment' instead"  #-}

-- | Information about the most recent successful deployment to the deployment group.
--
-- /Note:/ Consider using 'lastSuccessfulDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLastSuccessfulDeployment :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.LastDeploymentInfo)
dgiLastSuccessfulDeployment = Lens.field @"lastSuccessfulDeployment"
{-# INLINEABLE dgiLastSuccessfulDeployment #-}
{-# DEPRECATED lastSuccessfulDeployment "Use generic-lens or generic-optics with 'lastSuccessfulDeployment' instead"  #-}

-- | Information about the load balancer to use in a deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiLoadBalancerInfo :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.LoadBalancerInfo)
dgiLoadBalancerInfo = Lens.field @"loadBalancerInfo"
{-# INLINEABLE dgiLoadBalancerInfo #-}
{-# DEPRECATED loadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead"  #-}

-- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags.
--
-- /Note:/ Consider using 'onPremisesInstanceTagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiOnPremisesInstanceTagFilters :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.TagFilter])
dgiOnPremisesInstanceTagFilters = Lens.field @"onPremisesInstanceTagFilters"
{-# INLINEABLE dgiOnPremisesInstanceTagFilters #-}
{-# DEPRECATED onPremisesInstanceTagFilters "Use generic-lens or generic-optics with 'onPremisesInstanceTagFilters' instead"  #-}

-- | Information about groups of tags applied to an on-premises instance. The deployment group includes only on-premises instances identified by all the tag groups. Cannot be used in the same call as onPremisesInstanceTagFilters.
--
-- /Note:/ Consider using 'onPremisesTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiOnPremisesTagSet :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.OnPremisesTagSet)
dgiOnPremisesTagSet = Lens.field @"onPremisesTagSet"
{-# INLINEABLE dgiOnPremisesTagSet #-}
{-# DEPRECATED onPremisesTagSet "Use generic-lens or generic-optics with 'onPremisesTagSet' instead"  #-}

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy permission to make calls to AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiServiceRoleArn :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.ServiceRoleArn)
dgiServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE dgiServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | Information about the deployment group's target revision, including type and location.
--
-- /Note:/ Consider using 'targetRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiTargetRevision :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Types.RevisionLocation)
dgiTargetRevision = Lens.field @"targetRevision"
{-# INLINEABLE dgiTargetRevision #-}
{-# DEPRECATED targetRevision "Use generic-lens or generic-optics with 'targetRevision' instead"  #-}

-- | Information about triggers associated with the deployment group.
--
-- /Note:/ Consider using 'triggerConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiTriggerConfigurations :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [Types.TriggerConfig])
dgiTriggerConfigurations = Lens.field @"triggerConfigurations"
{-# INLINEABLE dgiTriggerConfigurations #-}
{-# DEPRECATED triggerConfigurations "Use generic-lens or generic-optics with 'triggerConfigurations' instead"  #-}

instance Core.FromJSON DeploymentGroupInfo where
        parseJSON
          = Core.withObject "DeploymentGroupInfo" Core.$
              \ x ->
                DeploymentGroupInfo' Core.<$>
                  (x Core..:? "alarmConfiguration") Core.<*>
                    x Core..:? "applicationName"
                    Core.<*> x Core..:? "autoRollbackConfiguration"
                    Core.<*> x Core..:? "autoScalingGroups"
                    Core.<*> x Core..:? "blueGreenDeploymentConfiguration"
                    Core.<*> x Core..:? "computePlatform"
                    Core.<*> x Core..:? "deploymentConfigName"
                    Core.<*> x Core..:? "deploymentGroupId"
                    Core.<*> x Core..:? "deploymentGroupName"
                    Core.<*> x Core..:? "deploymentStyle"
                    Core.<*> x Core..:? "ec2TagFilters"
                    Core.<*> x Core..:? "ec2TagSet"
                    Core.<*> x Core..:? "ecsServices"
                    Core.<*> x Core..:? "lastAttemptedDeployment"
                    Core.<*> x Core..:? "lastSuccessfulDeployment"
                    Core.<*> x Core..:? "loadBalancerInfo"
                    Core.<*> x Core..:? "onPremisesInstanceTagFilters"
                    Core.<*> x Core..:? "onPremisesTagSet"
                    Core.<*> x Core..:? "serviceRoleArn"
                    Core.<*> x Core..:? "targetRevision"
                    Core.<*> x Core..:? "triggerConfigurations"
