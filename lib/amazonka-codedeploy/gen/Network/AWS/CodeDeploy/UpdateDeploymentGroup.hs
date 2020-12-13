{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.UpdateDeploymentGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a deployment group.
module Network.AWS.CodeDeploy.UpdateDeploymentGroup
  ( -- * Creating a request
    UpdateDeploymentGroup (..),
    mkUpdateDeploymentGroup,

    -- ** Request lenses
    udgServiceRoleARN,
    udgEc2TagSet,
    udgDeploymentConfigName,
    udgOnPremisesTagSet,
    udgNewDeploymentGroupName,
    udgEc2TagFilters,
    udgCurrentDeploymentGroupName,
    udgEcsServices,
    udgBlueGreenDeploymentConfiguration,
    udgLoadBalancerInfo,
    udgOnPremisesInstanceTagFilters,
    udgApplicationName,
    udgAlarmConfiguration,
    udgTriggerConfigurations,
    udgAutoScalingGroups,
    udgDeploymentStyle,
    udgAutoRollbackConfiguration,

    -- * Destructuring the response
    UpdateDeploymentGroupResponse (..),
    mkUpdateDeploymentGroupResponse,

    -- ** Response lenses
    udgrsHooksNotCleanedUp,
    udgrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an @UpdateDeploymentGroup@ operation.
--
-- /See:/ 'mkUpdateDeploymentGroup' smart constructor.
data UpdateDeploymentGroup = UpdateDeploymentGroup'
  { -- | A replacement ARN for the service role, if you want to change it.
    serviceRoleARN :: Lude.Maybe Lude.Text,
    -- | Information about groups of tags applied to on-premises instances. The deployment group includes only EC2 instances identified by all the tag groups.
    ec2TagSet :: Lude.Maybe EC2TagSet,
    -- | The replacement deployment configuration name to use, if you want to change it.
    deploymentConfigName :: Lude.Maybe Lude.Text,
    -- | Information about an on-premises instance tag set. The deployment group includes only on-premises instances identified by all the tag groups.
    onPremisesTagSet :: Lude.Maybe OnPremisesTagSet,
    -- | The new name of the deployment group, if you want to change it.
    newDeploymentGroupName :: Lude.Maybe Lude.Text,
    -- | The replacement set of Amazon EC2 tags on which to filter, if you want to change them. To keep the existing tags, enter their names. To remove tags, do not enter any tag names.
    ec2TagFilters :: Lude.Maybe [EC2TagFilter],
    -- | The current name of the deployment group.
    currentDeploymentGroupName :: Lude.Text,
    -- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
    ecsServices :: Lude.Maybe [ECSService],
    -- | Information about blue/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Lude.Maybe BlueGreenDeploymentConfiguration,
    -- | Information about the load balancer used in a deployment.
    loadBalancerInfo :: Lude.Maybe LoadBalancerInfo,
    -- | The replacement set of on-premises instance tags on which to filter, if you want to change them. To keep the existing tags, enter their names. To remove tags, do not enter any tag names.
    onPremisesInstanceTagFilters :: Lude.Maybe [TagFilter],
    -- | The application name that corresponds to the deployment group to update.
    applicationName :: Lude.Text,
    -- | Information to add or change about Amazon CloudWatch alarms when the deployment group is updated.
    alarmConfiguration :: Lude.Maybe AlarmConfiguration,
    -- | Information about triggers to change when the deployment group is updated. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group> in the /AWS CodeDeploy User Guide/ .
    triggerConfigurations :: Lude.Maybe [TriggerConfig],
    -- | The replacement list of Auto Scaling groups to be included in the deployment group, if you want to change them. To keep the Auto Scaling groups, enter their names. To remove Auto Scaling groups, do not enter any Auto Scaling group names.
    autoScalingGroups :: Lude.Maybe [Lude.Text],
    -- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
    deploymentStyle :: Lude.Maybe DeploymentStyle,
    -- | Information for an automatic rollback configuration that is added or changed when a deployment group is updated.
    autoRollbackConfiguration :: Lude.Maybe AutoRollbackConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeploymentGroup' with the minimum fields required to make a request.
--
-- * 'serviceRoleARN' - A replacement ARN for the service role, if you want to change it.
-- * 'ec2TagSet' - Information about groups of tags applied to on-premises instances. The deployment group includes only EC2 instances identified by all the tag groups.
-- * 'deploymentConfigName' - The replacement deployment configuration name to use, if you want to change it.
-- * 'onPremisesTagSet' - Information about an on-premises instance tag set. The deployment group includes only on-premises instances identified by all the tag groups.
-- * 'newDeploymentGroupName' - The new name of the deployment group, if you want to change it.
-- * 'ec2TagFilters' - The replacement set of Amazon EC2 tags on which to filter, if you want to change them. To keep the existing tags, enter their names. To remove tags, do not enter any tag names.
-- * 'currentDeploymentGroupName' - The current name of the deployment group.
-- * 'ecsServices' - The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
-- * 'blueGreenDeploymentConfiguration' - Information about blue/green deployment options for a deployment group.
-- * 'loadBalancerInfo' - Information about the load balancer used in a deployment.
-- * 'onPremisesInstanceTagFilters' - The replacement set of on-premises instance tags on which to filter, if you want to change them. To keep the existing tags, enter their names. To remove tags, do not enter any tag names.
-- * 'applicationName' - The application name that corresponds to the deployment group to update.
-- * 'alarmConfiguration' - Information to add or change about Amazon CloudWatch alarms when the deployment group is updated.
-- * 'triggerConfigurations' - Information about triggers to change when the deployment group is updated. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group> in the /AWS CodeDeploy User Guide/ .
-- * 'autoScalingGroups' - The replacement list of Auto Scaling groups to be included in the deployment group, if you want to change them. To keep the Auto Scaling groups, enter their names. To remove Auto Scaling groups, do not enter any Auto Scaling group names.
-- * 'deploymentStyle' - Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
-- * 'autoRollbackConfiguration' - Information for an automatic rollback configuration that is added or changed when a deployment group is updated.
mkUpdateDeploymentGroup ::
  -- | 'currentDeploymentGroupName'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  UpdateDeploymentGroup
mkUpdateDeploymentGroup
  pCurrentDeploymentGroupName_
  pApplicationName_ =
    UpdateDeploymentGroup'
      { serviceRoleARN = Lude.Nothing,
        ec2TagSet = Lude.Nothing,
        deploymentConfigName = Lude.Nothing,
        onPremisesTagSet = Lude.Nothing,
        newDeploymentGroupName = Lude.Nothing,
        ec2TagFilters = Lude.Nothing,
        currentDeploymentGroupName = pCurrentDeploymentGroupName_,
        ecsServices = Lude.Nothing,
        blueGreenDeploymentConfiguration = Lude.Nothing,
        loadBalancerInfo = Lude.Nothing,
        onPremisesInstanceTagFilters = Lude.Nothing,
        applicationName = pApplicationName_,
        alarmConfiguration = Lude.Nothing,
        triggerConfigurations = Lude.Nothing,
        autoScalingGroups = Lude.Nothing,
        deploymentStyle = Lude.Nothing,
        autoRollbackConfiguration = Lude.Nothing
      }

-- | A replacement ARN for the service role, if you want to change it.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgServiceRoleARN :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe Lude.Text)
udgServiceRoleARN = Lens.lens (serviceRoleARN :: UpdateDeploymentGroup -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Information about groups of tags applied to on-premises instances. The deployment group includes only EC2 instances identified by all the tag groups.
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgEc2TagSet :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe EC2TagSet)
udgEc2TagSet = Lens.lens (ec2TagSet :: UpdateDeploymentGroup -> Lude.Maybe EC2TagSet) (\s a -> s {ec2TagSet = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgEc2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead." #-}

-- | The replacement deployment configuration name to use, if you want to change it.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgDeploymentConfigName :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe Lude.Text)
udgDeploymentConfigName = Lens.lens (deploymentConfigName :: UpdateDeploymentGroup -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigName = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

-- | Information about an on-premises instance tag set. The deployment group includes only on-premises instances identified by all the tag groups.
--
-- /Note:/ Consider using 'onPremisesTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgOnPremisesTagSet :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe OnPremisesTagSet)
udgOnPremisesTagSet = Lens.lens (onPremisesTagSet :: UpdateDeploymentGroup -> Lude.Maybe OnPremisesTagSet) (\s a -> s {onPremisesTagSet = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgOnPremisesTagSet "Use generic-lens or generic-optics with 'onPremisesTagSet' instead." #-}

-- | The new name of the deployment group, if you want to change it.
--
-- /Note:/ Consider using 'newDeploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgNewDeploymentGroupName :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe Lude.Text)
udgNewDeploymentGroupName = Lens.lens (newDeploymentGroupName :: UpdateDeploymentGroup -> Lude.Maybe Lude.Text) (\s a -> s {newDeploymentGroupName = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgNewDeploymentGroupName "Use generic-lens or generic-optics with 'newDeploymentGroupName' instead." #-}

-- | The replacement set of Amazon EC2 tags on which to filter, if you want to change them. To keep the existing tags, enter their names. To remove tags, do not enter any tag names.
--
-- /Note:/ Consider using 'ec2TagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgEc2TagFilters :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe [EC2TagFilter])
udgEc2TagFilters = Lens.lens (ec2TagFilters :: UpdateDeploymentGroup -> Lude.Maybe [EC2TagFilter]) (\s a -> s {ec2TagFilters = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgEc2TagFilters "Use generic-lens or generic-optics with 'ec2TagFilters' instead." #-}

-- | The current name of the deployment group.
--
-- /Note:/ Consider using 'currentDeploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgCurrentDeploymentGroupName :: Lens.Lens' UpdateDeploymentGroup Lude.Text
udgCurrentDeploymentGroupName = Lens.lens (currentDeploymentGroupName :: UpdateDeploymentGroup -> Lude.Text) (\s a -> s {currentDeploymentGroupName = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgCurrentDeploymentGroupName "Use generic-lens or generic-optics with 'currentDeploymentGroupName' instead." #-}

-- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
--
-- /Note:/ Consider using 'ecsServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgEcsServices :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe [ECSService])
udgEcsServices = Lens.lens (ecsServices :: UpdateDeploymentGroup -> Lude.Maybe [ECSService]) (\s a -> s {ecsServices = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgEcsServices "Use generic-lens or generic-optics with 'ecsServices' instead." #-}

-- | Information about blue/green deployment options for a deployment group.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgBlueGreenDeploymentConfiguration :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe BlueGreenDeploymentConfiguration)
udgBlueGreenDeploymentConfiguration = Lens.lens (blueGreenDeploymentConfiguration :: UpdateDeploymentGroup -> Lude.Maybe BlueGreenDeploymentConfiguration) (\s a -> s {blueGreenDeploymentConfiguration = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgBlueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead." #-}

-- | Information about the load balancer used in a deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgLoadBalancerInfo :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe LoadBalancerInfo)
udgLoadBalancerInfo = Lens.lens (loadBalancerInfo :: UpdateDeploymentGroup -> Lude.Maybe LoadBalancerInfo) (\s a -> s {loadBalancerInfo = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgLoadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead." #-}

-- | The replacement set of on-premises instance tags on which to filter, if you want to change them. To keep the existing tags, enter their names. To remove tags, do not enter any tag names.
--
-- /Note:/ Consider using 'onPremisesInstanceTagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgOnPremisesInstanceTagFilters :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe [TagFilter])
udgOnPremisesInstanceTagFilters = Lens.lens (onPremisesInstanceTagFilters :: UpdateDeploymentGroup -> Lude.Maybe [TagFilter]) (\s a -> s {onPremisesInstanceTagFilters = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgOnPremisesInstanceTagFilters "Use generic-lens or generic-optics with 'onPremisesInstanceTagFilters' instead." #-}

-- | The application name that corresponds to the deployment group to update.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgApplicationName :: Lens.Lens' UpdateDeploymentGroup Lude.Text
udgApplicationName = Lens.lens (applicationName :: UpdateDeploymentGroup -> Lude.Text) (\s a -> s {applicationName = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information to add or change about Amazon CloudWatch alarms when the deployment group is updated.
--
-- /Note:/ Consider using 'alarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgAlarmConfiguration :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe AlarmConfiguration)
udgAlarmConfiguration = Lens.lens (alarmConfiguration :: UpdateDeploymentGroup -> Lude.Maybe AlarmConfiguration) (\s a -> s {alarmConfiguration = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgAlarmConfiguration "Use generic-lens or generic-optics with 'alarmConfiguration' instead." #-}

-- | Information about triggers to change when the deployment group is updated. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'triggerConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgTriggerConfigurations :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe [TriggerConfig])
udgTriggerConfigurations = Lens.lens (triggerConfigurations :: UpdateDeploymentGroup -> Lude.Maybe [TriggerConfig]) (\s a -> s {triggerConfigurations = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgTriggerConfigurations "Use generic-lens or generic-optics with 'triggerConfigurations' instead." #-}

-- | The replacement list of Auto Scaling groups to be included in the deployment group, if you want to change them. To keep the Auto Scaling groups, enter their names. To remove Auto Scaling groups, do not enter any Auto Scaling group names.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgAutoScalingGroups :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe [Lude.Text])
udgAutoScalingGroups = Lens.lens (autoScalingGroups :: UpdateDeploymentGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {autoScalingGroups = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgDeploymentStyle :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe DeploymentStyle)
udgDeploymentStyle = Lens.lens (deploymentStyle :: UpdateDeploymentGroup -> Lude.Maybe DeploymentStyle) (\s a -> s {deploymentStyle = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgDeploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead." #-}

-- | Information for an automatic rollback configuration that is added or changed when a deployment group is updated.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgAutoRollbackConfiguration :: Lens.Lens' UpdateDeploymentGroup (Lude.Maybe AutoRollbackConfiguration)
udgAutoRollbackConfiguration = Lens.lens (autoRollbackConfiguration :: UpdateDeploymentGroup -> Lude.Maybe AutoRollbackConfiguration) (\s a -> s {autoRollbackConfiguration = a} :: UpdateDeploymentGroup)
{-# DEPRECATED udgAutoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead." #-}

instance Lude.AWSRequest UpdateDeploymentGroup where
  type Rs UpdateDeploymentGroup = UpdateDeploymentGroupResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDeploymentGroupResponse'
            Lude.<$> (x Lude..?> "hooksNotCleanedUp" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDeploymentGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.UpdateDeploymentGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDeploymentGroup where
  toJSON UpdateDeploymentGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serviceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("ec2TagSet" Lude..=) Lude.<$> ec2TagSet,
            ("deploymentConfigName" Lude..=) Lude.<$> deploymentConfigName,
            ("onPremisesTagSet" Lude..=) Lude.<$> onPremisesTagSet,
            ("newDeploymentGroupName" Lude..=) Lude.<$> newDeploymentGroupName,
            ("ec2TagFilters" Lude..=) Lude.<$> ec2TagFilters,
            Lude.Just
              ("currentDeploymentGroupName" Lude..= currentDeploymentGroupName),
            ("ecsServices" Lude..=) Lude.<$> ecsServices,
            ("blueGreenDeploymentConfiguration" Lude..=)
              Lude.<$> blueGreenDeploymentConfiguration,
            ("loadBalancerInfo" Lude..=) Lude.<$> loadBalancerInfo,
            ("onPremisesInstanceTagFilters" Lude..=)
              Lude.<$> onPremisesInstanceTagFilters,
            Lude.Just ("applicationName" Lude..= applicationName),
            ("alarmConfiguration" Lude..=) Lude.<$> alarmConfiguration,
            ("triggerConfigurations" Lude..=) Lude.<$> triggerConfigurations,
            ("autoScalingGroups" Lude..=) Lude.<$> autoScalingGroups,
            ("deploymentStyle" Lude..=) Lude.<$> deploymentStyle,
            ("autoRollbackConfiguration" Lude..=)
              Lude.<$> autoRollbackConfiguration
          ]
      )

instance Lude.ToPath UpdateDeploymentGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDeploymentGroup where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @UpdateDeploymentGroup@ operation.
--
-- /See:/ 'mkUpdateDeploymentGroupResponse' smart constructor.
data UpdateDeploymentGroupResponse = UpdateDeploymentGroupResponse'
  { -- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the AWS account. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the AWS account.
    hooksNotCleanedUp :: Lude.Maybe [AutoScalingGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- * 'hooksNotCleanedUp' - If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the AWS account. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the AWS account.
-- * 'responseStatus' - The response status code.
mkUpdateDeploymentGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDeploymentGroupResponse
mkUpdateDeploymentGroupResponse pResponseStatus_ =
  UpdateDeploymentGroupResponse'
    { hooksNotCleanedUp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the AWS account. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the AWS account.
--
-- /Note:/ Consider using 'hooksNotCleanedUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgrsHooksNotCleanedUp :: Lens.Lens' UpdateDeploymentGroupResponse (Lude.Maybe [AutoScalingGroup])
udgrsHooksNotCleanedUp = Lens.lens (hooksNotCleanedUp :: UpdateDeploymentGroupResponse -> Lude.Maybe [AutoScalingGroup]) (\s a -> s {hooksNotCleanedUp = a} :: UpdateDeploymentGroupResponse)
{-# DEPRECATED udgrsHooksNotCleanedUp "Use generic-lens or generic-optics with 'hooksNotCleanedUp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udgrsResponseStatus :: Lens.Lens' UpdateDeploymentGroupResponse Lude.Int
udgrsResponseStatus = Lens.lens (responseStatus :: UpdateDeploymentGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDeploymentGroupResponse)
{-# DEPRECATED udgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
