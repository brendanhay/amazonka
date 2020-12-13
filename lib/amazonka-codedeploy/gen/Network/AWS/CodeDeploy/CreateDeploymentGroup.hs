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
    cdgServiceRoleARN,
    cdgEc2TagSet,
    cdgDeploymentConfigName,
    cdgOnPremisesTagSet,
    cdgEc2TagFilters,
    cdgEcsServices,
    cdgBlueGreenDeploymentConfiguration,
    cdgLoadBalancerInfo,
    cdgOnPremisesInstanceTagFilters,
    cdgApplicationName,
    cdgAlarmConfiguration,
    cdgTriggerConfigurations,
    cdgAutoScalingGroups,
    cdgDeploymentStyle,
    cdgAutoRollbackConfiguration,
    cdgDeploymentGroupName,
    cdgTags,

    -- * Destructuring the response
    CreateDeploymentGroupResponse (..),
    mkCreateDeploymentGroupResponse,

    -- ** Response lenses
    cdgrsDeploymentGroupId,
    cdgrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateDeploymentGroup@ operation.
--
-- /See:/ 'mkCreateDeploymentGroup' smart constructor.
data CreateDeploymentGroup = CreateDeploymentGroup'
  { -- | A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to act on the user's behalf when interacting with AWS services.
    serviceRoleARN :: Lude.Text,
    -- | Information about groups of tags applied to EC2 instances. The deployment group includes only EC2 instances identified by all the tag groups. Cannot be used in the same call as @ec2TagFilters@ .
    ec2TagSet :: Lude.Maybe EC2TagSet,
    -- | If specified, the deployment configuration name can be either one of the predefined configurations provided with AWS CodeDeploy or a custom deployment configuration that you create by calling the create deployment configuration operation.
    --
    -- @CodeDeployDefault.OneAtATime@ is the default deployment configuration. It is used if a configuration isn't specified for the deployment or deployment group.
    -- For more information about the predefined deployment configurations in AWS CodeDeploy, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy> in the /AWS CodeDeploy User Guide/ .
    deploymentConfigName :: Lude.Maybe Lude.Text,
    -- | Information about groups of tags applied to on-premises instances. The deployment group includes only on-premises instances identified by all of the tag groups. Cannot be used in the same call as @onPremisesInstanceTagFilters@ .
    onPremisesTagSet :: Lude.Maybe OnPremisesTagSet,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags. Cannot be used in the same call as ec2TagSet.
    ec2TagFilters :: Lude.Maybe [EC2TagFilter],
    -- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
    ecsServices :: Lude.Maybe [ECSService],
    -- | Information about blue/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Lude.Maybe BlueGreenDeploymentConfiguration,
    -- | Information about the load balancer used in a deployment.
    loadBalancerInfo :: Lude.Maybe LoadBalancerInfo,
    -- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags. Cannot be used in the same call as @OnPremisesTagSet@ .
    onPremisesInstanceTagFilters :: Lude.Maybe [TagFilter],
    -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Lude.Text,
    -- | Information to add about Amazon CloudWatch alarms when the deployment group is created.
    alarmConfiguration :: Lude.Maybe AlarmConfiguration,
    -- | Information about triggers to create when the deployment group is created. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event> in the /AWS CodeDeploy User Guide/ .
    triggerConfigurations :: Lude.Maybe [TriggerConfig],
    -- | A list of associated Amazon EC2 Auto Scaling groups.
    autoScalingGroups :: Lude.Maybe [Lude.Text],
    -- | Information about the type of deployment, in-place or blue/green, that you want to run and whether to route deployment traffic behind a load balancer.
    deploymentStyle :: Lude.Maybe DeploymentStyle,
    -- | Configuration information for an automatic rollback that is added when a deployment group is created.
    autoRollbackConfiguration :: Lude.Maybe AutoRollbackConfiguration,
    -- | The name of a new deployment group for the specified application.
    deploymentGroupName :: Lude.Text,
    -- | The metadata that you apply to CodeDeploy deployment groups to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeploymentGroup' with the minimum fields required to make a request.
--
-- * 'serviceRoleARN' - A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to act on the user's behalf when interacting with AWS services.
-- * 'ec2TagSet' - Information about groups of tags applied to EC2 instances. The deployment group includes only EC2 instances identified by all the tag groups. Cannot be used in the same call as @ec2TagFilters@ .
-- * 'deploymentConfigName' - If specified, the deployment configuration name can be either one of the predefined configurations provided with AWS CodeDeploy or a custom deployment configuration that you create by calling the create deployment configuration operation.
--
-- @CodeDeployDefault.OneAtATime@ is the default deployment configuration. It is used if a configuration isn't specified for the deployment or deployment group.
-- For more information about the predefined deployment configurations in AWS CodeDeploy, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy> in the /AWS CodeDeploy User Guide/ .
-- * 'onPremisesTagSet' - Information about groups of tags applied to on-premises instances. The deployment group includes only on-premises instances identified by all of the tag groups. Cannot be used in the same call as @onPremisesInstanceTagFilters@ .
-- * 'ec2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags. Cannot be used in the same call as ec2TagSet.
-- * 'ecsServices' - The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
-- * 'blueGreenDeploymentConfiguration' - Information about blue/green deployment options for a deployment group.
-- * 'loadBalancerInfo' - Information about the load balancer used in a deployment.
-- * 'onPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags. Cannot be used in the same call as @OnPremisesTagSet@ .
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
-- * 'alarmConfiguration' - Information to add about Amazon CloudWatch alarms when the deployment group is created.
-- * 'triggerConfigurations' - Information about triggers to create when the deployment group is created. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event> in the /AWS CodeDeploy User Guide/ .
-- * 'autoScalingGroups' - A list of associated Amazon EC2 Auto Scaling groups.
-- * 'deploymentStyle' - Information about the type of deployment, in-place or blue/green, that you want to run and whether to route deployment traffic behind a load balancer.
-- * 'autoRollbackConfiguration' - Configuration information for an automatic rollback that is added when a deployment group is created.
-- * 'deploymentGroupName' - The name of a new deployment group for the specified application.
-- * 'tags' - The metadata that you apply to CodeDeploy deployment groups to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
mkCreateDeploymentGroup ::
  -- | 'serviceRoleARN'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  -- | 'deploymentGroupName'
  Lude.Text ->
  CreateDeploymentGroup
mkCreateDeploymentGroup
  pServiceRoleARN_
  pApplicationName_
  pDeploymentGroupName_ =
    CreateDeploymentGroup'
      { serviceRoleARN = pServiceRoleARN_,
        ec2TagSet = Lude.Nothing,
        deploymentConfigName = Lude.Nothing,
        onPremisesTagSet = Lude.Nothing,
        ec2TagFilters = Lude.Nothing,
        ecsServices = Lude.Nothing,
        blueGreenDeploymentConfiguration = Lude.Nothing,
        loadBalancerInfo = Lude.Nothing,
        onPremisesInstanceTagFilters = Lude.Nothing,
        applicationName = pApplicationName_,
        alarmConfiguration = Lude.Nothing,
        triggerConfigurations = Lude.Nothing,
        autoScalingGroups = Lude.Nothing,
        deploymentStyle = Lude.Nothing,
        autoRollbackConfiguration = Lude.Nothing,
        deploymentGroupName = pDeploymentGroupName_,
        tags = Lude.Nothing
      }

-- | A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to act on the user's behalf when interacting with AWS services.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgServiceRoleARN :: Lens.Lens' CreateDeploymentGroup Lude.Text
cdgServiceRoleARN = Lens.lens (serviceRoleARN :: CreateDeploymentGroup -> Lude.Text) (\s a -> s {serviceRoleARN = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Information about groups of tags applied to EC2 instances. The deployment group includes only EC2 instances identified by all the tag groups. Cannot be used in the same call as @ec2TagFilters@ .
--
-- /Note:/ Consider using 'ec2TagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgEc2TagSet :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe EC2TagSet)
cdgEc2TagSet = Lens.lens (ec2TagSet :: CreateDeploymentGroup -> Lude.Maybe EC2TagSet) (\s a -> s {ec2TagSet = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgEc2TagSet "Use generic-lens or generic-optics with 'ec2TagSet' instead." #-}

-- | If specified, the deployment configuration name can be either one of the predefined configurations provided with AWS CodeDeploy or a custom deployment configuration that you create by calling the create deployment configuration operation.
--
-- @CodeDeployDefault.OneAtATime@ is the default deployment configuration. It is used if a configuration isn't specified for the deployment or deployment group.
-- For more information about the predefined deployment configurations in AWS CodeDeploy, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgDeploymentConfigName :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe Lude.Text)
cdgDeploymentConfigName = Lens.lens (deploymentConfigName :: CreateDeploymentGroup -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigName = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

-- | Information about groups of tags applied to on-premises instances. The deployment group includes only on-premises instances identified by all of the tag groups. Cannot be used in the same call as @onPremisesInstanceTagFilters@ .
--
-- /Note:/ Consider using 'onPremisesTagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgOnPremisesTagSet :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe OnPremisesTagSet)
cdgOnPremisesTagSet = Lens.lens (onPremisesTagSet :: CreateDeploymentGroup -> Lude.Maybe OnPremisesTagSet) (\s a -> s {onPremisesTagSet = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgOnPremisesTagSet "Use generic-lens or generic-optics with 'onPremisesTagSet' instead." #-}

-- | The Amazon EC2 tags on which to filter. The deployment group includes EC2 instances with any of the specified tags. Cannot be used in the same call as ec2TagSet.
--
-- /Note:/ Consider using 'ec2TagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgEc2TagFilters :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe [EC2TagFilter])
cdgEc2TagFilters = Lens.lens (ec2TagFilters :: CreateDeploymentGroup -> Lude.Maybe [EC2TagFilter]) (\s a -> s {ec2TagFilters = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgEc2TagFilters "Use generic-lens or generic-optics with 'ec2TagFilters' instead." #-}

-- | The target Amazon ECS services in the deployment group. This applies only to deployment groups that use the Amazon ECS compute platform. A target Amazon ECS service is specified as an Amazon ECS cluster and service name pair using the format @<clustername>:<servicename>@ .
--
-- /Note:/ Consider using 'ecsServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgEcsServices :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe [ECSService])
cdgEcsServices = Lens.lens (ecsServices :: CreateDeploymentGroup -> Lude.Maybe [ECSService]) (\s a -> s {ecsServices = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgEcsServices "Use generic-lens or generic-optics with 'ecsServices' instead." #-}

-- | Information about blue/green deployment options for a deployment group.
--
-- /Note:/ Consider using 'blueGreenDeploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgBlueGreenDeploymentConfiguration :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe BlueGreenDeploymentConfiguration)
cdgBlueGreenDeploymentConfiguration = Lens.lens (blueGreenDeploymentConfiguration :: CreateDeploymentGroup -> Lude.Maybe BlueGreenDeploymentConfiguration) (\s a -> s {blueGreenDeploymentConfiguration = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgBlueGreenDeploymentConfiguration "Use generic-lens or generic-optics with 'blueGreenDeploymentConfiguration' instead." #-}

-- | Information about the load balancer used in a deployment.
--
-- /Note:/ Consider using 'loadBalancerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgLoadBalancerInfo :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe LoadBalancerInfo)
cdgLoadBalancerInfo = Lens.lens (loadBalancerInfo :: CreateDeploymentGroup -> Lude.Maybe LoadBalancerInfo) (\s a -> s {loadBalancerInfo = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgLoadBalancerInfo "Use generic-lens or generic-optics with 'loadBalancerInfo' instead." #-}

-- | The on-premises instance tags on which to filter. The deployment group includes on-premises instances with any of the specified tags. Cannot be used in the same call as @OnPremisesTagSet@ .
--
-- /Note:/ Consider using 'onPremisesInstanceTagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgOnPremisesInstanceTagFilters :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe [TagFilter])
cdgOnPremisesInstanceTagFilters = Lens.lens (onPremisesInstanceTagFilters :: CreateDeploymentGroup -> Lude.Maybe [TagFilter]) (\s a -> s {onPremisesInstanceTagFilters = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgOnPremisesInstanceTagFilters "Use generic-lens or generic-optics with 'onPremisesInstanceTagFilters' instead." #-}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgApplicationName :: Lens.Lens' CreateDeploymentGroup Lude.Text
cdgApplicationName = Lens.lens (applicationName :: CreateDeploymentGroup -> Lude.Text) (\s a -> s {applicationName = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information to add about Amazon CloudWatch alarms when the deployment group is created.
--
-- /Note:/ Consider using 'alarmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAlarmConfiguration :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe AlarmConfiguration)
cdgAlarmConfiguration = Lens.lens (alarmConfiguration :: CreateDeploymentGroup -> Lude.Maybe AlarmConfiguration) (\s a -> s {alarmConfiguration = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgAlarmConfiguration "Use generic-lens or generic-optics with 'alarmConfiguration' instead." #-}

-- | Information about triggers to create when the deployment group is created. For examples, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'triggerConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgTriggerConfigurations :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe [TriggerConfig])
cdgTriggerConfigurations = Lens.lens (triggerConfigurations :: CreateDeploymentGroup -> Lude.Maybe [TriggerConfig]) (\s a -> s {triggerConfigurations = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgTriggerConfigurations "Use generic-lens or generic-optics with 'triggerConfigurations' instead." #-}

-- | A list of associated Amazon EC2 Auto Scaling groups.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAutoScalingGroups :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe [Lude.Text])
cdgAutoScalingGroups = Lens.lens (autoScalingGroups :: CreateDeploymentGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {autoScalingGroups = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | Information about the type of deployment, in-place or blue/green, that you want to run and whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgDeploymentStyle :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe DeploymentStyle)
cdgDeploymentStyle = Lens.lens (deploymentStyle :: CreateDeploymentGroup -> Lude.Maybe DeploymentStyle) (\s a -> s {deploymentStyle = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgDeploymentStyle "Use generic-lens or generic-optics with 'deploymentStyle' instead." #-}

-- | Configuration information for an automatic rollback that is added when a deployment group is created.
--
-- /Note:/ Consider using 'autoRollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAutoRollbackConfiguration :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe AutoRollbackConfiguration)
cdgAutoRollbackConfiguration = Lens.lens (autoRollbackConfiguration :: CreateDeploymentGroup -> Lude.Maybe AutoRollbackConfiguration) (\s a -> s {autoRollbackConfiguration = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgAutoRollbackConfiguration "Use generic-lens or generic-optics with 'autoRollbackConfiguration' instead." #-}

-- | The name of a new deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgDeploymentGroupName :: Lens.Lens' CreateDeploymentGroup Lude.Text
cdgDeploymentGroupName = Lens.lens (deploymentGroupName :: CreateDeploymentGroup -> Lude.Text) (\s a -> s {deploymentGroupName = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

-- | The metadata that you apply to CodeDeploy deployment groups to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgTags :: Lens.Lens' CreateDeploymentGroup (Lude.Maybe [Tag])
cdgTags = Lens.lens (tags :: CreateDeploymentGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDeploymentGroup)
{-# DEPRECATED cdgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDeploymentGroup where
  type Rs CreateDeploymentGroup = CreateDeploymentGroupResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeploymentGroupResponse'
            Lude.<$> (x Lude..?> "deploymentGroupId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeploymentGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.CreateDeploymentGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDeploymentGroup where
  toJSON CreateDeploymentGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("serviceRoleArn" Lude..= serviceRoleARN),
            ("ec2TagSet" Lude..=) Lude.<$> ec2TagSet,
            ("deploymentConfigName" Lude..=) Lude.<$> deploymentConfigName,
            ("onPremisesTagSet" Lude..=) Lude.<$> onPremisesTagSet,
            ("ec2TagFilters" Lude..=) Lude.<$> ec2TagFilters,
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
              Lude.<$> autoRollbackConfiguration,
            Lude.Just ("deploymentGroupName" Lude..= deploymentGroupName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDeploymentGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDeploymentGroup where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateDeploymentGroup@ operation.
--
-- /See:/ 'mkCreateDeploymentGroupResponse' smart constructor.
data CreateDeploymentGroupResponse = CreateDeploymentGroupResponse'
  { -- | A unique deployment group ID.
    deploymentGroupId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- * 'deploymentGroupId' - A unique deployment group ID.
-- * 'responseStatus' - The response status code.
mkCreateDeploymentGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeploymentGroupResponse
mkCreateDeploymentGroupResponse pResponseStatus_ =
  CreateDeploymentGroupResponse'
    { deploymentGroupId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique deployment group ID.
--
-- /Note:/ Consider using 'deploymentGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgrsDeploymentGroupId :: Lens.Lens' CreateDeploymentGroupResponse (Lude.Maybe Lude.Text)
cdgrsDeploymentGroupId = Lens.lens (deploymentGroupId :: CreateDeploymentGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentGroupId = a} :: CreateDeploymentGroupResponse)
{-# DEPRECATED cdgrsDeploymentGroupId "Use generic-lens or generic-optics with 'deploymentGroupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgrsResponseStatus :: Lens.Lens' CreateDeploymentGroupResponse Lude.Int
cdgrsResponseStatus = Lens.lens (responseStatus :: CreateDeploymentGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeploymentGroupResponse)
{-# DEPRECATED cdgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
