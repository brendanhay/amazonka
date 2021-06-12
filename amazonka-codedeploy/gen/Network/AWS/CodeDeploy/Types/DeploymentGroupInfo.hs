{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentGroupInfo where

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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a deployment group.
--
-- /See:/ 'newDeploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { -- | Information about groups of tags applied to an on-premises instance. The
    -- deployment group includes only on-premises instances identified by all
    -- the tag groups. Cannot be used in the same call as
    -- onPremisesInstanceTagFilters.
    onPremisesTagSet :: Core.Maybe OnPremisesTagSet,
    -- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
    -- permission to make calls to AWS services on your behalf. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
    -- in the /AWS CodeDeploy User Guide/.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | The deployment configuration name.
    deploymentConfigName :: Core.Maybe Core.Text,
    -- | Information about the automatic rollback configuration associated with
    -- the deployment group.
    autoRollbackConfiguration :: Core.Maybe AutoRollbackConfiguration,
    -- | The deployment group name.
    deploymentGroupName :: Core.Maybe Core.Text,
    -- | Information about triggers associated with the deployment group.
    triggerConfigurations :: Core.Maybe [TriggerConfig],
    -- | The deployment group ID.
    deploymentGroupId :: Core.Maybe Core.Text,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes
    -- EC2 instances with any of the specified tags.
    ec2TagFilters :: Core.Maybe [EC2TagFilter],
    -- | Information about the deployment group\'s target revision, including
    -- type and location.
    targetRevision :: Core.Maybe RevisionLocation,
    -- | Information about the most recent successful deployment to the
    -- deployment group.
    lastSuccessfulDeployment :: Core.Maybe LastDeploymentInfo,
    -- | Information about the most recent attempted deployment to the deployment
    -- group.
    lastAttemptedDeployment :: Core.Maybe LastDeploymentInfo,
    -- | The on-premises instance tags on which to filter. The deployment group
    -- includes on-premises instances with any of the specified tags.
    onPremisesInstanceTagFilters :: Core.Maybe [TagFilter],
    -- | Information about the load balancer to use in a deployment.
    loadBalancerInfo :: Core.Maybe LoadBalancerInfo,
    -- | Information about groups of tags applied to an EC2 instance. The
    -- deployment group includes only EC2 instances identified by all of the
    -- tag groups. Cannot be used in the same call as ec2TagFilters.
    ec2TagSet :: Core.Maybe EC2TagSet,
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Core.Maybe BlueGreenDeploymentConfiguration,
    -- | A list of associated Auto Scaling groups.
    autoScalingGroups :: Core.Maybe [AutoScalingGroup],
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Core.Maybe DeploymentStyle,
    -- | A list of alarms associated with the deployment group.
    alarmConfiguration :: Core.Maybe AlarmConfiguration,
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Core.Maybe [ECSService],
    -- | The application name.
    applicationName :: Core.Maybe Core.Text,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Core.Maybe ComputePlatform
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeploymentGroupInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onPremisesTagSet', 'deploymentGroupInfo_onPremisesTagSet' - Information about groups of tags applied to an on-premises instance. The
-- deployment group includes only on-premises instances identified by all
-- the tag groups. Cannot be used in the same call as
-- onPremisesInstanceTagFilters.
--
-- 'serviceRoleArn', 'deploymentGroupInfo_serviceRoleArn' - A service role Amazon Resource Name (ARN) that grants CodeDeploy
-- permission to make calls to AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
-- in the /AWS CodeDeploy User Guide/.
--
-- 'deploymentConfigName', 'deploymentGroupInfo_deploymentConfigName' - The deployment configuration name.
--
-- 'autoRollbackConfiguration', 'deploymentGroupInfo_autoRollbackConfiguration' - Information about the automatic rollback configuration associated with
-- the deployment group.
--
-- 'deploymentGroupName', 'deploymentGroupInfo_deploymentGroupName' - The deployment group name.
--
-- 'triggerConfigurations', 'deploymentGroupInfo_triggerConfigurations' - Information about triggers associated with the deployment group.
--
-- 'deploymentGroupId', 'deploymentGroupInfo_deploymentGroupId' - The deployment group ID.
--
-- 'ec2TagFilters', 'deploymentGroupInfo_ec2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags.
--
-- 'targetRevision', 'deploymentGroupInfo_targetRevision' - Information about the deployment group\'s target revision, including
-- type and location.
--
-- 'lastSuccessfulDeployment', 'deploymentGroupInfo_lastSuccessfulDeployment' - Information about the most recent successful deployment to the
-- deployment group.
--
-- 'lastAttemptedDeployment', 'deploymentGroupInfo_lastAttemptedDeployment' - Information about the most recent attempted deployment to the deployment
-- group.
--
-- 'onPremisesInstanceTagFilters', 'deploymentGroupInfo_onPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags.
--
-- 'loadBalancerInfo', 'deploymentGroupInfo_loadBalancerInfo' - Information about the load balancer to use in a deployment.
--
-- 'ec2TagSet', 'deploymentGroupInfo_ec2TagSet' - Information about groups of tags applied to an EC2 instance. The
-- deployment group includes only EC2 instances identified by all of the
-- tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- 'blueGreenDeploymentConfiguration', 'deploymentGroupInfo_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for a deployment group.
--
-- 'autoScalingGroups', 'deploymentGroupInfo_autoScalingGroups' - A list of associated Auto Scaling groups.
--
-- 'deploymentStyle', 'deploymentGroupInfo_deploymentStyle' - Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- 'alarmConfiguration', 'deploymentGroupInfo_alarmConfiguration' - A list of alarms associated with the deployment group.
--
-- 'ecsServices', 'deploymentGroupInfo_ecsServices' - The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
--
-- 'applicationName', 'deploymentGroupInfo_applicationName' - The application name.
--
-- 'computePlatform', 'deploymentGroupInfo_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
newDeploymentGroupInfo ::
  DeploymentGroupInfo
newDeploymentGroupInfo =
  DeploymentGroupInfo'
    { onPremisesTagSet =
        Core.Nothing,
      serviceRoleArn = Core.Nothing,
      deploymentConfigName = Core.Nothing,
      autoRollbackConfiguration = Core.Nothing,
      deploymentGroupName = Core.Nothing,
      triggerConfigurations = Core.Nothing,
      deploymentGroupId = Core.Nothing,
      ec2TagFilters = Core.Nothing,
      targetRevision = Core.Nothing,
      lastSuccessfulDeployment = Core.Nothing,
      lastAttemptedDeployment = Core.Nothing,
      onPremisesInstanceTagFilters = Core.Nothing,
      loadBalancerInfo = Core.Nothing,
      ec2TagSet = Core.Nothing,
      blueGreenDeploymentConfiguration = Core.Nothing,
      autoScalingGroups = Core.Nothing,
      deploymentStyle = Core.Nothing,
      alarmConfiguration = Core.Nothing,
      ecsServices = Core.Nothing,
      applicationName = Core.Nothing,
      computePlatform = Core.Nothing
    }

-- | Information about groups of tags applied to an on-premises instance. The
-- deployment group includes only on-premises instances identified by all
-- the tag groups. Cannot be used in the same call as
-- onPremisesInstanceTagFilters.
deploymentGroupInfo_onPremisesTagSet :: Lens.Lens' DeploymentGroupInfo (Core.Maybe OnPremisesTagSet)
deploymentGroupInfo_onPremisesTagSet = Lens.lens (\DeploymentGroupInfo' {onPremisesTagSet} -> onPremisesTagSet) (\s@DeploymentGroupInfo' {} a -> s {onPremisesTagSet = a} :: DeploymentGroupInfo)

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
-- permission to make calls to AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
-- in the /AWS CodeDeploy User Guide/.
deploymentGroupInfo_serviceRoleArn :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Core.Text)
deploymentGroupInfo_serviceRoleArn = Lens.lens (\DeploymentGroupInfo' {serviceRoleArn} -> serviceRoleArn) (\s@DeploymentGroupInfo' {} a -> s {serviceRoleArn = a} :: DeploymentGroupInfo)

-- | The deployment configuration name.
deploymentGroupInfo_deploymentConfigName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Core.Text)
deploymentGroupInfo_deploymentConfigName = Lens.lens (\DeploymentGroupInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentGroupInfo' {} a -> s {deploymentConfigName = a} :: DeploymentGroupInfo)

-- | Information about the automatic rollback configuration associated with
-- the deployment group.
deploymentGroupInfo_autoRollbackConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe AutoRollbackConfiguration)
deploymentGroupInfo_autoRollbackConfiguration = Lens.lens (\DeploymentGroupInfo' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentGroupInfo' {} a -> s {autoRollbackConfiguration = a} :: DeploymentGroupInfo)

-- | The deployment group name.
deploymentGroupInfo_deploymentGroupName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Core.Text)
deploymentGroupInfo_deploymentGroupName = Lens.lens (\DeploymentGroupInfo' {deploymentGroupName} -> deploymentGroupName) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupName = a} :: DeploymentGroupInfo)

-- | Information about triggers associated with the deployment group.
deploymentGroupInfo_triggerConfigurations :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [TriggerConfig])
deploymentGroupInfo_triggerConfigurations = Lens.lens (\DeploymentGroupInfo' {triggerConfigurations} -> triggerConfigurations) (\s@DeploymentGroupInfo' {} a -> s {triggerConfigurations = a} :: DeploymentGroupInfo) Core.. Lens.mapping Lens._Coerce

-- | The deployment group ID.
deploymentGroupInfo_deploymentGroupId :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Core.Text)
deploymentGroupInfo_deploymentGroupId = Lens.lens (\DeploymentGroupInfo' {deploymentGroupId} -> deploymentGroupId) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupId = a} :: DeploymentGroupInfo)

-- | The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags.
deploymentGroupInfo_ec2TagFilters :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [EC2TagFilter])
deploymentGroupInfo_ec2TagFilters = Lens.lens (\DeploymentGroupInfo' {ec2TagFilters} -> ec2TagFilters) (\s@DeploymentGroupInfo' {} a -> s {ec2TagFilters = a} :: DeploymentGroupInfo) Core.. Lens.mapping Lens._Coerce

-- | Information about the deployment group\'s target revision, including
-- type and location.
deploymentGroupInfo_targetRevision :: Lens.Lens' DeploymentGroupInfo (Core.Maybe RevisionLocation)
deploymentGroupInfo_targetRevision = Lens.lens (\DeploymentGroupInfo' {targetRevision} -> targetRevision) (\s@DeploymentGroupInfo' {} a -> s {targetRevision = a} :: DeploymentGroupInfo)

-- | Information about the most recent successful deployment to the
-- deployment group.
deploymentGroupInfo_lastSuccessfulDeployment :: Lens.Lens' DeploymentGroupInfo (Core.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastSuccessfulDeployment = Lens.lens (\DeploymentGroupInfo' {lastSuccessfulDeployment} -> lastSuccessfulDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastSuccessfulDeployment = a} :: DeploymentGroupInfo)

-- | Information about the most recent attempted deployment to the deployment
-- group.
deploymentGroupInfo_lastAttemptedDeployment :: Lens.Lens' DeploymentGroupInfo (Core.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastAttemptedDeployment = Lens.lens (\DeploymentGroupInfo' {lastAttemptedDeployment} -> lastAttemptedDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastAttemptedDeployment = a} :: DeploymentGroupInfo)

-- | The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags.
deploymentGroupInfo_onPremisesInstanceTagFilters :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [TagFilter])
deploymentGroupInfo_onPremisesInstanceTagFilters = Lens.lens (\DeploymentGroupInfo' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@DeploymentGroupInfo' {} a -> s {onPremisesInstanceTagFilters = a} :: DeploymentGroupInfo) Core.. Lens.mapping Lens._Coerce

-- | Information about the load balancer to use in a deployment.
deploymentGroupInfo_loadBalancerInfo :: Lens.Lens' DeploymentGroupInfo (Core.Maybe LoadBalancerInfo)
deploymentGroupInfo_loadBalancerInfo = Lens.lens (\DeploymentGroupInfo' {loadBalancerInfo} -> loadBalancerInfo) (\s@DeploymentGroupInfo' {} a -> s {loadBalancerInfo = a} :: DeploymentGroupInfo)

-- | Information about groups of tags applied to an EC2 instance. The
-- deployment group includes only EC2 instances identified by all of the
-- tag groups. Cannot be used in the same call as ec2TagFilters.
deploymentGroupInfo_ec2TagSet :: Lens.Lens' DeploymentGroupInfo (Core.Maybe EC2TagSet)
deploymentGroupInfo_ec2TagSet = Lens.lens (\DeploymentGroupInfo' {ec2TagSet} -> ec2TagSet) (\s@DeploymentGroupInfo' {} a -> s {ec2TagSet = a} :: DeploymentGroupInfo)

-- | Information about blue\/green deployment options for a deployment group.
deploymentGroupInfo_blueGreenDeploymentConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe BlueGreenDeploymentConfiguration)
deploymentGroupInfo_blueGreenDeploymentConfiguration = Lens.lens (\DeploymentGroupInfo' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@DeploymentGroupInfo' {} a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentGroupInfo)

-- | A list of associated Auto Scaling groups.
deploymentGroupInfo_autoScalingGroups :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [AutoScalingGroup])
deploymentGroupInfo_autoScalingGroups = Lens.lens (\DeploymentGroupInfo' {autoScalingGroups} -> autoScalingGroups) (\s@DeploymentGroupInfo' {} a -> s {autoScalingGroups = a} :: DeploymentGroupInfo) Core.. Lens.mapping Lens._Coerce

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
deploymentGroupInfo_deploymentStyle :: Lens.Lens' DeploymentGroupInfo (Core.Maybe DeploymentStyle)
deploymentGroupInfo_deploymentStyle = Lens.lens (\DeploymentGroupInfo' {deploymentStyle} -> deploymentStyle) (\s@DeploymentGroupInfo' {} a -> s {deploymentStyle = a} :: DeploymentGroupInfo)

-- | A list of alarms associated with the deployment group.
deploymentGroupInfo_alarmConfiguration :: Lens.Lens' DeploymentGroupInfo (Core.Maybe AlarmConfiguration)
deploymentGroupInfo_alarmConfiguration = Lens.lens (\DeploymentGroupInfo' {alarmConfiguration} -> alarmConfiguration) (\s@DeploymentGroupInfo' {} a -> s {alarmConfiguration = a} :: DeploymentGroupInfo)

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
deploymentGroupInfo_ecsServices :: Lens.Lens' DeploymentGroupInfo (Core.Maybe [ECSService])
deploymentGroupInfo_ecsServices = Lens.lens (\DeploymentGroupInfo' {ecsServices} -> ecsServices) (\s@DeploymentGroupInfo' {} a -> s {ecsServices = a} :: DeploymentGroupInfo) Core.. Lens.mapping Lens._Coerce

-- | The application name.
deploymentGroupInfo_applicationName :: Lens.Lens' DeploymentGroupInfo (Core.Maybe Core.Text)
deploymentGroupInfo_applicationName = Lens.lens (\DeploymentGroupInfo' {applicationName} -> applicationName) (\s@DeploymentGroupInfo' {} a -> s {applicationName = a} :: DeploymentGroupInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentGroupInfo_computePlatform :: Lens.Lens' DeploymentGroupInfo (Core.Maybe ComputePlatform)
deploymentGroupInfo_computePlatform = Lens.lens (\DeploymentGroupInfo' {computePlatform} -> computePlatform) (\s@DeploymentGroupInfo' {} a -> s {computePlatform = a} :: DeploymentGroupInfo)

instance Core.FromJSON DeploymentGroupInfo where
  parseJSON =
    Core.withObject
      "DeploymentGroupInfo"
      ( \x ->
          DeploymentGroupInfo'
            Core.<$> (x Core..:? "onPremisesTagSet")
            Core.<*> (x Core..:? "serviceRoleArn")
            Core.<*> (x Core..:? "deploymentConfigName")
            Core.<*> (x Core..:? "autoRollbackConfiguration")
            Core.<*> (x Core..:? "deploymentGroupName")
            Core.<*> ( x Core..:? "triggerConfigurations"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "deploymentGroupId")
            Core.<*> (x Core..:? "ec2TagFilters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "targetRevision")
            Core.<*> (x Core..:? "lastSuccessfulDeployment")
            Core.<*> (x Core..:? "lastAttemptedDeployment")
            Core.<*> ( x Core..:? "onPremisesInstanceTagFilters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "loadBalancerInfo")
            Core.<*> (x Core..:? "ec2TagSet")
            Core.<*> (x Core..:? "blueGreenDeploymentConfiguration")
            Core.<*> (x Core..:? "autoScalingGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "deploymentStyle")
            Core.<*> (x Core..:? "alarmConfiguration")
            Core.<*> (x Core..:? "ecsServices" Core..!= Core.mempty)
            Core.<*> (x Core..:? "applicationName")
            Core.<*> (x Core..:? "computePlatform")
      )

instance Core.Hashable DeploymentGroupInfo

instance Core.NFData DeploymentGroupInfo
