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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentGroupInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentGroupInfo where

import Amazonka.CodeDeploy.Types.AlarmConfiguration
import Amazonka.CodeDeploy.Types.AutoRollbackConfiguration
import Amazonka.CodeDeploy.Types.AutoScalingGroup
import Amazonka.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Amazonka.CodeDeploy.Types.ComputePlatform
import Amazonka.CodeDeploy.Types.DeploymentStyle
import Amazonka.CodeDeploy.Types.EC2TagFilter
import Amazonka.CodeDeploy.Types.EC2TagSet
import Amazonka.CodeDeploy.Types.ECSService
import Amazonka.CodeDeploy.Types.LastDeploymentInfo
import Amazonka.CodeDeploy.Types.LoadBalancerInfo
import Amazonka.CodeDeploy.Types.OnPremisesTagSet
import Amazonka.CodeDeploy.Types.OutdatedInstancesStrategy
import Amazonka.CodeDeploy.Types.RevisionLocation
import Amazonka.CodeDeploy.Types.TagFilter
import Amazonka.CodeDeploy.Types.TriggerConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a deployment group.
--
-- /See:/ 'newDeploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { -- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
    -- permission to make calls to AWS services on your behalf. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
    -- in the /AWS CodeDeploy User Guide/.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Information about groups of tags applied to an EC2 instance. The
    -- deployment group includes only EC2 instances identified by all of the
    -- tag groups. Cannot be used in the same call as ec2TagFilters.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | The deployment configuration name.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | Information about the most recent attempted deployment to the deployment
    -- group.
    lastAttemptedDeployment :: Prelude.Maybe LastDeploymentInfo,
    -- | Information about groups of tags applied to an on-premises instance. The
    -- deployment group includes only on-premises instances identified by all
    -- the tag groups. Cannot be used in the same call as
    -- onPremisesInstanceTagFilters.
    onPremisesTagSet :: Prelude.Maybe OnPremisesTagSet,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform,
    -- | Information about the deployment group\'s target revision, including
    -- type and location.
    targetRevision :: Prelude.Maybe RevisionLocation,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes
    -- EC2 instances with any of the specified tags.
    ec2TagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Prelude.Maybe [ECSService],
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | Information about the load balancer to use in a deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | Indicates what happens when new EC2 instances are launched
    -- mid-deployment and do not receive the deployed application revision.
    --
    -- If this option is set to @UPDATE@ or is unspecified, CodeDeploy
    -- initiates one or more \'auto-update outdated instances\' deployments to
    -- apply the deployed application revision to the new EC2 instances.
    --
    -- If this option is set to @IGNORE@, CodeDeploy does not initiate a
    -- deployment to update the new EC2 instances. This may result in instances
    -- having different revisions.
    outdatedInstancesStrategy :: Prelude.Maybe OutdatedInstancesStrategy,
    -- | The on-premises instance tags on which to filter. The deployment group
    -- includes on-premises instances with any of the specified tags.
    onPremisesInstanceTagFilters :: Prelude.Maybe [TagFilter],
    -- | Information about the most recent successful deployment to the
    -- deployment group.
    lastSuccessfulDeployment :: Prelude.Maybe LastDeploymentInfo,
    -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | A list of alarms associated with the deployment group.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | Information about triggers associated with the deployment group.
    triggerConfigurations :: Prelude.Maybe [TriggerConfig],
    -- | The deployment group ID.
    deploymentGroupId :: Prelude.Maybe Prelude.Text,
    -- | A list of associated Auto Scaling groups.
    autoScalingGroups :: Prelude.Maybe [AutoScalingGroup],
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | Information about the automatic rollback configuration associated with
    -- the deployment group.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | The deployment group name.
    deploymentGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentGroupInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceRoleArn', 'deploymentGroupInfo_serviceRoleArn' - A service role Amazon Resource Name (ARN) that grants CodeDeploy
-- permission to make calls to AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
-- in the /AWS CodeDeploy User Guide/.
--
-- 'ec2TagSet', 'deploymentGroupInfo_ec2TagSet' - Information about groups of tags applied to an EC2 instance. The
-- deployment group includes only EC2 instances identified by all of the
-- tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- 'deploymentConfigName', 'deploymentGroupInfo_deploymentConfigName' - The deployment configuration name.
--
-- 'lastAttemptedDeployment', 'deploymentGroupInfo_lastAttemptedDeployment' - Information about the most recent attempted deployment to the deployment
-- group.
--
-- 'onPremisesTagSet', 'deploymentGroupInfo_onPremisesTagSet' - Information about groups of tags applied to an on-premises instance. The
-- deployment group includes only on-premises instances identified by all
-- the tag groups. Cannot be used in the same call as
-- onPremisesInstanceTagFilters.
--
-- 'computePlatform', 'deploymentGroupInfo_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
--
-- 'targetRevision', 'deploymentGroupInfo_targetRevision' - Information about the deployment group\'s target revision, including
-- type and location.
--
-- 'ec2TagFilters', 'deploymentGroupInfo_ec2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags.
--
-- 'ecsServices', 'deploymentGroupInfo_ecsServices' - The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
--
-- 'blueGreenDeploymentConfiguration', 'deploymentGroupInfo_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for a deployment group.
--
-- 'loadBalancerInfo', 'deploymentGroupInfo_loadBalancerInfo' - Information about the load balancer to use in a deployment.
--
-- 'outdatedInstancesStrategy', 'deploymentGroupInfo_outdatedInstancesStrategy' - Indicates what happens when new EC2 instances are launched
-- mid-deployment and do not receive the deployed application revision.
--
-- If this option is set to @UPDATE@ or is unspecified, CodeDeploy
-- initiates one or more \'auto-update outdated instances\' deployments to
-- apply the deployed application revision to the new EC2 instances.
--
-- If this option is set to @IGNORE@, CodeDeploy does not initiate a
-- deployment to update the new EC2 instances. This may result in instances
-- having different revisions.
--
-- 'onPremisesInstanceTagFilters', 'deploymentGroupInfo_onPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags.
--
-- 'lastSuccessfulDeployment', 'deploymentGroupInfo_lastSuccessfulDeployment' - Information about the most recent successful deployment to the
-- deployment group.
--
-- 'applicationName', 'deploymentGroupInfo_applicationName' - The application name.
--
-- 'alarmConfiguration', 'deploymentGroupInfo_alarmConfiguration' - A list of alarms associated with the deployment group.
--
-- 'triggerConfigurations', 'deploymentGroupInfo_triggerConfigurations' - Information about triggers associated with the deployment group.
--
-- 'deploymentGroupId', 'deploymentGroupInfo_deploymentGroupId' - The deployment group ID.
--
-- 'autoScalingGroups', 'deploymentGroupInfo_autoScalingGroups' - A list of associated Auto Scaling groups.
--
-- 'deploymentStyle', 'deploymentGroupInfo_deploymentStyle' - Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- 'autoRollbackConfiguration', 'deploymentGroupInfo_autoRollbackConfiguration' - Information about the automatic rollback configuration associated with
-- the deployment group.
--
-- 'deploymentGroupName', 'deploymentGroupInfo_deploymentGroupName' - The deployment group name.
newDeploymentGroupInfo ::
  DeploymentGroupInfo
newDeploymentGroupInfo =
  DeploymentGroupInfo'
    { serviceRoleArn =
        Prelude.Nothing,
      ec2TagSet = Prelude.Nothing,
      deploymentConfigName = Prelude.Nothing,
      lastAttemptedDeployment = Prelude.Nothing,
      onPremisesTagSet = Prelude.Nothing,
      computePlatform = Prelude.Nothing,
      targetRevision = Prelude.Nothing,
      ec2TagFilters = Prelude.Nothing,
      ecsServices = Prelude.Nothing,
      blueGreenDeploymentConfiguration = Prelude.Nothing,
      loadBalancerInfo = Prelude.Nothing,
      outdatedInstancesStrategy = Prelude.Nothing,
      onPremisesInstanceTagFilters = Prelude.Nothing,
      lastSuccessfulDeployment = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      alarmConfiguration = Prelude.Nothing,
      triggerConfigurations = Prelude.Nothing,
      deploymentGroupId = Prelude.Nothing,
      autoScalingGroups = Prelude.Nothing,
      deploymentStyle = Prelude.Nothing,
      autoRollbackConfiguration = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing
    }

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
-- permission to make calls to AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
-- in the /AWS CodeDeploy User Guide/.
deploymentGroupInfo_serviceRoleArn :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_serviceRoleArn = Lens.lens (\DeploymentGroupInfo' {serviceRoleArn} -> serviceRoleArn) (\s@DeploymentGroupInfo' {} a -> s {serviceRoleArn = a} :: DeploymentGroupInfo)

-- | Information about groups of tags applied to an EC2 instance. The
-- deployment group includes only EC2 instances identified by all of the
-- tag groups. Cannot be used in the same call as ec2TagFilters.
deploymentGroupInfo_ec2TagSet :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe EC2TagSet)
deploymentGroupInfo_ec2TagSet = Lens.lens (\DeploymentGroupInfo' {ec2TagSet} -> ec2TagSet) (\s@DeploymentGroupInfo' {} a -> s {ec2TagSet = a} :: DeploymentGroupInfo)

-- | The deployment configuration name.
deploymentGroupInfo_deploymentConfigName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentConfigName = Lens.lens (\DeploymentGroupInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentGroupInfo' {} a -> s {deploymentConfigName = a} :: DeploymentGroupInfo)

-- | Information about the most recent attempted deployment to the deployment
-- group.
deploymentGroupInfo_lastAttemptedDeployment :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastAttemptedDeployment = Lens.lens (\DeploymentGroupInfo' {lastAttemptedDeployment} -> lastAttemptedDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastAttemptedDeployment = a} :: DeploymentGroupInfo)

-- | Information about groups of tags applied to an on-premises instance. The
-- deployment group includes only on-premises instances identified by all
-- the tag groups. Cannot be used in the same call as
-- onPremisesInstanceTagFilters.
deploymentGroupInfo_onPremisesTagSet :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe OnPremisesTagSet)
deploymentGroupInfo_onPremisesTagSet = Lens.lens (\DeploymentGroupInfo' {onPremisesTagSet} -> onPremisesTagSet) (\s@DeploymentGroupInfo' {} a -> s {onPremisesTagSet = a} :: DeploymentGroupInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentGroupInfo_computePlatform :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe ComputePlatform)
deploymentGroupInfo_computePlatform = Lens.lens (\DeploymentGroupInfo' {computePlatform} -> computePlatform) (\s@DeploymentGroupInfo' {} a -> s {computePlatform = a} :: DeploymentGroupInfo)

-- | Information about the deployment group\'s target revision, including
-- type and location.
deploymentGroupInfo_targetRevision :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe RevisionLocation)
deploymentGroupInfo_targetRevision = Lens.lens (\DeploymentGroupInfo' {targetRevision} -> targetRevision) (\s@DeploymentGroupInfo' {} a -> s {targetRevision = a} :: DeploymentGroupInfo)

-- | The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags.
deploymentGroupInfo_ec2TagFilters :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [EC2TagFilter])
deploymentGroupInfo_ec2TagFilters = Lens.lens (\DeploymentGroupInfo' {ec2TagFilters} -> ec2TagFilters) (\s@DeploymentGroupInfo' {} a -> s {ec2TagFilters = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
deploymentGroupInfo_ecsServices :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [ECSService])
deploymentGroupInfo_ecsServices = Lens.lens (\DeploymentGroupInfo' {ecsServices} -> ecsServices) (\s@DeploymentGroupInfo' {} a -> s {ecsServices = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about blue\/green deployment options for a deployment group.
deploymentGroupInfo_blueGreenDeploymentConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe BlueGreenDeploymentConfiguration)
deploymentGroupInfo_blueGreenDeploymentConfiguration = Lens.lens (\DeploymentGroupInfo' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@DeploymentGroupInfo' {} a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentGroupInfo)

-- | Information about the load balancer to use in a deployment.
deploymentGroupInfo_loadBalancerInfo :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LoadBalancerInfo)
deploymentGroupInfo_loadBalancerInfo = Lens.lens (\DeploymentGroupInfo' {loadBalancerInfo} -> loadBalancerInfo) (\s@DeploymentGroupInfo' {} a -> s {loadBalancerInfo = a} :: DeploymentGroupInfo)

-- | Indicates what happens when new EC2 instances are launched
-- mid-deployment and do not receive the deployed application revision.
--
-- If this option is set to @UPDATE@ or is unspecified, CodeDeploy
-- initiates one or more \'auto-update outdated instances\' deployments to
-- apply the deployed application revision to the new EC2 instances.
--
-- If this option is set to @IGNORE@, CodeDeploy does not initiate a
-- deployment to update the new EC2 instances. This may result in instances
-- having different revisions.
deploymentGroupInfo_outdatedInstancesStrategy :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe OutdatedInstancesStrategy)
deploymentGroupInfo_outdatedInstancesStrategy = Lens.lens (\DeploymentGroupInfo' {outdatedInstancesStrategy} -> outdatedInstancesStrategy) (\s@DeploymentGroupInfo' {} a -> s {outdatedInstancesStrategy = a} :: DeploymentGroupInfo)

-- | The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags.
deploymentGroupInfo_onPremisesInstanceTagFilters :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [TagFilter])
deploymentGroupInfo_onPremisesInstanceTagFilters = Lens.lens (\DeploymentGroupInfo' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@DeploymentGroupInfo' {} a -> s {onPremisesInstanceTagFilters = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about the most recent successful deployment to the
-- deployment group.
deploymentGroupInfo_lastSuccessfulDeployment :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastSuccessfulDeployment = Lens.lens (\DeploymentGroupInfo' {lastSuccessfulDeployment} -> lastSuccessfulDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastSuccessfulDeployment = a} :: DeploymentGroupInfo)

-- | The application name.
deploymentGroupInfo_applicationName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_applicationName = Lens.lens (\DeploymentGroupInfo' {applicationName} -> applicationName) (\s@DeploymentGroupInfo' {} a -> s {applicationName = a} :: DeploymentGroupInfo)

-- | A list of alarms associated with the deployment group.
deploymentGroupInfo_alarmConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe AlarmConfiguration)
deploymentGroupInfo_alarmConfiguration = Lens.lens (\DeploymentGroupInfo' {alarmConfiguration} -> alarmConfiguration) (\s@DeploymentGroupInfo' {} a -> s {alarmConfiguration = a} :: DeploymentGroupInfo)

-- | Information about triggers associated with the deployment group.
deploymentGroupInfo_triggerConfigurations :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [TriggerConfig])
deploymentGroupInfo_triggerConfigurations = Lens.lens (\DeploymentGroupInfo' {triggerConfigurations} -> triggerConfigurations) (\s@DeploymentGroupInfo' {} a -> s {triggerConfigurations = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | The deployment group ID.
deploymentGroupInfo_deploymentGroupId :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentGroupId = Lens.lens (\DeploymentGroupInfo' {deploymentGroupId} -> deploymentGroupId) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupId = a} :: DeploymentGroupInfo)

-- | A list of associated Auto Scaling groups.
deploymentGroupInfo_autoScalingGroups :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [AutoScalingGroup])
deploymentGroupInfo_autoScalingGroups = Lens.lens (\DeploymentGroupInfo' {autoScalingGroups} -> autoScalingGroups) (\s@DeploymentGroupInfo' {} a -> s {autoScalingGroups = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
deploymentGroupInfo_deploymentStyle :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe DeploymentStyle)
deploymentGroupInfo_deploymentStyle = Lens.lens (\DeploymentGroupInfo' {deploymentStyle} -> deploymentStyle) (\s@DeploymentGroupInfo' {} a -> s {deploymentStyle = a} :: DeploymentGroupInfo)

-- | Information about the automatic rollback configuration associated with
-- the deployment group.
deploymentGroupInfo_autoRollbackConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe AutoRollbackConfiguration)
deploymentGroupInfo_autoRollbackConfiguration = Lens.lens (\DeploymentGroupInfo' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentGroupInfo' {} a -> s {autoRollbackConfiguration = a} :: DeploymentGroupInfo)

-- | The deployment group name.
deploymentGroupInfo_deploymentGroupName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentGroupName = Lens.lens (\DeploymentGroupInfo' {deploymentGroupName} -> deploymentGroupName) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupName = a} :: DeploymentGroupInfo)

instance Core.FromJSON DeploymentGroupInfo where
  parseJSON =
    Core.withObject
      "DeploymentGroupInfo"
      ( \x ->
          DeploymentGroupInfo'
            Prelude.<$> (x Core..:? "serviceRoleArn")
            Prelude.<*> (x Core..:? "ec2TagSet")
            Prelude.<*> (x Core..:? "deploymentConfigName")
            Prelude.<*> (x Core..:? "lastAttemptedDeployment")
            Prelude.<*> (x Core..:? "onPremisesTagSet")
            Prelude.<*> (x Core..:? "computePlatform")
            Prelude.<*> (x Core..:? "targetRevision")
            Prelude.<*> (x Core..:? "ec2TagFilters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ecsServices" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "blueGreenDeploymentConfiguration")
            Prelude.<*> (x Core..:? "loadBalancerInfo")
            Prelude.<*> (x Core..:? "outdatedInstancesStrategy")
            Prelude.<*> ( x Core..:? "onPremisesInstanceTagFilters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lastSuccessfulDeployment")
            Prelude.<*> (x Core..:? "applicationName")
            Prelude.<*> (x Core..:? "alarmConfiguration")
            Prelude.<*> ( x Core..:? "triggerConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "deploymentGroupId")
            Prelude.<*> ( x Core..:? "autoScalingGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "deploymentStyle")
            Prelude.<*> (x Core..:? "autoRollbackConfiguration")
            Prelude.<*> (x Core..:? "deploymentGroupName")
      )

instance Prelude.Hashable DeploymentGroupInfo

instance Prelude.NFData DeploymentGroupInfo
