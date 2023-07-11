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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a deployment group.
--
-- /See:/ 'newDeploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { -- | A list of alarms associated with the deployment group.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | Information about the automatic rollback configuration associated with
    -- the deployment group.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | A list of associated Auto Scaling groups.
    autoScalingGroups :: Prelude.Maybe [AutoScalingGroup],
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform,
    -- | The deployment configuration name.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | The deployment group ID.
    deploymentGroupId :: Prelude.Maybe Prelude.Text,
    -- | The deployment group name.
    deploymentGroupName :: Prelude.Maybe Prelude.Text,
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes
    -- EC2 instances with any of the specified tags.
    ec2TagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | Information about groups of tags applied to an Amazon EC2 instance. The
    -- deployment group includes only Amazon EC2 instances identified by all of
    -- the tag groups. Cannot be used in the same call as ec2TagFilters.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Prelude.Maybe [ECSService],
    -- | Information about the most recent attempted deployment to the deployment
    -- group.
    lastAttemptedDeployment :: Prelude.Maybe LastDeploymentInfo,
    -- | Information about the most recent successful deployment to the
    -- deployment group.
    lastSuccessfulDeployment :: Prelude.Maybe LastDeploymentInfo,
    -- | Information about the load balancer to use in a deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | The on-premises instance tags on which to filter. The deployment group
    -- includes on-premises instances with any of the specified tags.
    onPremisesInstanceTagFilters :: Prelude.Maybe [TagFilter],
    -- | Information about groups of tags applied to an on-premises instance. The
    -- deployment group includes only on-premises instances identified by all
    -- the tag groups. Cannot be used in the same call as
    -- onPremisesInstanceTagFilters.
    onPremisesTagSet :: Prelude.Maybe OnPremisesTagSet,
    -- | Indicates what happens when new Amazon EC2 instances are launched
    -- mid-deployment and do not receive the deployed application revision.
    --
    -- If this option is set to @UPDATE@ or is unspecified, CodeDeploy
    -- initiates one or more \'auto-update outdated instances\' deployments to
    -- apply the deployed application revision to the new Amazon EC2 instances.
    --
    -- If this option is set to @IGNORE@, CodeDeploy does not initiate a
    -- deployment to update the new Amazon EC2 instances. This may result in
    -- instances having different revisions.
    outdatedInstancesStrategy :: Prelude.Maybe OutdatedInstancesStrategy,
    -- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
    -- permission to make calls to Amazon Web Services services on your behalf.
    -- For more information, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for CodeDeploy>
    -- in the /CodeDeploy User Guide/.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the deployment group\'s target revision, including
    -- type and location.
    targetRevision :: Prelude.Maybe RevisionLocation,
    -- | Information about triggers associated with the deployment group.
    triggerConfigurations :: Prelude.Maybe [TriggerConfig]
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
-- 'alarmConfiguration', 'deploymentGroupInfo_alarmConfiguration' - A list of alarms associated with the deployment group.
--
-- 'applicationName', 'deploymentGroupInfo_applicationName' - The application name.
--
-- 'autoRollbackConfiguration', 'deploymentGroupInfo_autoRollbackConfiguration' - Information about the automatic rollback configuration associated with
-- the deployment group.
--
-- 'autoScalingGroups', 'deploymentGroupInfo_autoScalingGroups' - A list of associated Auto Scaling groups.
--
-- 'blueGreenDeploymentConfiguration', 'deploymentGroupInfo_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for a deployment group.
--
-- 'computePlatform', 'deploymentGroupInfo_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
--
-- 'deploymentConfigName', 'deploymentGroupInfo_deploymentConfigName' - The deployment configuration name.
--
-- 'deploymentGroupId', 'deploymentGroupInfo_deploymentGroupId' - The deployment group ID.
--
-- 'deploymentGroupName', 'deploymentGroupInfo_deploymentGroupName' - The deployment group name.
--
-- 'deploymentStyle', 'deploymentGroupInfo_deploymentStyle' - Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- 'ec2TagFilters', 'deploymentGroupInfo_ec2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags.
--
-- 'ec2TagSet', 'deploymentGroupInfo_ec2TagSet' - Information about groups of tags applied to an Amazon EC2 instance. The
-- deployment group includes only Amazon EC2 instances identified by all of
-- the tag groups. Cannot be used in the same call as ec2TagFilters.
--
-- 'ecsServices', 'deploymentGroupInfo_ecsServices' - The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
--
-- 'lastAttemptedDeployment', 'deploymentGroupInfo_lastAttemptedDeployment' - Information about the most recent attempted deployment to the deployment
-- group.
--
-- 'lastSuccessfulDeployment', 'deploymentGroupInfo_lastSuccessfulDeployment' - Information about the most recent successful deployment to the
-- deployment group.
--
-- 'loadBalancerInfo', 'deploymentGroupInfo_loadBalancerInfo' - Information about the load balancer to use in a deployment.
--
-- 'onPremisesInstanceTagFilters', 'deploymentGroupInfo_onPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags.
--
-- 'onPremisesTagSet', 'deploymentGroupInfo_onPremisesTagSet' - Information about groups of tags applied to an on-premises instance. The
-- deployment group includes only on-premises instances identified by all
-- the tag groups. Cannot be used in the same call as
-- onPremisesInstanceTagFilters.
--
-- 'outdatedInstancesStrategy', 'deploymentGroupInfo_outdatedInstancesStrategy' - Indicates what happens when new Amazon EC2 instances are launched
-- mid-deployment and do not receive the deployed application revision.
--
-- If this option is set to @UPDATE@ or is unspecified, CodeDeploy
-- initiates one or more \'auto-update outdated instances\' deployments to
-- apply the deployed application revision to the new Amazon EC2 instances.
--
-- If this option is set to @IGNORE@, CodeDeploy does not initiate a
-- deployment to update the new Amazon EC2 instances. This may result in
-- instances having different revisions.
--
-- 'serviceRoleArn', 'deploymentGroupInfo_serviceRoleArn' - A service role Amazon Resource Name (ARN) that grants CodeDeploy
-- permission to make calls to Amazon Web Services services on your behalf.
-- For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for CodeDeploy>
-- in the /CodeDeploy User Guide/.
--
-- 'targetRevision', 'deploymentGroupInfo_targetRevision' - Information about the deployment group\'s target revision, including
-- type and location.
--
-- 'triggerConfigurations', 'deploymentGroupInfo_triggerConfigurations' - Information about triggers associated with the deployment group.
newDeploymentGroupInfo ::
  DeploymentGroupInfo
newDeploymentGroupInfo =
  DeploymentGroupInfo'
    { alarmConfiguration =
        Prelude.Nothing,
      applicationName = Prelude.Nothing,
      autoRollbackConfiguration = Prelude.Nothing,
      autoScalingGroups = Prelude.Nothing,
      blueGreenDeploymentConfiguration = Prelude.Nothing,
      computePlatform = Prelude.Nothing,
      deploymentConfigName = Prelude.Nothing,
      deploymentGroupId = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing,
      deploymentStyle = Prelude.Nothing,
      ec2TagFilters = Prelude.Nothing,
      ec2TagSet = Prelude.Nothing,
      ecsServices = Prelude.Nothing,
      lastAttemptedDeployment = Prelude.Nothing,
      lastSuccessfulDeployment = Prelude.Nothing,
      loadBalancerInfo = Prelude.Nothing,
      onPremisesInstanceTagFilters = Prelude.Nothing,
      onPremisesTagSet = Prelude.Nothing,
      outdatedInstancesStrategy = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      targetRevision = Prelude.Nothing,
      triggerConfigurations = Prelude.Nothing
    }

-- | A list of alarms associated with the deployment group.
deploymentGroupInfo_alarmConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe AlarmConfiguration)
deploymentGroupInfo_alarmConfiguration = Lens.lens (\DeploymentGroupInfo' {alarmConfiguration} -> alarmConfiguration) (\s@DeploymentGroupInfo' {} a -> s {alarmConfiguration = a} :: DeploymentGroupInfo)

-- | The application name.
deploymentGroupInfo_applicationName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_applicationName = Lens.lens (\DeploymentGroupInfo' {applicationName} -> applicationName) (\s@DeploymentGroupInfo' {} a -> s {applicationName = a} :: DeploymentGroupInfo)

-- | Information about the automatic rollback configuration associated with
-- the deployment group.
deploymentGroupInfo_autoRollbackConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe AutoRollbackConfiguration)
deploymentGroupInfo_autoRollbackConfiguration = Lens.lens (\DeploymentGroupInfo' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentGroupInfo' {} a -> s {autoRollbackConfiguration = a} :: DeploymentGroupInfo)

-- | A list of associated Auto Scaling groups.
deploymentGroupInfo_autoScalingGroups :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [AutoScalingGroup])
deploymentGroupInfo_autoScalingGroups = Lens.lens (\DeploymentGroupInfo' {autoScalingGroups} -> autoScalingGroups) (\s@DeploymentGroupInfo' {} a -> s {autoScalingGroups = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about blue\/green deployment options for a deployment group.
deploymentGroupInfo_blueGreenDeploymentConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe BlueGreenDeploymentConfiguration)
deploymentGroupInfo_blueGreenDeploymentConfiguration = Lens.lens (\DeploymentGroupInfo' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@DeploymentGroupInfo' {} a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentGroupInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentGroupInfo_computePlatform :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe ComputePlatform)
deploymentGroupInfo_computePlatform = Lens.lens (\DeploymentGroupInfo' {computePlatform} -> computePlatform) (\s@DeploymentGroupInfo' {} a -> s {computePlatform = a} :: DeploymentGroupInfo)

-- | The deployment configuration name.
deploymentGroupInfo_deploymentConfigName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentConfigName = Lens.lens (\DeploymentGroupInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentGroupInfo' {} a -> s {deploymentConfigName = a} :: DeploymentGroupInfo)

-- | The deployment group ID.
deploymentGroupInfo_deploymentGroupId :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentGroupId = Lens.lens (\DeploymentGroupInfo' {deploymentGroupId} -> deploymentGroupId) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupId = a} :: DeploymentGroupInfo)

-- | The deployment group name.
deploymentGroupInfo_deploymentGroupName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentGroupName = Lens.lens (\DeploymentGroupInfo' {deploymentGroupName} -> deploymentGroupName) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupName = a} :: DeploymentGroupInfo)

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
deploymentGroupInfo_deploymentStyle :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe DeploymentStyle)
deploymentGroupInfo_deploymentStyle = Lens.lens (\DeploymentGroupInfo' {deploymentStyle} -> deploymentStyle) (\s@DeploymentGroupInfo' {} a -> s {deploymentStyle = a} :: DeploymentGroupInfo)

-- | The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags.
deploymentGroupInfo_ec2TagFilters :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [EC2TagFilter])
deploymentGroupInfo_ec2TagFilters = Lens.lens (\DeploymentGroupInfo' {ec2TagFilters} -> ec2TagFilters) (\s@DeploymentGroupInfo' {} a -> s {ec2TagFilters = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about groups of tags applied to an Amazon EC2 instance. The
-- deployment group includes only Amazon EC2 instances identified by all of
-- the tag groups. Cannot be used in the same call as ec2TagFilters.
deploymentGroupInfo_ec2TagSet :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe EC2TagSet)
deploymentGroupInfo_ec2TagSet = Lens.lens (\DeploymentGroupInfo' {ec2TagSet} -> ec2TagSet) (\s@DeploymentGroupInfo' {} a -> s {ec2TagSet = a} :: DeploymentGroupInfo)

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
deploymentGroupInfo_ecsServices :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [ECSService])
deploymentGroupInfo_ecsServices = Lens.lens (\DeploymentGroupInfo' {ecsServices} -> ecsServices) (\s@DeploymentGroupInfo' {} a -> s {ecsServices = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about the most recent attempted deployment to the deployment
-- group.
deploymentGroupInfo_lastAttemptedDeployment :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastAttemptedDeployment = Lens.lens (\DeploymentGroupInfo' {lastAttemptedDeployment} -> lastAttemptedDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastAttemptedDeployment = a} :: DeploymentGroupInfo)

-- | Information about the most recent successful deployment to the
-- deployment group.
deploymentGroupInfo_lastSuccessfulDeployment :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastSuccessfulDeployment = Lens.lens (\DeploymentGroupInfo' {lastSuccessfulDeployment} -> lastSuccessfulDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastSuccessfulDeployment = a} :: DeploymentGroupInfo)

-- | Information about the load balancer to use in a deployment.
deploymentGroupInfo_loadBalancerInfo :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LoadBalancerInfo)
deploymentGroupInfo_loadBalancerInfo = Lens.lens (\DeploymentGroupInfo' {loadBalancerInfo} -> loadBalancerInfo) (\s@DeploymentGroupInfo' {} a -> s {loadBalancerInfo = a} :: DeploymentGroupInfo)

-- | The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags.
deploymentGroupInfo_onPremisesInstanceTagFilters :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [TagFilter])
deploymentGroupInfo_onPremisesInstanceTagFilters = Lens.lens (\DeploymentGroupInfo' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@DeploymentGroupInfo' {} a -> s {onPremisesInstanceTagFilters = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

-- | Information about groups of tags applied to an on-premises instance. The
-- deployment group includes only on-premises instances identified by all
-- the tag groups. Cannot be used in the same call as
-- onPremisesInstanceTagFilters.
deploymentGroupInfo_onPremisesTagSet :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe OnPremisesTagSet)
deploymentGroupInfo_onPremisesTagSet = Lens.lens (\DeploymentGroupInfo' {onPremisesTagSet} -> onPremisesTagSet) (\s@DeploymentGroupInfo' {} a -> s {onPremisesTagSet = a} :: DeploymentGroupInfo)

-- | Indicates what happens when new Amazon EC2 instances are launched
-- mid-deployment and do not receive the deployed application revision.
--
-- If this option is set to @UPDATE@ or is unspecified, CodeDeploy
-- initiates one or more \'auto-update outdated instances\' deployments to
-- apply the deployed application revision to the new Amazon EC2 instances.
--
-- If this option is set to @IGNORE@, CodeDeploy does not initiate a
-- deployment to update the new Amazon EC2 instances. This may result in
-- instances having different revisions.
deploymentGroupInfo_outdatedInstancesStrategy :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe OutdatedInstancesStrategy)
deploymentGroupInfo_outdatedInstancesStrategy = Lens.lens (\DeploymentGroupInfo' {outdatedInstancesStrategy} -> outdatedInstancesStrategy) (\s@DeploymentGroupInfo' {} a -> s {outdatedInstancesStrategy = a} :: DeploymentGroupInfo)

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
-- permission to make calls to Amazon Web Services services on your behalf.
-- For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for CodeDeploy>
-- in the /CodeDeploy User Guide/.
deploymentGroupInfo_serviceRoleArn :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_serviceRoleArn = Lens.lens (\DeploymentGroupInfo' {serviceRoleArn} -> serviceRoleArn) (\s@DeploymentGroupInfo' {} a -> s {serviceRoleArn = a} :: DeploymentGroupInfo)

-- | Information about the deployment group\'s target revision, including
-- type and location.
deploymentGroupInfo_targetRevision :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe RevisionLocation)
deploymentGroupInfo_targetRevision = Lens.lens (\DeploymentGroupInfo' {targetRevision} -> targetRevision) (\s@DeploymentGroupInfo' {} a -> s {targetRevision = a} :: DeploymentGroupInfo)

-- | Information about triggers associated with the deployment group.
deploymentGroupInfo_triggerConfigurations :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [TriggerConfig])
deploymentGroupInfo_triggerConfigurations = Lens.lens (\DeploymentGroupInfo' {triggerConfigurations} -> triggerConfigurations) (\s@DeploymentGroupInfo' {} a -> s {triggerConfigurations = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DeploymentGroupInfo where
  parseJSON =
    Data.withObject
      "DeploymentGroupInfo"
      ( \x ->
          DeploymentGroupInfo'
            Prelude.<$> (x Data..:? "alarmConfiguration")
            Prelude.<*> (x Data..:? "applicationName")
            Prelude.<*> (x Data..:? "autoRollbackConfiguration")
            Prelude.<*> ( x
                            Data..:? "autoScalingGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "blueGreenDeploymentConfiguration")
            Prelude.<*> (x Data..:? "computePlatform")
            Prelude.<*> (x Data..:? "deploymentConfigName")
            Prelude.<*> (x Data..:? "deploymentGroupId")
            Prelude.<*> (x Data..:? "deploymentGroupName")
            Prelude.<*> (x Data..:? "deploymentStyle")
            Prelude.<*> (x Data..:? "ec2TagFilters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ec2TagSet")
            Prelude.<*> (x Data..:? "ecsServices" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "lastAttemptedDeployment")
            Prelude.<*> (x Data..:? "lastSuccessfulDeployment")
            Prelude.<*> (x Data..:? "loadBalancerInfo")
            Prelude.<*> ( x
                            Data..:? "onPremisesInstanceTagFilters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "onPremisesTagSet")
            Prelude.<*> (x Data..:? "outdatedInstancesStrategy")
            Prelude.<*> (x Data..:? "serviceRoleArn")
            Prelude.<*> (x Data..:? "targetRevision")
            Prelude.<*> ( x
                            Data..:? "triggerConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DeploymentGroupInfo where
  hashWithSalt _salt DeploymentGroupInfo' {..} =
    _salt
      `Prelude.hashWithSalt` alarmConfiguration
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` autoRollbackConfiguration
      `Prelude.hashWithSalt` autoScalingGroups
      `Prelude.hashWithSalt` blueGreenDeploymentConfiguration
      `Prelude.hashWithSalt` computePlatform
      `Prelude.hashWithSalt` deploymentConfigName
      `Prelude.hashWithSalt` deploymentGroupId
      `Prelude.hashWithSalt` deploymentGroupName
      `Prelude.hashWithSalt` deploymentStyle
      `Prelude.hashWithSalt` ec2TagFilters
      `Prelude.hashWithSalt` ec2TagSet
      `Prelude.hashWithSalt` ecsServices
      `Prelude.hashWithSalt` lastAttemptedDeployment
      `Prelude.hashWithSalt` lastSuccessfulDeployment
      `Prelude.hashWithSalt` loadBalancerInfo
      `Prelude.hashWithSalt` onPremisesInstanceTagFilters
      `Prelude.hashWithSalt` onPremisesTagSet
      `Prelude.hashWithSalt` outdatedInstancesStrategy
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` targetRevision
      `Prelude.hashWithSalt` triggerConfigurations

instance Prelude.NFData DeploymentGroupInfo where
  rnf DeploymentGroupInfo' {..} =
    Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf autoRollbackConfiguration
      `Prelude.seq` Prelude.rnf autoScalingGroups
      `Prelude.seq` Prelude.rnf blueGreenDeploymentConfiguration
      `Prelude.seq` Prelude.rnf computePlatform
      `Prelude.seq` Prelude.rnf deploymentConfigName
      `Prelude.seq` Prelude.rnf deploymentGroupId
      `Prelude.seq` Prelude.rnf deploymentGroupName
      `Prelude.seq` Prelude.rnf deploymentStyle
      `Prelude.seq` Prelude.rnf ec2TagFilters
      `Prelude.seq` Prelude.rnf ec2TagSet
      `Prelude.seq` Prelude.rnf ecsServices
      `Prelude.seq` Prelude.rnf lastAttemptedDeployment
      `Prelude.seq` Prelude.rnf lastSuccessfulDeployment
      `Prelude.seq` Prelude.rnf loadBalancerInfo
      `Prelude.seq` Prelude.rnf
        onPremisesInstanceTagFilters
      `Prelude.seq` Prelude.rnf onPremisesTagSet
      `Prelude.seq` Prelude.rnf
        outdatedInstancesStrategy
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf targetRevision
      `Prelude.seq` Prelude.rnf
        triggerConfigurations
