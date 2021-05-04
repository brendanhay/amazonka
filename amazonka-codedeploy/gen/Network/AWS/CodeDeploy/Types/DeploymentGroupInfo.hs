{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a deployment group.
--
-- /See:/ 'newDeploymentGroupInfo' smart constructor.
data DeploymentGroupInfo = DeploymentGroupInfo'
  { -- | Information about groups of tags applied to an on-premises instance. The
    -- deployment group includes only on-premises instances identified by all
    -- the tag groups. Cannot be used in the same call as
    -- onPremisesInstanceTagFilters.
    onPremisesTagSet :: Prelude.Maybe OnPremisesTagSet,
    -- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
    -- permission to make calls to AWS services on your behalf. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
    -- in the /AWS CodeDeploy User Guide/.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The deployment configuration name.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | Information about the automatic rollback configuration associated with
    -- the deployment group.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | The deployment group name.
    deploymentGroupName :: Prelude.Maybe Prelude.Text,
    -- | Information about triggers associated with the deployment group.
    triggerConfigurations :: Prelude.Maybe [TriggerConfig],
    -- | The deployment group ID.
    deploymentGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes
    -- EC2 instances with any of the specified tags.
    ec2TagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | Information about the deployment group\'s target revision, including
    -- type and location.
    targetRevision :: Prelude.Maybe RevisionLocation,
    -- | Information about the most recent successful deployment to the
    -- deployment group.
    lastSuccessfulDeployment :: Prelude.Maybe LastDeploymentInfo,
    -- | Information about the most recent attempted deployment to the deployment
    -- group.
    lastAttemptedDeployment :: Prelude.Maybe LastDeploymentInfo,
    -- | The on-premises instance tags on which to filter. The deployment group
    -- includes on-premises instances with any of the specified tags.
    onPremisesInstanceTagFilters :: Prelude.Maybe [TagFilter],
    -- | Information about the load balancer to use in a deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | Information about groups of tags applied to an EC2 instance. The
    -- deployment group includes only EC2 instances identified by all of the
    -- tag groups. Cannot be used in the same call as ec2TagFilters.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | A list of associated Auto Scaling groups.
    autoScalingGroups :: Prelude.Maybe [AutoScalingGroup],
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | A list of alarms associated with the deployment group.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Prelude.Maybe [ECSService],
    -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      deploymentConfigName = Prelude.Nothing,
      autoRollbackConfiguration = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing,
      triggerConfigurations = Prelude.Nothing,
      deploymentGroupId = Prelude.Nothing,
      ec2TagFilters = Prelude.Nothing,
      targetRevision = Prelude.Nothing,
      lastSuccessfulDeployment = Prelude.Nothing,
      lastAttemptedDeployment = Prelude.Nothing,
      onPremisesInstanceTagFilters = Prelude.Nothing,
      loadBalancerInfo = Prelude.Nothing,
      ec2TagSet = Prelude.Nothing,
      blueGreenDeploymentConfiguration = Prelude.Nothing,
      autoScalingGroups = Prelude.Nothing,
      deploymentStyle = Prelude.Nothing,
      alarmConfiguration = Prelude.Nothing,
      ecsServices = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      computePlatform = Prelude.Nothing
    }

-- | Information about groups of tags applied to an on-premises instance. The
-- deployment group includes only on-premises instances identified by all
-- the tag groups. Cannot be used in the same call as
-- onPremisesInstanceTagFilters.
deploymentGroupInfo_onPremisesTagSet :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe OnPremisesTagSet)
deploymentGroupInfo_onPremisesTagSet = Lens.lens (\DeploymentGroupInfo' {onPremisesTagSet} -> onPremisesTagSet) (\s@DeploymentGroupInfo' {} a -> s {onPremisesTagSet = a} :: DeploymentGroupInfo)

-- | A service role Amazon Resource Name (ARN) that grants CodeDeploy
-- permission to make calls to AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/getting-started-create-service-role.html Create a Service Role for AWS CodeDeploy>
-- in the /AWS CodeDeploy User Guide/.
deploymentGroupInfo_serviceRoleArn :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_serviceRoleArn = Lens.lens (\DeploymentGroupInfo' {serviceRoleArn} -> serviceRoleArn) (\s@DeploymentGroupInfo' {} a -> s {serviceRoleArn = a} :: DeploymentGroupInfo)

-- | The deployment configuration name.
deploymentGroupInfo_deploymentConfigName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentConfigName = Lens.lens (\DeploymentGroupInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentGroupInfo' {} a -> s {deploymentConfigName = a} :: DeploymentGroupInfo)

-- | Information about the automatic rollback configuration associated with
-- the deployment group.
deploymentGroupInfo_autoRollbackConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe AutoRollbackConfiguration)
deploymentGroupInfo_autoRollbackConfiguration = Lens.lens (\DeploymentGroupInfo' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentGroupInfo' {} a -> s {autoRollbackConfiguration = a} :: DeploymentGroupInfo)

-- | The deployment group name.
deploymentGroupInfo_deploymentGroupName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentGroupName = Lens.lens (\DeploymentGroupInfo' {deploymentGroupName} -> deploymentGroupName) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupName = a} :: DeploymentGroupInfo)

-- | Information about triggers associated with the deployment group.
deploymentGroupInfo_triggerConfigurations :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [TriggerConfig])
deploymentGroupInfo_triggerConfigurations = Lens.lens (\DeploymentGroupInfo' {triggerConfigurations} -> triggerConfigurations) (\s@DeploymentGroupInfo' {} a -> s {triggerConfigurations = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The deployment group ID.
deploymentGroupInfo_deploymentGroupId :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_deploymentGroupId = Lens.lens (\DeploymentGroupInfo' {deploymentGroupId} -> deploymentGroupId) (\s@DeploymentGroupInfo' {} a -> s {deploymentGroupId = a} :: DeploymentGroupInfo)

-- | The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags.
deploymentGroupInfo_ec2TagFilters :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [EC2TagFilter])
deploymentGroupInfo_ec2TagFilters = Lens.lens (\DeploymentGroupInfo' {ec2TagFilters} -> ec2TagFilters) (\s@DeploymentGroupInfo' {} a -> s {ec2TagFilters = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the deployment group\'s target revision, including
-- type and location.
deploymentGroupInfo_targetRevision :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe RevisionLocation)
deploymentGroupInfo_targetRevision = Lens.lens (\DeploymentGroupInfo' {targetRevision} -> targetRevision) (\s@DeploymentGroupInfo' {} a -> s {targetRevision = a} :: DeploymentGroupInfo)

-- | Information about the most recent successful deployment to the
-- deployment group.
deploymentGroupInfo_lastSuccessfulDeployment :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastSuccessfulDeployment = Lens.lens (\DeploymentGroupInfo' {lastSuccessfulDeployment} -> lastSuccessfulDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastSuccessfulDeployment = a} :: DeploymentGroupInfo)

-- | Information about the most recent attempted deployment to the deployment
-- group.
deploymentGroupInfo_lastAttemptedDeployment :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LastDeploymentInfo)
deploymentGroupInfo_lastAttemptedDeployment = Lens.lens (\DeploymentGroupInfo' {lastAttemptedDeployment} -> lastAttemptedDeployment) (\s@DeploymentGroupInfo' {} a -> s {lastAttemptedDeployment = a} :: DeploymentGroupInfo)

-- | The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags.
deploymentGroupInfo_onPremisesInstanceTagFilters :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [TagFilter])
deploymentGroupInfo_onPremisesInstanceTagFilters = Lens.lens (\DeploymentGroupInfo' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@DeploymentGroupInfo' {} a -> s {onPremisesInstanceTagFilters = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the load balancer to use in a deployment.
deploymentGroupInfo_loadBalancerInfo :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe LoadBalancerInfo)
deploymentGroupInfo_loadBalancerInfo = Lens.lens (\DeploymentGroupInfo' {loadBalancerInfo} -> loadBalancerInfo) (\s@DeploymentGroupInfo' {} a -> s {loadBalancerInfo = a} :: DeploymentGroupInfo)

-- | Information about groups of tags applied to an EC2 instance. The
-- deployment group includes only EC2 instances identified by all of the
-- tag groups. Cannot be used in the same call as ec2TagFilters.
deploymentGroupInfo_ec2TagSet :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe EC2TagSet)
deploymentGroupInfo_ec2TagSet = Lens.lens (\DeploymentGroupInfo' {ec2TagSet} -> ec2TagSet) (\s@DeploymentGroupInfo' {} a -> s {ec2TagSet = a} :: DeploymentGroupInfo)

-- | Information about blue\/green deployment options for a deployment group.
deploymentGroupInfo_blueGreenDeploymentConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe BlueGreenDeploymentConfiguration)
deploymentGroupInfo_blueGreenDeploymentConfiguration = Lens.lens (\DeploymentGroupInfo' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@DeploymentGroupInfo' {} a -> s {blueGreenDeploymentConfiguration = a} :: DeploymentGroupInfo)

-- | A list of associated Auto Scaling groups.
deploymentGroupInfo_autoScalingGroups :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [AutoScalingGroup])
deploymentGroupInfo_autoScalingGroups = Lens.lens (\DeploymentGroupInfo' {autoScalingGroups} -> autoScalingGroups) (\s@DeploymentGroupInfo' {} a -> s {autoScalingGroups = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
deploymentGroupInfo_deploymentStyle :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe DeploymentStyle)
deploymentGroupInfo_deploymentStyle = Lens.lens (\DeploymentGroupInfo' {deploymentStyle} -> deploymentStyle) (\s@DeploymentGroupInfo' {} a -> s {deploymentStyle = a} :: DeploymentGroupInfo)

-- | A list of alarms associated with the deployment group.
deploymentGroupInfo_alarmConfiguration :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe AlarmConfiguration)
deploymentGroupInfo_alarmConfiguration = Lens.lens (\DeploymentGroupInfo' {alarmConfiguration} -> alarmConfiguration) (\s@DeploymentGroupInfo' {} a -> s {alarmConfiguration = a} :: DeploymentGroupInfo)

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
deploymentGroupInfo_ecsServices :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe [ECSService])
deploymentGroupInfo_ecsServices = Lens.lens (\DeploymentGroupInfo' {ecsServices} -> ecsServices) (\s@DeploymentGroupInfo' {} a -> s {ecsServices = a} :: DeploymentGroupInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The application name.
deploymentGroupInfo_applicationName :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe Prelude.Text)
deploymentGroupInfo_applicationName = Lens.lens (\DeploymentGroupInfo' {applicationName} -> applicationName) (\s@DeploymentGroupInfo' {} a -> s {applicationName = a} :: DeploymentGroupInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentGroupInfo_computePlatform :: Lens.Lens' DeploymentGroupInfo (Prelude.Maybe ComputePlatform)
deploymentGroupInfo_computePlatform = Lens.lens (\DeploymentGroupInfo' {computePlatform} -> computePlatform) (\s@DeploymentGroupInfo' {} a -> s {computePlatform = a} :: DeploymentGroupInfo)

instance Prelude.FromJSON DeploymentGroupInfo where
  parseJSON =
    Prelude.withObject
      "DeploymentGroupInfo"
      ( \x ->
          DeploymentGroupInfo'
            Prelude.<$> (x Prelude..:? "onPremisesTagSet")
            Prelude.<*> (x Prelude..:? "serviceRoleArn")
            Prelude.<*> (x Prelude..:? "deploymentConfigName")
            Prelude.<*> (x Prelude..:? "autoRollbackConfiguration")
            Prelude.<*> (x Prelude..:? "deploymentGroupName")
            Prelude.<*> ( x Prelude..:? "triggerConfigurations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "deploymentGroupId")
            Prelude.<*> ( x Prelude..:? "ec2TagFilters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "targetRevision")
            Prelude.<*> (x Prelude..:? "lastSuccessfulDeployment")
            Prelude.<*> (x Prelude..:? "lastAttemptedDeployment")
            Prelude.<*> ( x Prelude..:? "onPremisesInstanceTagFilters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "loadBalancerInfo")
            Prelude.<*> (x Prelude..:? "ec2TagSet")
            Prelude.<*> (x Prelude..:? "blueGreenDeploymentConfiguration")
            Prelude.<*> ( x Prelude..:? "autoScalingGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "deploymentStyle")
            Prelude.<*> (x Prelude..:? "alarmConfiguration")
            Prelude.<*> ( x Prelude..:? "ecsServices"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "applicationName")
            Prelude.<*> (x Prelude..:? "computePlatform")
      )

instance Prelude.Hashable DeploymentGroupInfo

instance Prelude.NFData DeploymentGroupInfo
