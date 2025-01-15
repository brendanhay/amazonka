{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeDeploy.UpdateDeploymentGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a deployment group.
module Amazonka.CodeDeploy.UpdateDeploymentGroup
  ( -- * Creating a Request
    UpdateDeploymentGroup (..),
    newUpdateDeploymentGroup,

    -- * Request Lenses
    updateDeploymentGroup_alarmConfiguration,
    updateDeploymentGroup_autoRollbackConfiguration,
    updateDeploymentGroup_autoScalingGroups,
    updateDeploymentGroup_blueGreenDeploymentConfiguration,
    updateDeploymentGroup_deploymentConfigName,
    updateDeploymentGroup_deploymentStyle,
    updateDeploymentGroup_ec2TagFilters,
    updateDeploymentGroup_ec2TagSet,
    updateDeploymentGroup_ecsServices,
    updateDeploymentGroup_loadBalancerInfo,
    updateDeploymentGroup_newDeploymentGroupName,
    updateDeploymentGroup_onPremisesInstanceTagFilters,
    updateDeploymentGroup_onPremisesTagSet,
    updateDeploymentGroup_outdatedInstancesStrategy,
    updateDeploymentGroup_serviceRoleArn,
    updateDeploymentGroup_triggerConfigurations,
    updateDeploymentGroup_applicationName,
    updateDeploymentGroup_currentDeploymentGroupName,

    -- * Destructuring the Response
    UpdateDeploymentGroupResponse (..),
    newUpdateDeploymentGroupResponse,

    -- * Response Lenses
    updateDeploymentGroupResponse_hooksNotCleanedUp,
    updateDeploymentGroupResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of an @UpdateDeploymentGroup@ operation.
--
-- /See:/ 'newUpdateDeploymentGroup' smart constructor.
data UpdateDeploymentGroup = UpdateDeploymentGroup'
  { -- | Information to add or change about Amazon CloudWatch alarms when the
    -- deployment group is updated.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | Information for an automatic rollback configuration that is added or
    -- changed when a deployment group is updated.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | The replacement list of Auto Scaling groups to be included in the
    -- deployment group, if you want to change them.
    --
    -- -   To keep the Auto Scaling groups, enter their names or do not specify
    --     this parameter.
    --
    -- -   To remove Auto Scaling groups, specify a non-null empty list of Auto
    --     Scaling group names to detach all CodeDeploy-managed Auto Scaling
    --     lifecycle hooks. For examples, see
    --     <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/codedeploy/latest/userguide/troubleshooting-auto-scaling.html#troubleshooting-auto-scaling-heartbeat Amazon EC2 instances in an Amazon EC2 Auto Scaling group fail to launch and receive the error \"Heartbeat Timeout\">
    --     in the /CodeDeploy User Guide/.
    autoScalingGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | The replacement deployment configuration name to use, if you want to
    -- change it.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | The replacement set of Amazon EC2 tags on which to filter, if you want
    -- to change them. To keep the existing tags, enter their names. To remove
    -- tags, do not enter any tag names.
    ec2TagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | Information about groups of tags applied to on-premises instances. The
    -- deployment group includes only Amazon EC2 instances identified by all
    -- the tag groups.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Prelude.Maybe [ECSService],
    -- | Information about the load balancer used in a deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | The new name of the deployment group, if you want to change it.
    newDeploymentGroupName' :: Prelude.Maybe Prelude.Text,
    -- | The replacement set of on-premises instance tags on which to filter, if
    -- you want to change them. To keep the existing tags, enter their names.
    -- To remove tags, do not enter any tag names.
    onPremisesInstanceTagFilters :: Prelude.Maybe [TagFilter],
    -- | Information about an on-premises instance tag set. The deployment group
    -- includes only on-premises instances identified by all the tag groups.
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
    -- | A replacement ARN for the service role, if you want to change it.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Information about triggers to change when the deployment group is
    -- updated. For examples, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group>
    -- in the /CodeDeploy User Guide/.
    triggerConfigurations :: Prelude.Maybe [TriggerConfig],
    -- | The application name that corresponds to the deployment group to update.
    applicationName :: Prelude.Text,
    -- | The current name of the deployment group.
    currentDeploymentGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeploymentGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmConfiguration', 'updateDeploymentGroup_alarmConfiguration' - Information to add or change about Amazon CloudWatch alarms when the
-- deployment group is updated.
--
-- 'autoRollbackConfiguration', 'updateDeploymentGroup_autoRollbackConfiguration' - Information for an automatic rollback configuration that is added or
-- changed when a deployment group is updated.
--
-- 'autoScalingGroups', 'updateDeploymentGroup_autoScalingGroups' - The replacement list of Auto Scaling groups to be included in the
-- deployment group, if you want to change them.
--
-- -   To keep the Auto Scaling groups, enter their names or do not specify
--     this parameter.
--
-- -   To remove Auto Scaling groups, specify a non-null empty list of Auto
--     Scaling group names to detach all CodeDeploy-managed Auto Scaling
--     lifecycle hooks. For examples, see
--     <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/codedeploy/latest/userguide/troubleshooting-auto-scaling.html#troubleshooting-auto-scaling-heartbeat Amazon EC2 instances in an Amazon EC2 Auto Scaling group fail to launch and receive the error \"Heartbeat Timeout\">
--     in the /CodeDeploy User Guide/.
--
-- 'blueGreenDeploymentConfiguration', 'updateDeploymentGroup_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for a deployment group.
--
-- 'deploymentConfigName', 'updateDeploymentGroup_deploymentConfigName' - The replacement deployment configuration name to use, if you want to
-- change it.
--
-- 'deploymentStyle', 'updateDeploymentGroup_deploymentStyle' - Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- 'ec2TagFilters', 'updateDeploymentGroup_ec2TagFilters' - The replacement set of Amazon EC2 tags on which to filter, if you want
-- to change them. To keep the existing tags, enter their names. To remove
-- tags, do not enter any tag names.
--
-- 'ec2TagSet', 'updateDeploymentGroup_ec2TagSet' - Information about groups of tags applied to on-premises instances. The
-- deployment group includes only Amazon EC2 instances identified by all
-- the tag groups.
--
-- 'ecsServices', 'updateDeploymentGroup_ecsServices' - The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
--
-- 'loadBalancerInfo', 'updateDeploymentGroup_loadBalancerInfo' - Information about the load balancer used in a deployment.
--
-- 'newDeploymentGroupName'', 'updateDeploymentGroup_newDeploymentGroupName' - The new name of the deployment group, if you want to change it.
--
-- 'onPremisesInstanceTagFilters', 'updateDeploymentGroup_onPremisesInstanceTagFilters' - The replacement set of on-premises instance tags on which to filter, if
-- you want to change them. To keep the existing tags, enter their names.
-- To remove tags, do not enter any tag names.
--
-- 'onPremisesTagSet', 'updateDeploymentGroup_onPremisesTagSet' - Information about an on-premises instance tag set. The deployment group
-- includes only on-premises instances identified by all the tag groups.
--
-- 'outdatedInstancesStrategy', 'updateDeploymentGroup_outdatedInstancesStrategy' - Indicates what happens when new Amazon EC2 instances are launched
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
-- 'serviceRoleArn', 'updateDeploymentGroup_serviceRoleArn' - A replacement ARN for the service role, if you want to change it.
--
-- 'triggerConfigurations', 'updateDeploymentGroup_triggerConfigurations' - Information about triggers to change when the deployment group is
-- updated. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group>
-- in the /CodeDeploy User Guide/.
--
-- 'applicationName', 'updateDeploymentGroup_applicationName' - The application name that corresponds to the deployment group to update.
--
-- 'currentDeploymentGroupName', 'updateDeploymentGroup_currentDeploymentGroupName' - The current name of the deployment group.
newUpdateDeploymentGroup ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentDeploymentGroupName'
  Prelude.Text ->
  UpdateDeploymentGroup
newUpdateDeploymentGroup
  pApplicationName_
  pCurrentDeploymentGroupName_ =
    UpdateDeploymentGroup'
      { alarmConfiguration =
          Prelude.Nothing,
        autoRollbackConfiguration = Prelude.Nothing,
        autoScalingGroups = Prelude.Nothing,
        blueGreenDeploymentConfiguration = Prelude.Nothing,
        deploymentConfigName = Prelude.Nothing,
        deploymentStyle = Prelude.Nothing,
        ec2TagFilters = Prelude.Nothing,
        ec2TagSet = Prelude.Nothing,
        ecsServices = Prelude.Nothing,
        loadBalancerInfo = Prelude.Nothing,
        newDeploymentGroupName' = Prelude.Nothing,
        onPremisesInstanceTagFilters = Prelude.Nothing,
        onPremisesTagSet = Prelude.Nothing,
        outdatedInstancesStrategy = Prelude.Nothing,
        serviceRoleArn = Prelude.Nothing,
        triggerConfigurations = Prelude.Nothing,
        applicationName = pApplicationName_,
        currentDeploymentGroupName =
          pCurrentDeploymentGroupName_
      }

-- | Information to add or change about Amazon CloudWatch alarms when the
-- deployment group is updated.
updateDeploymentGroup_alarmConfiguration :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe AlarmConfiguration)
updateDeploymentGroup_alarmConfiguration = Lens.lens (\UpdateDeploymentGroup' {alarmConfiguration} -> alarmConfiguration) (\s@UpdateDeploymentGroup' {} a -> s {alarmConfiguration = a} :: UpdateDeploymentGroup)

-- | Information for an automatic rollback configuration that is added or
-- changed when a deployment group is updated.
updateDeploymentGroup_autoRollbackConfiguration :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe AutoRollbackConfiguration)
updateDeploymentGroup_autoRollbackConfiguration = Lens.lens (\UpdateDeploymentGroup' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@UpdateDeploymentGroup' {} a -> s {autoRollbackConfiguration = a} :: UpdateDeploymentGroup)

-- | The replacement list of Auto Scaling groups to be included in the
-- deployment group, if you want to change them.
--
-- -   To keep the Auto Scaling groups, enter their names or do not specify
--     this parameter.
--
-- -   To remove Auto Scaling groups, specify a non-null empty list of Auto
--     Scaling group names to detach all CodeDeploy-managed Auto Scaling
--     lifecycle hooks. For examples, see
--     <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/codedeploy/latest/userguide/troubleshooting-auto-scaling.html#troubleshooting-auto-scaling-heartbeat Amazon EC2 instances in an Amazon EC2 Auto Scaling group fail to launch and receive the error \"Heartbeat Timeout\">
--     in the /CodeDeploy User Guide/.
updateDeploymentGroup_autoScalingGroups :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [Prelude.Text])
updateDeploymentGroup_autoScalingGroups = Lens.lens (\UpdateDeploymentGroup' {autoScalingGroups} -> autoScalingGroups) (\s@UpdateDeploymentGroup' {} a -> s {autoScalingGroups = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Information about blue\/green deployment options for a deployment group.
updateDeploymentGroup_blueGreenDeploymentConfiguration :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe BlueGreenDeploymentConfiguration)
updateDeploymentGroup_blueGreenDeploymentConfiguration = Lens.lens (\UpdateDeploymentGroup' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@UpdateDeploymentGroup' {} a -> s {blueGreenDeploymentConfiguration = a} :: UpdateDeploymentGroup)

-- | The replacement deployment configuration name to use, if you want to
-- change it.
updateDeploymentGroup_deploymentConfigName :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe Prelude.Text)
updateDeploymentGroup_deploymentConfigName = Lens.lens (\UpdateDeploymentGroup' {deploymentConfigName} -> deploymentConfigName) (\s@UpdateDeploymentGroup' {} a -> s {deploymentConfigName = a} :: UpdateDeploymentGroup)

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
updateDeploymentGroup_deploymentStyle :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe DeploymentStyle)
updateDeploymentGroup_deploymentStyle = Lens.lens (\UpdateDeploymentGroup' {deploymentStyle} -> deploymentStyle) (\s@UpdateDeploymentGroup' {} a -> s {deploymentStyle = a} :: UpdateDeploymentGroup)

-- | The replacement set of Amazon EC2 tags on which to filter, if you want
-- to change them. To keep the existing tags, enter their names. To remove
-- tags, do not enter any tag names.
updateDeploymentGroup_ec2TagFilters :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [EC2TagFilter])
updateDeploymentGroup_ec2TagFilters = Lens.lens (\UpdateDeploymentGroup' {ec2TagFilters} -> ec2TagFilters) (\s@UpdateDeploymentGroup' {} a -> s {ec2TagFilters = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Information about groups of tags applied to on-premises instances. The
-- deployment group includes only Amazon EC2 instances identified by all
-- the tag groups.
updateDeploymentGroup_ec2TagSet :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe EC2TagSet)
updateDeploymentGroup_ec2TagSet = Lens.lens (\UpdateDeploymentGroup' {ec2TagSet} -> ec2TagSet) (\s@UpdateDeploymentGroup' {} a -> s {ec2TagSet = a} :: UpdateDeploymentGroup)

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
updateDeploymentGroup_ecsServices :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [ECSService])
updateDeploymentGroup_ecsServices = Lens.lens (\UpdateDeploymentGroup' {ecsServices} -> ecsServices) (\s@UpdateDeploymentGroup' {} a -> s {ecsServices = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Information about the load balancer used in a deployment.
updateDeploymentGroup_loadBalancerInfo :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe LoadBalancerInfo)
updateDeploymentGroup_loadBalancerInfo = Lens.lens (\UpdateDeploymentGroup' {loadBalancerInfo} -> loadBalancerInfo) (\s@UpdateDeploymentGroup' {} a -> s {loadBalancerInfo = a} :: UpdateDeploymentGroup)

-- | The new name of the deployment group, if you want to change it.
updateDeploymentGroup_newDeploymentGroupName :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe Prelude.Text)
updateDeploymentGroup_newDeploymentGroupName = Lens.lens (\UpdateDeploymentGroup' {newDeploymentGroupName'} -> newDeploymentGroupName') (\s@UpdateDeploymentGroup' {} a -> s {newDeploymentGroupName' = a} :: UpdateDeploymentGroup)

-- | The replacement set of on-premises instance tags on which to filter, if
-- you want to change them. To keep the existing tags, enter their names.
-- To remove tags, do not enter any tag names.
updateDeploymentGroup_onPremisesInstanceTagFilters :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [TagFilter])
updateDeploymentGroup_onPremisesInstanceTagFilters = Lens.lens (\UpdateDeploymentGroup' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@UpdateDeploymentGroup' {} a -> s {onPremisesInstanceTagFilters = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Information about an on-premises instance tag set. The deployment group
-- includes only on-premises instances identified by all the tag groups.
updateDeploymentGroup_onPremisesTagSet :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe OnPremisesTagSet)
updateDeploymentGroup_onPremisesTagSet = Lens.lens (\UpdateDeploymentGroup' {onPremisesTagSet} -> onPremisesTagSet) (\s@UpdateDeploymentGroup' {} a -> s {onPremisesTagSet = a} :: UpdateDeploymentGroup)

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
updateDeploymentGroup_outdatedInstancesStrategy :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe OutdatedInstancesStrategy)
updateDeploymentGroup_outdatedInstancesStrategy = Lens.lens (\UpdateDeploymentGroup' {outdatedInstancesStrategy} -> outdatedInstancesStrategy) (\s@UpdateDeploymentGroup' {} a -> s {outdatedInstancesStrategy = a} :: UpdateDeploymentGroup)

-- | A replacement ARN for the service role, if you want to change it.
updateDeploymentGroup_serviceRoleArn :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe Prelude.Text)
updateDeploymentGroup_serviceRoleArn = Lens.lens (\UpdateDeploymentGroup' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateDeploymentGroup' {} a -> s {serviceRoleArn = a} :: UpdateDeploymentGroup)

-- | Information about triggers to change when the deployment group is
-- updated. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group>
-- in the /CodeDeploy User Guide/.
updateDeploymentGroup_triggerConfigurations :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [TriggerConfig])
updateDeploymentGroup_triggerConfigurations = Lens.lens (\UpdateDeploymentGroup' {triggerConfigurations} -> triggerConfigurations) (\s@UpdateDeploymentGroup' {} a -> s {triggerConfigurations = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | The application name that corresponds to the deployment group to update.
updateDeploymentGroup_applicationName :: Lens.Lens' UpdateDeploymentGroup Prelude.Text
updateDeploymentGroup_applicationName = Lens.lens (\UpdateDeploymentGroup' {applicationName} -> applicationName) (\s@UpdateDeploymentGroup' {} a -> s {applicationName = a} :: UpdateDeploymentGroup)

-- | The current name of the deployment group.
updateDeploymentGroup_currentDeploymentGroupName :: Lens.Lens' UpdateDeploymentGroup Prelude.Text
updateDeploymentGroup_currentDeploymentGroupName = Lens.lens (\UpdateDeploymentGroup' {currentDeploymentGroupName} -> currentDeploymentGroupName) (\s@UpdateDeploymentGroup' {} a -> s {currentDeploymentGroupName = a} :: UpdateDeploymentGroup)

instance Core.AWSRequest UpdateDeploymentGroup where
  type
    AWSResponse UpdateDeploymentGroup =
      UpdateDeploymentGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDeploymentGroupResponse'
            Prelude.<$> ( x
                            Data..?> "hooksNotCleanedUp"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeploymentGroup where
  hashWithSalt _salt UpdateDeploymentGroup' {..} =
    _salt
      `Prelude.hashWithSalt` alarmConfiguration
      `Prelude.hashWithSalt` autoRollbackConfiguration
      `Prelude.hashWithSalt` autoScalingGroups
      `Prelude.hashWithSalt` blueGreenDeploymentConfiguration
      `Prelude.hashWithSalt` deploymentConfigName
      `Prelude.hashWithSalt` deploymentStyle
      `Prelude.hashWithSalt` ec2TagFilters
      `Prelude.hashWithSalt` ec2TagSet
      `Prelude.hashWithSalt` ecsServices
      `Prelude.hashWithSalt` loadBalancerInfo
      `Prelude.hashWithSalt` newDeploymentGroupName'
      `Prelude.hashWithSalt` onPremisesInstanceTagFilters
      `Prelude.hashWithSalt` onPremisesTagSet
      `Prelude.hashWithSalt` outdatedInstancesStrategy
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` triggerConfigurations
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` currentDeploymentGroupName

instance Prelude.NFData UpdateDeploymentGroup where
  rnf UpdateDeploymentGroup' {..} =
    Prelude.rnf alarmConfiguration `Prelude.seq`
      Prelude.rnf autoRollbackConfiguration `Prelude.seq`
        Prelude.rnf autoScalingGroups `Prelude.seq`
          Prelude.rnf blueGreenDeploymentConfiguration `Prelude.seq`
            Prelude.rnf deploymentConfigName `Prelude.seq`
              Prelude.rnf deploymentStyle `Prelude.seq`
                Prelude.rnf ec2TagFilters `Prelude.seq`
                  Prelude.rnf ec2TagSet `Prelude.seq`
                    Prelude.rnf ecsServices `Prelude.seq`
                      Prelude.rnf loadBalancerInfo `Prelude.seq`
                        Prelude.rnf newDeploymentGroupName' `Prelude.seq`
                          Prelude.rnf onPremisesInstanceTagFilters `Prelude.seq`
                            Prelude.rnf onPremisesTagSet `Prelude.seq`
                              Prelude.rnf outdatedInstancesStrategy `Prelude.seq`
                                Prelude.rnf serviceRoleArn `Prelude.seq`
                                  Prelude.rnf triggerConfigurations `Prelude.seq`
                                    Prelude.rnf applicationName `Prelude.seq`
                                      Prelude.rnf
                                        currentDeploymentGroupName

instance Data.ToHeaders UpdateDeploymentGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.UpdateDeploymentGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDeploymentGroup where
  toJSON UpdateDeploymentGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alarmConfiguration" Data..=)
              Prelude.<$> alarmConfiguration,
            ("autoRollbackConfiguration" Data..=)
              Prelude.<$> autoRollbackConfiguration,
            ("autoScalingGroups" Data..=)
              Prelude.<$> autoScalingGroups,
            ("blueGreenDeploymentConfiguration" Data..=)
              Prelude.<$> blueGreenDeploymentConfiguration,
            ("deploymentConfigName" Data..=)
              Prelude.<$> deploymentConfigName,
            ("deploymentStyle" Data..=)
              Prelude.<$> deploymentStyle,
            ("ec2TagFilters" Data..=) Prelude.<$> ec2TagFilters,
            ("ec2TagSet" Data..=) Prelude.<$> ec2TagSet,
            ("ecsServices" Data..=) Prelude.<$> ecsServices,
            ("loadBalancerInfo" Data..=)
              Prelude.<$> loadBalancerInfo,
            ("newDeploymentGroupName" Data..=)
              Prelude.<$> newDeploymentGroupName',
            ("onPremisesInstanceTagFilters" Data..=)
              Prelude.<$> onPremisesInstanceTagFilters,
            ("onPremisesTagSet" Data..=)
              Prelude.<$> onPremisesTagSet,
            ("outdatedInstancesStrategy" Data..=)
              Prelude.<$> outdatedInstancesStrategy,
            ("serviceRoleArn" Data..=)
              Prelude.<$> serviceRoleArn,
            ("triggerConfigurations" Data..=)
              Prelude.<$> triggerConfigurations,
            Prelude.Just
              ("applicationName" Data..= applicationName),
            Prelude.Just
              ( "currentDeploymentGroupName"
                  Data..= currentDeploymentGroupName
              )
          ]
      )

instance Data.ToPath UpdateDeploymentGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDeploymentGroup where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @UpdateDeploymentGroup@ operation.
--
-- /See:/ 'newUpdateDeploymentGroupResponse' smart constructor.
data UpdateDeploymentGroupResponse = UpdateDeploymentGroupResponse'
  { -- | If the output contains no data, and the corresponding deployment group
    -- contained at least one Auto Scaling group, CodeDeploy successfully
    -- removed all corresponding Auto Scaling lifecycle event hooks from the
    -- Amazon Web Services account. If the output contains data, CodeDeploy
    -- could not remove some Auto Scaling lifecycle event hooks from the Amazon
    -- Web Services account.
    hooksNotCleanedUp :: Prelude.Maybe [AutoScalingGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeploymentGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hooksNotCleanedUp', 'updateDeploymentGroupResponse_hooksNotCleanedUp' - If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- Amazon Web Services account. If the output contains data, CodeDeploy
-- could not remove some Auto Scaling lifecycle event hooks from the Amazon
-- Web Services account.
--
-- 'httpStatus', 'updateDeploymentGroupResponse_httpStatus' - The response's http status code.
newUpdateDeploymentGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDeploymentGroupResponse
newUpdateDeploymentGroupResponse pHttpStatus_ =
  UpdateDeploymentGroupResponse'
    { hooksNotCleanedUp =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- Amazon Web Services account. If the output contains data, CodeDeploy
-- could not remove some Auto Scaling lifecycle event hooks from the Amazon
-- Web Services account.
updateDeploymentGroupResponse_hooksNotCleanedUp :: Lens.Lens' UpdateDeploymentGroupResponse (Prelude.Maybe [AutoScalingGroup])
updateDeploymentGroupResponse_hooksNotCleanedUp = Lens.lens (\UpdateDeploymentGroupResponse' {hooksNotCleanedUp} -> hooksNotCleanedUp) (\s@UpdateDeploymentGroupResponse' {} a -> s {hooksNotCleanedUp = a} :: UpdateDeploymentGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateDeploymentGroupResponse_httpStatus :: Lens.Lens' UpdateDeploymentGroupResponse Prelude.Int
updateDeploymentGroupResponse_httpStatus = Lens.lens (\UpdateDeploymentGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateDeploymentGroupResponse' {} a -> s {httpStatus = a} :: UpdateDeploymentGroupResponse)

instance Prelude.NFData UpdateDeploymentGroupResponse where
  rnf UpdateDeploymentGroupResponse' {..} =
    Prelude.rnf hooksNotCleanedUp `Prelude.seq`
      Prelude.rnf httpStatus
