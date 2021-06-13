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
-- Module      : Network.AWS.CodeDeploy.UpdateDeploymentGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a deployment group.
module Network.AWS.CodeDeploy.UpdateDeploymentGroup
  ( -- * Creating a Request
    UpdateDeploymentGroup (..),
    newUpdateDeploymentGroup,

    -- * Request Lenses
    updateDeploymentGroup_onPremisesTagSet,
    updateDeploymentGroup_serviceRoleArn,
    updateDeploymentGroup_deploymentConfigName,
    updateDeploymentGroup_autoRollbackConfiguration,
    updateDeploymentGroup_triggerConfigurations,
    updateDeploymentGroup_ec2TagFilters,
    updateDeploymentGroup_onPremisesInstanceTagFilters,
    updateDeploymentGroup_loadBalancerInfo,
    updateDeploymentGroup_ec2TagSet,
    updateDeploymentGroup_blueGreenDeploymentConfiguration,
    updateDeploymentGroup_autoScalingGroups,
    updateDeploymentGroup_deploymentStyle,
    updateDeploymentGroup_alarmConfiguration,
    updateDeploymentGroup_ecsServices,
    updateDeploymentGroup_newDeploymentGroupName,
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

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateDeploymentGroup@ operation.
--
-- /See:/ 'newUpdateDeploymentGroup' smart constructor.
data UpdateDeploymentGroup = UpdateDeploymentGroup'
  { -- | Information about an on-premises instance tag set. The deployment group
    -- includes only on-premises instances identified by all the tag groups.
    onPremisesTagSet :: Prelude.Maybe OnPremisesTagSet,
    -- | A replacement ARN for the service role, if you want to change it.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The replacement deployment configuration name to use, if you want to
    -- change it.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | Information for an automatic rollback configuration that is added or
    -- changed when a deployment group is updated.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | Information about triggers to change when the deployment group is
    -- updated. For examples, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group>
    -- in the /AWS CodeDeploy User Guide/.
    triggerConfigurations :: Prelude.Maybe [TriggerConfig],
    -- | The replacement set of Amazon EC2 tags on which to filter, if you want
    -- to change them. To keep the existing tags, enter their names. To remove
    -- tags, do not enter any tag names.
    ec2TagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | The replacement set of on-premises instance tags on which to filter, if
    -- you want to change them. To keep the existing tags, enter their names.
    -- To remove tags, do not enter any tag names.
    onPremisesInstanceTagFilters :: Prelude.Maybe [TagFilter],
    -- | Information about the load balancer used in a deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | Information about groups of tags applied to on-premises instances. The
    -- deployment group includes only EC2 instances identified by all the tag
    -- groups.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | The replacement list of Auto Scaling groups to be included in the
    -- deployment group, if you want to change them. To keep the Auto Scaling
    -- groups, enter their names. To remove Auto Scaling groups, do not enter
    -- any Auto Scaling group names.
    autoScalingGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about the type of deployment, either in-place or
    -- blue\/green, you want to run and whether to route deployment traffic
    -- behind a load balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | Information to add or change about Amazon CloudWatch alarms when the
    -- deployment group is updated.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Prelude.Maybe [ECSService],
    -- | The new name of the deployment group, if you want to change it.
    newDeploymentGroupName' :: Prelude.Maybe Prelude.Text,
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
-- 'onPremisesTagSet', 'updateDeploymentGroup_onPremisesTagSet' - Information about an on-premises instance tag set. The deployment group
-- includes only on-premises instances identified by all the tag groups.
--
-- 'serviceRoleArn', 'updateDeploymentGroup_serviceRoleArn' - A replacement ARN for the service role, if you want to change it.
--
-- 'deploymentConfigName', 'updateDeploymentGroup_deploymentConfigName' - The replacement deployment configuration name to use, if you want to
-- change it.
--
-- 'autoRollbackConfiguration', 'updateDeploymentGroup_autoRollbackConfiguration' - Information for an automatic rollback configuration that is added or
-- changed when a deployment group is updated.
--
-- 'triggerConfigurations', 'updateDeploymentGroup_triggerConfigurations' - Information about triggers to change when the deployment group is
-- updated. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group>
-- in the /AWS CodeDeploy User Guide/.
--
-- 'ec2TagFilters', 'updateDeploymentGroup_ec2TagFilters' - The replacement set of Amazon EC2 tags on which to filter, if you want
-- to change them. To keep the existing tags, enter their names. To remove
-- tags, do not enter any tag names.
--
-- 'onPremisesInstanceTagFilters', 'updateDeploymentGroup_onPremisesInstanceTagFilters' - The replacement set of on-premises instance tags on which to filter, if
-- you want to change them. To keep the existing tags, enter their names.
-- To remove tags, do not enter any tag names.
--
-- 'loadBalancerInfo', 'updateDeploymentGroup_loadBalancerInfo' - Information about the load balancer used in a deployment.
--
-- 'ec2TagSet', 'updateDeploymentGroup_ec2TagSet' - Information about groups of tags applied to on-premises instances. The
-- deployment group includes only EC2 instances identified by all the tag
-- groups.
--
-- 'blueGreenDeploymentConfiguration', 'updateDeploymentGroup_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for a deployment group.
--
-- 'autoScalingGroups', 'updateDeploymentGroup_autoScalingGroups' - The replacement list of Auto Scaling groups to be included in the
-- deployment group, if you want to change them. To keep the Auto Scaling
-- groups, enter their names. To remove Auto Scaling groups, do not enter
-- any Auto Scaling group names.
--
-- 'deploymentStyle', 'updateDeploymentGroup_deploymentStyle' - Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
--
-- 'alarmConfiguration', 'updateDeploymentGroup_alarmConfiguration' - Information to add or change about Amazon CloudWatch alarms when the
-- deployment group is updated.
--
-- 'ecsServices', 'updateDeploymentGroup_ecsServices' - The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
--
-- 'newDeploymentGroupName'', 'updateDeploymentGroup_newDeploymentGroupName' - The new name of the deployment group, if you want to change it.
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
      { onPremisesTagSet =
          Prelude.Nothing,
        serviceRoleArn = Prelude.Nothing,
        deploymentConfigName = Prelude.Nothing,
        autoRollbackConfiguration = Prelude.Nothing,
        triggerConfigurations = Prelude.Nothing,
        ec2TagFilters = Prelude.Nothing,
        onPremisesInstanceTagFilters = Prelude.Nothing,
        loadBalancerInfo = Prelude.Nothing,
        ec2TagSet = Prelude.Nothing,
        blueGreenDeploymentConfiguration = Prelude.Nothing,
        autoScalingGroups = Prelude.Nothing,
        deploymentStyle = Prelude.Nothing,
        alarmConfiguration = Prelude.Nothing,
        ecsServices = Prelude.Nothing,
        newDeploymentGroupName' = Prelude.Nothing,
        applicationName = pApplicationName_,
        currentDeploymentGroupName =
          pCurrentDeploymentGroupName_
      }

-- | Information about an on-premises instance tag set. The deployment group
-- includes only on-premises instances identified by all the tag groups.
updateDeploymentGroup_onPremisesTagSet :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe OnPremisesTagSet)
updateDeploymentGroup_onPremisesTagSet = Lens.lens (\UpdateDeploymentGroup' {onPremisesTagSet} -> onPremisesTagSet) (\s@UpdateDeploymentGroup' {} a -> s {onPremisesTagSet = a} :: UpdateDeploymentGroup)

-- | A replacement ARN for the service role, if you want to change it.
updateDeploymentGroup_serviceRoleArn :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe Prelude.Text)
updateDeploymentGroup_serviceRoleArn = Lens.lens (\UpdateDeploymentGroup' {serviceRoleArn} -> serviceRoleArn) (\s@UpdateDeploymentGroup' {} a -> s {serviceRoleArn = a} :: UpdateDeploymentGroup)

-- | The replacement deployment configuration name to use, if you want to
-- change it.
updateDeploymentGroup_deploymentConfigName :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe Prelude.Text)
updateDeploymentGroup_deploymentConfigName = Lens.lens (\UpdateDeploymentGroup' {deploymentConfigName} -> deploymentConfigName) (\s@UpdateDeploymentGroup' {} a -> s {deploymentConfigName = a} :: UpdateDeploymentGroup)

-- | Information for an automatic rollback configuration that is added or
-- changed when a deployment group is updated.
updateDeploymentGroup_autoRollbackConfiguration :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe AutoRollbackConfiguration)
updateDeploymentGroup_autoRollbackConfiguration = Lens.lens (\UpdateDeploymentGroup' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@UpdateDeploymentGroup' {} a -> s {autoRollbackConfiguration = a} :: UpdateDeploymentGroup)

-- | Information about triggers to change when the deployment group is
-- updated. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-edit.html Edit a Trigger in a CodeDeploy Deployment Group>
-- in the /AWS CodeDeploy User Guide/.
updateDeploymentGroup_triggerConfigurations :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [TriggerConfig])
updateDeploymentGroup_triggerConfigurations = Lens.lens (\UpdateDeploymentGroup' {triggerConfigurations} -> triggerConfigurations) (\s@UpdateDeploymentGroup' {} a -> s {triggerConfigurations = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The replacement set of Amazon EC2 tags on which to filter, if you want
-- to change them. To keep the existing tags, enter their names. To remove
-- tags, do not enter any tag names.
updateDeploymentGroup_ec2TagFilters :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [EC2TagFilter])
updateDeploymentGroup_ec2TagFilters = Lens.lens (\UpdateDeploymentGroup' {ec2TagFilters} -> ec2TagFilters) (\s@UpdateDeploymentGroup' {} a -> s {ec2TagFilters = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The replacement set of on-premises instance tags on which to filter, if
-- you want to change them. To keep the existing tags, enter their names.
-- To remove tags, do not enter any tag names.
updateDeploymentGroup_onPremisesInstanceTagFilters :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [TagFilter])
updateDeploymentGroup_onPremisesInstanceTagFilters = Lens.lens (\UpdateDeploymentGroup' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@UpdateDeploymentGroup' {} a -> s {onPremisesInstanceTagFilters = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the load balancer used in a deployment.
updateDeploymentGroup_loadBalancerInfo :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe LoadBalancerInfo)
updateDeploymentGroup_loadBalancerInfo = Lens.lens (\UpdateDeploymentGroup' {loadBalancerInfo} -> loadBalancerInfo) (\s@UpdateDeploymentGroup' {} a -> s {loadBalancerInfo = a} :: UpdateDeploymentGroup)

-- | Information about groups of tags applied to on-premises instances. The
-- deployment group includes only EC2 instances identified by all the tag
-- groups.
updateDeploymentGroup_ec2TagSet :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe EC2TagSet)
updateDeploymentGroup_ec2TagSet = Lens.lens (\UpdateDeploymentGroup' {ec2TagSet} -> ec2TagSet) (\s@UpdateDeploymentGroup' {} a -> s {ec2TagSet = a} :: UpdateDeploymentGroup)

-- | Information about blue\/green deployment options for a deployment group.
updateDeploymentGroup_blueGreenDeploymentConfiguration :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe BlueGreenDeploymentConfiguration)
updateDeploymentGroup_blueGreenDeploymentConfiguration = Lens.lens (\UpdateDeploymentGroup' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@UpdateDeploymentGroup' {} a -> s {blueGreenDeploymentConfiguration = a} :: UpdateDeploymentGroup)

-- | The replacement list of Auto Scaling groups to be included in the
-- deployment group, if you want to change them. To keep the Auto Scaling
-- groups, enter their names. To remove Auto Scaling groups, do not enter
-- any Auto Scaling group names.
updateDeploymentGroup_autoScalingGroups :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [Prelude.Text])
updateDeploymentGroup_autoScalingGroups = Lens.lens (\UpdateDeploymentGroup' {autoScalingGroups} -> autoScalingGroups) (\s@UpdateDeploymentGroup' {} a -> s {autoScalingGroups = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the type of deployment, either in-place or
-- blue\/green, you want to run and whether to route deployment traffic
-- behind a load balancer.
updateDeploymentGroup_deploymentStyle :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe DeploymentStyle)
updateDeploymentGroup_deploymentStyle = Lens.lens (\UpdateDeploymentGroup' {deploymentStyle} -> deploymentStyle) (\s@UpdateDeploymentGroup' {} a -> s {deploymentStyle = a} :: UpdateDeploymentGroup)

-- | Information to add or change about Amazon CloudWatch alarms when the
-- deployment group is updated.
updateDeploymentGroup_alarmConfiguration :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe AlarmConfiguration)
updateDeploymentGroup_alarmConfiguration = Lens.lens (\UpdateDeploymentGroup' {alarmConfiguration} -> alarmConfiguration) (\s@UpdateDeploymentGroup' {} a -> s {alarmConfiguration = a} :: UpdateDeploymentGroup)

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
updateDeploymentGroup_ecsServices :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe [ECSService])
updateDeploymentGroup_ecsServices = Lens.lens (\UpdateDeploymentGroup' {ecsServices} -> ecsServices) (\s@UpdateDeploymentGroup' {} a -> s {ecsServices = a} :: UpdateDeploymentGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The new name of the deployment group, if you want to change it.
updateDeploymentGroup_newDeploymentGroupName :: Lens.Lens' UpdateDeploymentGroup (Prelude.Maybe Prelude.Text)
updateDeploymentGroup_newDeploymentGroupName = Lens.lens (\UpdateDeploymentGroup' {newDeploymentGroupName'} -> newDeploymentGroupName') (\s@UpdateDeploymentGroup' {} a -> s {newDeploymentGroupName' = a} :: UpdateDeploymentGroup)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDeploymentGroupResponse'
            Prelude.<$> ( x Core..?> "hooksNotCleanedUp"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeploymentGroup

instance Prelude.NFData UpdateDeploymentGroup

instance Core.ToHeaders UpdateDeploymentGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.UpdateDeploymentGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDeploymentGroup where
  toJSON UpdateDeploymentGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("onPremisesTagSet" Core..=)
              Prelude.<$> onPremisesTagSet,
            ("serviceRoleArn" Core..=)
              Prelude.<$> serviceRoleArn,
            ("deploymentConfigName" Core..=)
              Prelude.<$> deploymentConfigName,
            ("autoRollbackConfiguration" Core..=)
              Prelude.<$> autoRollbackConfiguration,
            ("triggerConfigurations" Core..=)
              Prelude.<$> triggerConfigurations,
            ("ec2TagFilters" Core..=) Prelude.<$> ec2TagFilters,
            ("onPremisesInstanceTagFilters" Core..=)
              Prelude.<$> onPremisesInstanceTagFilters,
            ("loadBalancerInfo" Core..=)
              Prelude.<$> loadBalancerInfo,
            ("ec2TagSet" Core..=) Prelude.<$> ec2TagSet,
            ("blueGreenDeploymentConfiguration" Core..=)
              Prelude.<$> blueGreenDeploymentConfiguration,
            ("autoScalingGroups" Core..=)
              Prelude.<$> autoScalingGroups,
            ("deploymentStyle" Core..=)
              Prelude.<$> deploymentStyle,
            ("alarmConfiguration" Core..=)
              Prelude.<$> alarmConfiguration,
            ("ecsServices" Core..=) Prelude.<$> ecsServices,
            ("newDeploymentGroupName" Core..=)
              Prelude.<$> newDeploymentGroupName',
            Prelude.Just
              ("applicationName" Core..= applicationName),
            Prelude.Just
              ( "currentDeploymentGroupName"
                  Core..= currentDeploymentGroupName
              )
          ]
      )

instance Core.ToPath UpdateDeploymentGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDeploymentGroup where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @UpdateDeploymentGroup@ operation.
--
-- /See:/ 'newUpdateDeploymentGroupResponse' smart constructor.
data UpdateDeploymentGroupResponse = UpdateDeploymentGroupResponse'
  { -- | If the output contains no data, and the corresponding deployment group
    -- contained at least one Auto Scaling group, AWS CodeDeploy successfully
    -- removed all corresponding Auto Scaling lifecycle event hooks from the
    -- AWS account. If the output contains data, AWS CodeDeploy could not
    -- remove some Auto Scaling lifecycle event hooks from the AWS account.
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
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- AWS account. If the output contains data, AWS CodeDeploy could not
-- remove some Auto Scaling lifecycle event hooks from the AWS account.
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
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- AWS account. If the output contains data, AWS CodeDeploy could not
-- remove some Auto Scaling lifecycle event hooks from the AWS account.
updateDeploymentGroupResponse_hooksNotCleanedUp :: Lens.Lens' UpdateDeploymentGroupResponse (Prelude.Maybe [AutoScalingGroup])
updateDeploymentGroupResponse_hooksNotCleanedUp = Lens.lens (\UpdateDeploymentGroupResponse' {hooksNotCleanedUp} -> hooksNotCleanedUp) (\s@UpdateDeploymentGroupResponse' {} a -> s {hooksNotCleanedUp = a} :: UpdateDeploymentGroupResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateDeploymentGroupResponse_httpStatus :: Lens.Lens' UpdateDeploymentGroupResponse Prelude.Int
updateDeploymentGroupResponse_httpStatus = Lens.lens (\UpdateDeploymentGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateDeploymentGroupResponse' {} a -> s {httpStatus = a} :: UpdateDeploymentGroupResponse)

instance Prelude.NFData UpdateDeploymentGroupResponse
