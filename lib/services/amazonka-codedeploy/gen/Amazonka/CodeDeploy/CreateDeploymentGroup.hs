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
-- Module      : Amazonka.CodeDeploy.CreateDeploymentGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment group to which application revisions are deployed.
module Amazonka.CodeDeploy.CreateDeploymentGroup
  ( -- * Creating a Request
    CreateDeploymentGroup (..),
    newCreateDeploymentGroup,

    -- * Request Lenses
    createDeploymentGroup_ec2TagSet,
    createDeploymentGroup_deploymentConfigName,
    createDeploymentGroup_onPremisesTagSet,
    createDeploymentGroup_ec2TagFilters,
    createDeploymentGroup_ecsServices,
    createDeploymentGroup_blueGreenDeploymentConfiguration,
    createDeploymentGroup_loadBalancerInfo,
    createDeploymentGroup_outdatedInstancesStrategy,
    createDeploymentGroup_onPremisesInstanceTagFilters,
    createDeploymentGroup_alarmConfiguration,
    createDeploymentGroup_triggerConfigurations,
    createDeploymentGroup_autoScalingGroups,
    createDeploymentGroup_deploymentStyle,
    createDeploymentGroup_autoRollbackConfiguration,
    createDeploymentGroup_tags,
    createDeploymentGroup_applicationName,
    createDeploymentGroup_deploymentGroupName,
    createDeploymentGroup_serviceRoleArn,

    -- * Destructuring the Response
    CreateDeploymentGroupResponse (..),
    newCreateDeploymentGroupResponse,

    -- * Response Lenses
    createDeploymentGroupResponse_deploymentGroupId,
    createDeploymentGroupResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateDeploymentGroup@ operation.
--
-- /See:/ 'newCreateDeploymentGroup' smart constructor.
data CreateDeploymentGroup = CreateDeploymentGroup'
  { -- | Information about groups of tags applied to EC2 instances. The
    -- deployment group includes only EC2 instances identified by all the tag
    -- groups. Cannot be used in the same call as @ec2TagFilters@.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | If specified, the deployment configuration name can be either one of the
    -- predefined configurations provided with AWS CodeDeploy or a custom
    -- deployment configuration that you create by calling the create
    -- deployment configuration operation.
    --
    -- @CodeDeployDefault.OneAtATime@ is the default deployment configuration.
    -- It is used if a configuration isn\'t specified for the deployment or
    -- deployment group.
    --
    -- For more information about the predefined deployment configurations in
    -- AWS CodeDeploy, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy>
    -- in the /AWS CodeDeploy User Guide/.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | Information about groups of tags applied to on-premises instances. The
    -- deployment group includes only on-premises instances identified by all
    -- of the tag groups. Cannot be used in the same call as
    -- @onPremisesInstanceTagFilters@.
    onPremisesTagSet :: Prelude.Maybe OnPremisesTagSet,
    -- | The Amazon EC2 tags on which to filter. The deployment group includes
    -- EC2 instances with any of the specified tags. Cannot be used in the same
    -- call as ec2TagSet.
    ec2TagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Prelude.Maybe [ECSService],
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | Information about the load balancer used in a deployment.
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
    -- includes on-premises instances with any of the specified tags. Cannot be
    -- used in the same call as @OnPremisesTagSet@.
    onPremisesInstanceTagFilters :: Prelude.Maybe [TagFilter],
    -- | Information to add about Amazon CloudWatch alarms when the deployment
    -- group is created.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | Information about triggers to create when the deployment group is
    -- created. For examples, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event>
    -- in the /AWS CodeDeploy User Guide/.
    triggerConfigurations :: Prelude.Maybe [TriggerConfig],
    -- | A list of associated Amazon EC2 Auto Scaling groups.
    autoScalingGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about the type of deployment, in-place or blue\/green, that
    -- you want to run and whether to route deployment traffic behind a load
    -- balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | Configuration information for an automatic rollback that is added when a
    -- deployment group is created.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | The metadata that you apply to CodeDeploy deployment groups to help you
    -- organize and categorize them. Each tag consists of a key and an optional
    -- value, both of which you define.
    tags :: Prelude.Maybe [Tag],
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Prelude.Text,
    -- | The name of a new deployment group for the specified application.
    deploymentGroupName :: Prelude.Text,
    -- | A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to
    -- act on the user\'s behalf when interacting with AWS services.
    serviceRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2TagSet', 'createDeploymentGroup_ec2TagSet' - Information about groups of tags applied to EC2 instances. The
-- deployment group includes only EC2 instances identified by all the tag
-- groups. Cannot be used in the same call as @ec2TagFilters@.
--
-- 'deploymentConfigName', 'createDeploymentGroup_deploymentConfigName' - If specified, the deployment configuration name can be either one of the
-- predefined configurations provided with AWS CodeDeploy or a custom
-- deployment configuration that you create by calling the create
-- deployment configuration operation.
--
-- @CodeDeployDefault.OneAtATime@ is the default deployment configuration.
-- It is used if a configuration isn\'t specified for the deployment or
-- deployment group.
--
-- For more information about the predefined deployment configurations in
-- AWS CodeDeploy, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy>
-- in the /AWS CodeDeploy User Guide/.
--
-- 'onPremisesTagSet', 'createDeploymentGroup_onPremisesTagSet' - Information about groups of tags applied to on-premises instances. The
-- deployment group includes only on-premises instances identified by all
-- of the tag groups. Cannot be used in the same call as
-- @onPremisesInstanceTagFilters@.
--
-- 'ec2TagFilters', 'createDeploymentGroup_ec2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags. Cannot be used in the same
-- call as ec2TagSet.
--
-- 'ecsServices', 'createDeploymentGroup_ecsServices' - The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
--
-- 'blueGreenDeploymentConfiguration', 'createDeploymentGroup_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for a deployment group.
--
-- 'loadBalancerInfo', 'createDeploymentGroup_loadBalancerInfo' - Information about the load balancer used in a deployment.
--
-- 'outdatedInstancesStrategy', 'createDeploymentGroup_outdatedInstancesStrategy' - Indicates what happens when new EC2 instances are launched
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
-- 'onPremisesInstanceTagFilters', 'createDeploymentGroup_onPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags. Cannot be
-- used in the same call as @OnPremisesTagSet@.
--
-- 'alarmConfiguration', 'createDeploymentGroup_alarmConfiguration' - Information to add about Amazon CloudWatch alarms when the deployment
-- group is created.
--
-- 'triggerConfigurations', 'createDeploymentGroup_triggerConfigurations' - Information about triggers to create when the deployment group is
-- created. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event>
-- in the /AWS CodeDeploy User Guide/.
--
-- 'autoScalingGroups', 'createDeploymentGroup_autoScalingGroups' - A list of associated Amazon EC2 Auto Scaling groups.
--
-- 'deploymentStyle', 'createDeploymentGroup_deploymentStyle' - Information about the type of deployment, in-place or blue\/green, that
-- you want to run and whether to route deployment traffic behind a load
-- balancer.
--
-- 'autoRollbackConfiguration', 'createDeploymentGroup_autoRollbackConfiguration' - Configuration information for an automatic rollback that is added when a
-- deployment group is created.
--
-- 'tags', 'createDeploymentGroup_tags' - The metadata that you apply to CodeDeploy deployment groups to help you
-- organize and categorize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- 'applicationName', 'createDeploymentGroup_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
--
-- 'deploymentGroupName', 'createDeploymentGroup_deploymentGroupName' - The name of a new deployment group for the specified application.
--
-- 'serviceRoleArn', 'createDeploymentGroup_serviceRoleArn' - A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to
-- act on the user\'s behalf when interacting with AWS services.
newCreateDeploymentGroup ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'deploymentGroupName'
  Prelude.Text ->
  -- | 'serviceRoleArn'
  Prelude.Text ->
  CreateDeploymentGroup
newCreateDeploymentGroup
  pApplicationName_
  pDeploymentGroupName_
  pServiceRoleArn_ =
    CreateDeploymentGroup'
      { ec2TagSet = Prelude.Nothing,
        deploymentConfigName = Prelude.Nothing,
        onPremisesTagSet = Prelude.Nothing,
        ec2TagFilters = Prelude.Nothing,
        ecsServices = Prelude.Nothing,
        blueGreenDeploymentConfiguration = Prelude.Nothing,
        loadBalancerInfo = Prelude.Nothing,
        outdatedInstancesStrategy = Prelude.Nothing,
        onPremisesInstanceTagFilters = Prelude.Nothing,
        alarmConfiguration = Prelude.Nothing,
        triggerConfigurations = Prelude.Nothing,
        autoScalingGroups = Prelude.Nothing,
        deploymentStyle = Prelude.Nothing,
        autoRollbackConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        applicationName = pApplicationName_,
        deploymentGroupName = pDeploymentGroupName_,
        serviceRoleArn = pServiceRoleArn_
      }

-- | Information about groups of tags applied to EC2 instances. The
-- deployment group includes only EC2 instances identified by all the tag
-- groups. Cannot be used in the same call as @ec2TagFilters@.
createDeploymentGroup_ec2TagSet :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe EC2TagSet)
createDeploymentGroup_ec2TagSet = Lens.lens (\CreateDeploymentGroup' {ec2TagSet} -> ec2TagSet) (\s@CreateDeploymentGroup' {} a -> s {ec2TagSet = a} :: CreateDeploymentGroup)

-- | If specified, the deployment configuration name can be either one of the
-- predefined configurations provided with AWS CodeDeploy or a custom
-- deployment configuration that you create by calling the create
-- deployment configuration operation.
--
-- @CodeDeployDefault.OneAtATime@ is the default deployment configuration.
-- It is used if a configuration isn\'t specified for the deployment or
-- deployment group.
--
-- For more information about the predefined deployment configurations in
-- AWS CodeDeploy, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/deployment-configurations.html Working with Deployment Configurations in CodeDeploy>
-- in the /AWS CodeDeploy User Guide/.
createDeploymentGroup_deploymentConfigName :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe Prelude.Text)
createDeploymentGroup_deploymentConfigName = Lens.lens (\CreateDeploymentGroup' {deploymentConfigName} -> deploymentConfigName) (\s@CreateDeploymentGroup' {} a -> s {deploymentConfigName = a} :: CreateDeploymentGroup)

-- | Information about groups of tags applied to on-premises instances. The
-- deployment group includes only on-premises instances identified by all
-- of the tag groups. Cannot be used in the same call as
-- @onPremisesInstanceTagFilters@.
createDeploymentGroup_onPremisesTagSet :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe OnPremisesTagSet)
createDeploymentGroup_onPremisesTagSet = Lens.lens (\CreateDeploymentGroup' {onPremisesTagSet} -> onPremisesTagSet) (\s@CreateDeploymentGroup' {} a -> s {onPremisesTagSet = a} :: CreateDeploymentGroup)

-- | The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags. Cannot be used in the same
-- call as ec2TagSet.
createDeploymentGroup_ec2TagFilters :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [EC2TagFilter])
createDeploymentGroup_ec2TagFilters = Lens.lens (\CreateDeploymentGroup' {ec2TagFilters} -> ec2TagFilters) (\s@CreateDeploymentGroup' {} a -> s {ec2TagFilters = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
createDeploymentGroup_ecsServices :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [ECSService])
createDeploymentGroup_ecsServices = Lens.lens (\CreateDeploymentGroup' {ecsServices} -> ecsServices) (\s@CreateDeploymentGroup' {} a -> s {ecsServices = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Information about blue\/green deployment options for a deployment group.
createDeploymentGroup_blueGreenDeploymentConfiguration :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe BlueGreenDeploymentConfiguration)
createDeploymentGroup_blueGreenDeploymentConfiguration = Lens.lens (\CreateDeploymentGroup' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@CreateDeploymentGroup' {} a -> s {blueGreenDeploymentConfiguration = a} :: CreateDeploymentGroup)

-- | Information about the load balancer used in a deployment.
createDeploymentGroup_loadBalancerInfo :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe LoadBalancerInfo)
createDeploymentGroup_loadBalancerInfo = Lens.lens (\CreateDeploymentGroup' {loadBalancerInfo} -> loadBalancerInfo) (\s@CreateDeploymentGroup' {} a -> s {loadBalancerInfo = a} :: CreateDeploymentGroup)

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
createDeploymentGroup_outdatedInstancesStrategy :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe OutdatedInstancesStrategy)
createDeploymentGroup_outdatedInstancesStrategy = Lens.lens (\CreateDeploymentGroup' {outdatedInstancesStrategy} -> outdatedInstancesStrategy) (\s@CreateDeploymentGroup' {} a -> s {outdatedInstancesStrategy = a} :: CreateDeploymentGroup)

-- | The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags. Cannot be
-- used in the same call as @OnPremisesTagSet@.
createDeploymentGroup_onPremisesInstanceTagFilters :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [TagFilter])
createDeploymentGroup_onPremisesInstanceTagFilters = Lens.lens (\CreateDeploymentGroup' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@CreateDeploymentGroup' {} a -> s {onPremisesInstanceTagFilters = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Information to add about Amazon CloudWatch alarms when the deployment
-- group is created.
createDeploymentGroup_alarmConfiguration :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe AlarmConfiguration)
createDeploymentGroup_alarmConfiguration = Lens.lens (\CreateDeploymentGroup' {alarmConfiguration} -> alarmConfiguration) (\s@CreateDeploymentGroup' {} a -> s {alarmConfiguration = a} :: CreateDeploymentGroup)

-- | Information about triggers to create when the deployment group is
-- created. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event>
-- in the /AWS CodeDeploy User Guide/.
createDeploymentGroup_triggerConfigurations :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [TriggerConfig])
createDeploymentGroup_triggerConfigurations = Lens.lens (\CreateDeploymentGroup' {triggerConfigurations} -> triggerConfigurations) (\s@CreateDeploymentGroup' {} a -> s {triggerConfigurations = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | A list of associated Amazon EC2 Auto Scaling groups.
createDeploymentGroup_autoScalingGroups :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [Prelude.Text])
createDeploymentGroup_autoScalingGroups = Lens.lens (\CreateDeploymentGroup' {autoScalingGroups} -> autoScalingGroups) (\s@CreateDeploymentGroup' {} a -> s {autoScalingGroups = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | Information about the type of deployment, in-place or blue\/green, that
-- you want to run and whether to route deployment traffic behind a load
-- balancer.
createDeploymentGroup_deploymentStyle :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe DeploymentStyle)
createDeploymentGroup_deploymentStyle = Lens.lens (\CreateDeploymentGroup' {deploymentStyle} -> deploymentStyle) (\s@CreateDeploymentGroup' {} a -> s {deploymentStyle = a} :: CreateDeploymentGroup)

-- | Configuration information for an automatic rollback that is added when a
-- deployment group is created.
createDeploymentGroup_autoRollbackConfiguration :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe AutoRollbackConfiguration)
createDeploymentGroup_autoRollbackConfiguration = Lens.lens (\CreateDeploymentGroup' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@CreateDeploymentGroup' {} a -> s {autoRollbackConfiguration = a} :: CreateDeploymentGroup)

-- | The metadata that you apply to CodeDeploy deployment groups to help you
-- organize and categorize them. Each tag consists of a key and an optional
-- value, both of which you define.
createDeploymentGroup_tags :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [Tag])
createDeploymentGroup_tags = Lens.lens (\CreateDeploymentGroup' {tags} -> tags) (\s@CreateDeploymentGroup' {} a -> s {tags = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
createDeploymentGroup_applicationName :: Lens.Lens' CreateDeploymentGroup Prelude.Text
createDeploymentGroup_applicationName = Lens.lens (\CreateDeploymentGroup' {applicationName} -> applicationName) (\s@CreateDeploymentGroup' {} a -> s {applicationName = a} :: CreateDeploymentGroup)

-- | The name of a new deployment group for the specified application.
createDeploymentGroup_deploymentGroupName :: Lens.Lens' CreateDeploymentGroup Prelude.Text
createDeploymentGroup_deploymentGroupName = Lens.lens (\CreateDeploymentGroup' {deploymentGroupName} -> deploymentGroupName) (\s@CreateDeploymentGroup' {} a -> s {deploymentGroupName = a} :: CreateDeploymentGroup)

-- | A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to
-- act on the user\'s behalf when interacting with AWS services.
createDeploymentGroup_serviceRoleArn :: Lens.Lens' CreateDeploymentGroup Prelude.Text
createDeploymentGroup_serviceRoleArn = Lens.lens (\CreateDeploymentGroup' {serviceRoleArn} -> serviceRoleArn) (\s@CreateDeploymentGroup' {} a -> s {serviceRoleArn = a} :: CreateDeploymentGroup)

instance Core.AWSRequest CreateDeploymentGroup where
  type
    AWSResponse CreateDeploymentGroup =
      CreateDeploymentGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentGroupResponse'
            Prelude.<$> (x Core..?> "deploymentGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeploymentGroup

instance Prelude.NFData CreateDeploymentGroup

instance Core.ToHeaders CreateDeploymentGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.CreateDeploymentGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDeploymentGroup where
  toJSON CreateDeploymentGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ec2TagSet" Core..=) Prelude.<$> ec2TagSet,
            ("deploymentConfigName" Core..=)
              Prelude.<$> deploymentConfigName,
            ("onPremisesTagSet" Core..=)
              Prelude.<$> onPremisesTagSet,
            ("ec2TagFilters" Core..=) Prelude.<$> ec2TagFilters,
            ("ecsServices" Core..=) Prelude.<$> ecsServices,
            ("blueGreenDeploymentConfiguration" Core..=)
              Prelude.<$> blueGreenDeploymentConfiguration,
            ("loadBalancerInfo" Core..=)
              Prelude.<$> loadBalancerInfo,
            ("outdatedInstancesStrategy" Core..=)
              Prelude.<$> outdatedInstancesStrategy,
            ("onPremisesInstanceTagFilters" Core..=)
              Prelude.<$> onPremisesInstanceTagFilters,
            ("alarmConfiguration" Core..=)
              Prelude.<$> alarmConfiguration,
            ("triggerConfigurations" Core..=)
              Prelude.<$> triggerConfigurations,
            ("autoScalingGroups" Core..=)
              Prelude.<$> autoScalingGroups,
            ("deploymentStyle" Core..=)
              Prelude.<$> deploymentStyle,
            ("autoRollbackConfiguration" Core..=)
              Prelude.<$> autoRollbackConfiguration,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("applicationName" Core..= applicationName),
            Prelude.Just
              ("deploymentGroupName" Core..= deploymentGroupName),
            Prelude.Just
              ("serviceRoleArn" Core..= serviceRoleArn)
          ]
      )

instance Core.ToPath CreateDeploymentGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDeploymentGroup where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateDeploymentGroup@ operation.
--
-- /See:/ 'newCreateDeploymentGroupResponse' smart constructor.
data CreateDeploymentGroupResponse = CreateDeploymentGroupResponse'
  { -- | A unique deployment group ID.
    deploymentGroupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentGroupId', 'createDeploymentGroupResponse_deploymentGroupId' - A unique deployment group ID.
--
-- 'httpStatus', 'createDeploymentGroupResponse_httpStatus' - The response's http status code.
newCreateDeploymentGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeploymentGroupResponse
newCreateDeploymentGroupResponse pHttpStatus_ =
  CreateDeploymentGroupResponse'
    { deploymentGroupId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique deployment group ID.
createDeploymentGroupResponse_deploymentGroupId :: Lens.Lens' CreateDeploymentGroupResponse (Prelude.Maybe Prelude.Text)
createDeploymentGroupResponse_deploymentGroupId = Lens.lens (\CreateDeploymentGroupResponse' {deploymentGroupId} -> deploymentGroupId) (\s@CreateDeploymentGroupResponse' {} a -> s {deploymentGroupId = a} :: CreateDeploymentGroupResponse)

-- | The response's http status code.
createDeploymentGroupResponse_httpStatus :: Lens.Lens' CreateDeploymentGroupResponse Prelude.Int
createDeploymentGroupResponse_httpStatus = Lens.lens (\CreateDeploymentGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentGroupResponse' {} a -> s {httpStatus = a} :: CreateDeploymentGroupResponse)

instance Prelude.NFData CreateDeploymentGroupResponse
