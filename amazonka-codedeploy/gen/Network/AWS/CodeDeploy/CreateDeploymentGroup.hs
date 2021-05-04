{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment group to which application revisions are deployed.
module Network.AWS.CodeDeploy.CreateDeploymentGroup
  ( -- * Creating a Request
    CreateDeploymentGroup (..),
    newCreateDeploymentGroup,

    -- * Request Lenses
    createDeploymentGroup_onPremisesTagSet,
    createDeploymentGroup_deploymentConfigName,
    createDeploymentGroup_autoRollbackConfiguration,
    createDeploymentGroup_triggerConfigurations,
    createDeploymentGroup_ec2TagFilters,
    createDeploymentGroup_onPremisesInstanceTagFilters,
    createDeploymentGroup_loadBalancerInfo,
    createDeploymentGroup_ec2TagSet,
    createDeploymentGroup_blueGreenDeploymentConfiguration,
    createDeploymentGroup_tags,
    createDeploymentGroup_autoScalingGroups,
    createDeploymentGroup_deploymentStyle,
    createDeploymentGroup_alarmConfiguration,
    createDeploymentGroup_ecsServices,
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

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateDeploymentGroup@ operation.
--
-- /See:/ 'newCreateDeploymentGroup' smart constructor.
data CreateDeploymentGroup = CreateDeploymentGroup'
  { -- | Information about groups of tags applied to on-premises instances. The
    -- deployment group includes only on-premises instances identified by all
    -- of the tag groups. Cannot be used in the same call as
    -- @onPremisesInstanceTagFilters@.
    onPremisesTagSet :: Prelude.Maybe OnPremisesTagSet,
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
    -- | Configuration information for an automatic rollback that is added when a
    -- deployment group is created.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | Information about triggers to create when the deployment group is
    -- created. For examples, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event>
    -- in the /AWS CodeDeploy User Guide/.
    triggerConfigurations :: Prelude.Maybe [TriggerConfig],
    -- | The Amazon EC2 tags on which to filter. The deployment group includes
    -- EC2 instances with any of the specified tags. Cannot be used in the same
    -- call as ec2TagSet.
    ec2TagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | The on-premises instance tags on which to filter. The deployment group
    -- includes on-premises instances with any of the specified tags. Cannot be
    -- used in the same call as @OnPremisesTagSet@.
    onPremisesInstanceTagFilters :: Prelude.Maybe [TagFilter],
    -- | Information about the load balancer used in a deployment.
    loadBalancerInfo :: Prelude.Maybe LoadBalancerInfo,
    -- | Information about groups of tags applied to EC2 instances. The
    -- deployment group includes only EC2 instances identified by all the tag
    -- groups. Cannot be used in the same call as @ec2TagFilters@.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | Information about blue\/green deployment options for a deployment group.
    blueGreenDeploymentConfiguration :: Prelude.Maybe BlueGreenDeploymentConfiguration,
    -- | The metadata that you apply to CodeDeploy deployment groups to help you
    -- organize and categorize them. Each tag consists of a key and an optional
    -- value, both of which you define.
    tags :: Prelude.Maybe [Tag],
    -- | A list of associated Amazon EC2 Auto Scaling groups.
    autoScalingGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about the type of deployment, in-place or blue\/green, that
    -- you want to run and whether to route deployment traffic behind a load
    -- balancer.
    deploymentStyle :: Prelude.Maybe DeploymentStyle,
    -- | Information to add about Amazon CloudWatch alarms when the deployment
    -- group is created.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The target Amazon ECS services in the deployment group. This applies
    -- only to deployment groups that use the Amazon ECS compute platform. A
    -- target Amazon ECS service is specified as an Amazon ECS cluster and
    -- service name pair using the format @\<clustername>:\<servicename>@.
    ecsServices :: Prelude.Maybe [ECSService],
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Prelude.Text,
    -- | The name of a new deployment group for the specified application.
    deploymentGroupName :: Prelude.Text,
    -- | A service role Amazon Resource Name (ARN) that allows AWS CodeDeploy to
    -- act on the user\'s behalf when interacting with AWS services.
    serviceRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onPremisesTagSet', 'createDeploymentGroup_onPremisesTagSet' - Information about groups of tags applied to on-premises instances. The
-- deployment group includes only on-premises instances identified by all
-- of the tag groups. Cannot be used in the same call as
-- @onPremisesInstanceTagFilters@.
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
-- 'autoRollbackConfiguration', 'createDeploymentGroup_autoRollbackConfiguration' - Configuration information for an automatic rollback that is added when a
-- deployment group is created.
--
-- 'triggerConfigurations', 'createDeploymentGroup_triggerConfigurations' - Information about triggers to create when the deployment group is
-- created. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event>
-- in the /AWS CodeDeploy User Guide/.
--
-- 'ec2TagFilters', 'createDeploymentGroup_ec2TagFilters' - The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags. Cannot be used in the same
-- call as ec2TagSet.
--
-- 'onPremisesInstanceTagFilters', 'createDeploymentGroup_onPremisesInstanceTagFilters' - The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags. Cannot be
-- used in the same call as @OnPremisesTagSet@.
--
-- 'loadBalancerInfo', 'createDeploymentGroup_loadBalancerInfo' - Information about the load balancer used in a deployment.
--
-- 'ec2TagSet', 'createDeploymentGroup_ec2TagSet' - Information about groups of tags applied to EC2 instances. The
-- deployment group includes only EC2 instances identified by all the tag
-- groups. Cannot be used in the same call as @ec2TagFilters@.
--
-- 'blueGreenDeploymentConfiguration', 'createDeploymentGroup_blueGreenDeploymentConfiguration' - Information about blue\/green deployment options for a deployment group.
--
-- 'tags', 'createDeploymentGroup_tags' - The metadata that you apply to CodeDeploy deployment groups to help you
-- organize and categorize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- 'autoScalingGroups', 'createDeploymentGroup_autoScalingGroups' - A list of associated Amazon EC2 Auto Scaling groups.
--
-- 'deploymentStyle', 'createDeploymentGroup_deploymentStyle' - Information about the type of deployment, in-place or blue\/green, that
-- you want to run and whether to route deployment traffic behind a load
-- balancer.
--
-- 'alarmConfiguration', 'createDeploymentGroup_alarmConfiguration' - Information to add about Amazon CloudWatch alarms when the deployment
-- group is created.
--
-- 'ecsServices', 'createDeploymentGroup_ecsServices' - The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
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
      { onPremisesTagSet =
          Prelude.Nothing,
        deploymentConfigName = Prelude.Nothing,
        autoRollbackConfiguration = Prelude.Nothing,
        triggerConfigurations = Prelude.Nothing,
        ec2TagFilters = Prelude.Nothing,
        onPremisesInstanceTagFilters = Prelude.Nothing,
        loadBalancerInfo = Prelude.Nothing,
        ec2TagSet = Prelude.Nothing,
        blueGreenDeploymentConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        autoScalingGroups = Prelude.Nothing,
        deploymentStyle = Prelude.Nothing,
        alarmConfiguration = Prelude.Nothing,
        ecsServices = Prelude.Nothing,
        applicationName = pApplicationName_,
        deploymentGroupName = pDeploymentGroupName_,
        serviceRoleArn = pServiceRoleArn_
      }

-- | Information about groups of tags applied to on-premises instances. The
-- deployment group includes only on-premises instances identified by all
-- of the tag groups. Cannot be used in the same call as
-- @onPremisesInstanceTagFilters@.
createDeploymentGroup_onPremisesTagSet :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe OnPremisesTagSet)
createDeploymentGroup_onPremisesTagSet = Lens.lens (\CreateDeploymentGroup' {onPremisesTagSet} -> onPremisesTagSet) (\s@CreateDeploymentGroup' {} a -> s {onPremisesTagSet = a} :: CreateDeploymentGroup)

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

-- | Configuration information for an automatic rollback that is added when a
-- deployment group is created.
createDeploymentGroup_autoRollbackConfiguration :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe AutoRollbackConfiguration)
createDeploymentGroup_autoRollbackConfiguration = Lens.lens (\CreateDeploymentGroup' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@CreateDeploymentGroup' {} a -> s {autoRollbackConfiguration = a} :: CreateDeploymentGroup)

-- | Information about triggers to create when the deployment group is
-- created. For examples, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/how-to-notify-sns.html Create a Trigger for an AWS CodeDeploy Event>
-- in the /AWS CodeDeploy User Guide/.
createDeploymentGroup_triggerConfigurations :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [TriggerConfig])
createDeploymentGroup_triggerConfigurations = Lens.lens (\CreateDeploymentGroup' {triggerConfigurations} -> triggerConfigurations) (\s@CreateDeploymentGroup' {} a -> s {triggerConfigurations = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon EC2 tags on which to filter. The deployment group includes
-- EC2 instances with any of the specified tags. Cannot be used in the same
-- call as ec2TagSet.
createDeploymentGroup_ec2TagFilters :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [EC2TagFilter])
createDeploymentGroup_ec2TagFilters = Lens.lens (\CreateDeploymentGroup' {ec2TagFilters} -> ec2TagFilters) (\s@CreateDeploymentGroup' {} a -> s {ec2TagFilters = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The on-premises instance tags on which to filter. The deployment group
-- includes on-premises instances with any of the specified tags. Cannot be
-- used in the same call as @OnPremisesTagSet@.
createDeploymentGroup_onPremisesInstanceTagFilters :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [TagFilter])
createDeploymentGroup_onPremisesInstanceTagFilters = Lens.lens (\CreateDeploymentGroup' {onPremisesInstanceTagFilters} -> onPremisesInstanceTagFilters) (\s@CreateDeploymentGroup' {} a -> s {onPremisesInstanceTagFilters = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the load balancer used in a deployment.
createDeploymentGroup_loadBalancerInfo :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe LoadBalancerInfo)
createDeploymentGroup_loadBalancerInfo = Lens.lens (\CreateDeploymentGroup' {loadBalancerInfo} -> loadBalancerInfo) (\s@CreateDeploymentGroup' {} a -> s {loadBalancerInfo = a} :: CreateDeploymentGroup)

-- | Information about groups of tags applied to EC2 instances. The
-- deployment group includes only EC2 instances identified by all the tag
-- groups. Cannot be used in the same call as @ec2TagFilters@.
createDeploymentGroup_ec2TagSet :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe EC2TagSet)
createDeploymentGroup_ec2TagSet = Lens.lens (\CreateDeploymentGroup' {ec2TagSet} -> ec2TagSet) (\s@CreateDeploymentGroup' {} a -> s {ec2TagSet = a} :: CreateDeploymentGroup)

-- | Information about blue\/green deployment options for a deployment group.
createDeploymentGroup_blueGreenDeploymentConfiguration :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe BlueGreenDeploymentConfiguration)
createDeploymentGroup_blueGreenDeploymentConfiguration = Lens.lens (\CreateDeploymentGroup' {blueGreenDeploymentConfiguration} -> blueGreenDeploymentConfiguration) (\s@CreateDeploymentGroup' {} a -> s {blueGreenDeploymentConfiguration = a} :: CreateDeploymentGroup)

-- | The metadata that you apply to CodeDeploy deployment groups to help you
-- organize and categorize them. Each tag consists of a key and an optional
-- value, both of which you define.
createDeploymentGroup_tags :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [Tag])
createDeploymentGroup_tags = Lens.lens (\CreateDeploymentGroup' {tags} -> tags) (\s@CreateDeploymentGroup' {} a -> s {tags = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of associated Amazon EC2 Auto Scaling groups.
createDeploymentGroup_autoScalingGroups :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [Prelude.Text])
createDeploymentGroup_autoScalingGroups = Lens.lens (\CreateDeploymentGroup' {autoScalingGroups} -> autoScalingGroups) (\s@CreateDeploymentGroup' {} a -> s {autoScalingGroups = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the type of deployment, in-place or blue\/green, that
-- you want to run and whether to route deployment traffic behind a load
-- balancer.
createDeploymentGroup_deploymentStyle :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe DeploymentStyle)
createDeploymentGroup_deploymentStyle = Lens.lens (\CreateDeploymentGroup' {deploymentStyle} -> deploymentStyle) (\s@CreateDeploymentGroup' {} a -> s {deploymentStyle = a} :: CreateDeploymentGroup)

-- | Information to add about Amazon CloudWatch alarms when the deployment
-- group is created.
createDeploymentGroup_alarmConfiguration :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe AlarmConfiguration)
createDeploymentGroup_alarmConfiguration = Lens.lens (\CreateDeploymentGroup' {alarmConfiguration} -> alarmConfiguration) (\s@CreateDeploymentGroup' {} a -> s {alarmConfiguration = a} :: CreateDeploymentGroup)

-- | The target Amazon ECS services in the deployment group. This applies
-- only to deployment groups that use the Amazon ECS compute platform. A
-- target Amazon ECS service is specified as an Amazon ECS cluster and
-- service name pair using the format @\<clustername>:\<servicename>@.
createDeploymentGroup_ecsServices :: Lens.Lens' CreateDeploymentGroup (Prelude.Maybe [ECSService])
createDeploymentGroup_ecsServices = Lens.lens (\CreateDeploymentGroup' {ecsServices} -> ecsServices) (\s@CreateDeploymentGroup' {} a -> s {ecsServices = a} :: CreateDeploymentGroup) Prelude.. Lens.mapping Prelude._Coerce

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

instance Prelude.AWSRequest CreateDeploymentGroup where
  type
    Rs CreateDeploymentGroup =
      CreateDeploymentGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentGroupResponse'
            Prelude.<$> (x Prelude..?> "deploymentGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeploymentGroup

instance Prelude.NFData CreateDeploymentGroup

instance Prelude.ToHeaders CreateDeploymentGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.CreateDeploymentGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateDeploymentGroup where
  toJSON CreateDeploymentGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("onPremisesTagSet" Prelude..=)
              Prelude.<$> onPremisesTagSet,
            ("deploymentConfigName" Prelude..=)
              Prelude.<$> deploymentConfigName,
            ("autoRollbackConfiguration" Prelude..=)
              Prelude.<$> autoRollbackConfiguration,
            ("triggerConfigurations" Prelude..=)
              Prelude.<$> triggerConfigurations,
            ("ec2TagFilters" Prelude..=)
              Prelude.<$> ec2TagFilters,
            ("onPremisesInstanceTagFilters" Prelude..=)
              Prelude.<$> onPremisesInstanceTagFilters,
            ("loadBalancerInfo" Prelude..=)
              Prelude.<$> loadBalancerInfo,
            ("ec2TagSet" Prelude..=) Prelude.<$> ec2TagSet,
            ("blueGreenDeploymentConfiguration" Prelude..=)
              Prelude.<$> blueGreenDeploymentConfiguration,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("autoScalingGroups" Prelude..=)
              Prelude.<$> autoScalingGroups,
            ("deploymentStyle" Prelude..=)
              Prelude.<$> deploymentStyle,
            ("alarmConfiguration" Prelude..=)
              Prelude.<$> alarmConfiguration,
            ("ecsServices" Prelude..=) Prelude.<$> ecsServices,
            Prelude.Just
              ("applicationName" Prelude..= applicationName),
            Prelude.Just
              ( "deploymentGroupName"
                  Prelude..= deploymentGroupName
              ),
            Prelude.Just
              ("serviceRoleArn" Prelude..= serviceRoleArn)
          ]
      )

instance Prelude.ToPath CreateDeploymentGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDeploymentGroup where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
