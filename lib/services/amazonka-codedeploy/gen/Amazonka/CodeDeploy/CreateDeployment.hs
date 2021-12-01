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
-- Module      : Amazonka.CodeDeploy.CreateDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys an application revision through the specified deployment group.
module Amazonka.CodeDeploy.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_deploymentConfigName,
    createDeployment_fileExistsBehavior,
    createDeployment_targetInstances,
    createDeployment_revision,
    createDeployment_description,
    createDeployment_autoRollbackConfiguration,
    createDeployment_updateOutdatedInstancesOnly,
    createDeployment_deploymentGroupName,
    createDeployment_ignoreApplicationStopFailures,
    createDeployment_applicationName,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateDeployment@ operation.
--
-- /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | The name of a deployment configuration associated with the IAM user or
    -- AWS account.
    --
    -- If not specified, the value configured in the deployment group is used
    -- as the default. If the deployment group does not have a deployment
    -- configuration associated with it, @CodeDeployDefault@.@OneAtATime@ is
    -- used by default.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | Information about how AWS CodeDeploy handles files that already exist in
    -- a deployment target location but weren\'t part of the previous
    -- successful deployment.
    --
    -- The @fileExistsBehavior@ parameter takes any of the following values:
    --
    -- -   DISALLOW: The deployment fails. This is also the default behavior if
    --     no option is specified.
    --
    -- -   OVERWRITE: The version of the file from the application revision
    --     currently being deployed replaces the version already on the
    --     instance.
    --
    -- -   RETAIN: The version of the file already on the instance is kept and
    --     used as part of the new deployment.
    fileExistsBehavior :: Prelude.Maybe FileExistsBehavior,
    -- | Information about the instances that belong to the replacement
    -- environment in a blue\/green deployment.
    targetInstances :: Prelude.Maybe TargetInstances,
    -- | The type and location of the revision to deploy.
    revision :: Prelude.Maybe RevisionLocation,
    -- | A comment about the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for an automatic rollback that is added when a
    -- deployment is created.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfiguration,
    -- | Indicates whether to deploy to all instances or only to instances that
    -- are not running the latest application revision.
    updateOutdatedInstancesOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of the deployment group.
    deploymentGroupName :: Prelude.Maybe Prelude.Text,
    -- | If true, then if an @ApplicationStop@, @BeforeBlockTraffic@, or
    -- @AfterBlockTraffic@ deployment lifecycle event to an instance fails,
    -- then the deployment continues to the next deployment lifecycle event.
    -- For example, if @ApplicationStop@ fails, the deployment continues with
    -- @DownloadBundle@. If @BeforeBlockTraffic@ fails, the deployment
    -- continues with @BlockTraffic@. If @AfterBlockTraffic@ fails, the
    -- deployment continues with @ApplicationStop@.
    --
    -- If false or not specified, then if a lifecycle event fails during a
    -- deployment to an instance, that deployment fails. If deployment to that
    -- instance is part of an overall deployment and the number of healthy
    -- hosts is not less than the minimum number of healthy hosts, then a
    -- deployment to the next instance is attempted.
    --
    -- During a deployment, the AWS CodeDeploy agent runs the scripts specified
    -- for @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in
    -- the AppSpec file from the previous successful deployment. (All other
    -- scripts are run from the AppSpec file in the current deployment.) If one
    -- of these scripts contains an error and does not run successfully, the
    -- deployment can fail.
    --
    -- If the cause of the failure is a script from the last successful
    -- deployment that will never run successfully, create a new deployment and
    -- use @ignoreApplicationStopFailures@ to specify that the
    -- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
    -- failures should be ignored.
    ignoreApplicationStopFailures :: Prelude.Maybe Prelude.Bool,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfigName', 'createDeployment_deploymentConfigName' - The name of a deployment configuration associated with the IAM user or
-- AWS account.
--
-- If not specified, the value configured in the deployment group is used
-- as the default. If the deployment group does not have a deployment
-- configuration associated with it, @CodeDeployDefault@.@OneAtATime@ is
-- used by default.
--
-- 'fileExistsBehavior', 'createDeployment_fileExistsBehavior' - Information about how AWS CodeDeploy handles files that already exist in
-- a deployment target location but weren\'t part of the previous
-- successful deployment.
--
-- The @fileExistsBehavior@ parameter takes any of the following values:
--
-- -   DISALLOW: The deployment fails. This is also the default behavior if
--     no option is specified.
--
-- -   OVERWRITE: The version of the file from the application revision
--     currently being deployed replaces the version already on the
--     instance.
--
-- -   RETAIN: The version of the file already on the instance is kept and
--     used as part of the new deployment.
--
-- 'targetInstances', 'createDeployment_targetInstances' - Information about the instances that belong to the replacement
-- environment in a blue\/green deployment.
--
-- 'revision', 'createDeployment_revision' - The type and location of the revision to deploy.
--
-- 'description', 'createDeployment_description' - A comment about the deployment.
--
-- 'autoRollbackConfiguration', 'createDeployment_autoRollbackConfiguration' - Configuration information for an automatic rollback that is added when a
-- deployment is created.
--
-- 'updateOutdatedInstancesOnly', 'createDeployment_updateOutdatedInstancesOnly' - Indicates whether to deploy to all instances or only to instances that
-- are not running the latest application revision.
--
-- 'deploymentGroupName', 'createDeployment_deploymentGroupName' - The name of the deployment group.
--
-- 'ignoreApplicationStopFailures', 'createDeployment_ignoreApplicationStopFailures' - If true, then if an @ApplicationStop@, @BeforeBlockTraffic@, or
-- @AfterBlockTraffic@ deployment lifecycle event to an instance fails,
-- then the deployment continues to the next deployment lifecycle event.
-- For example, if @ApplicationStop@ fails, the deployment continues with
-- @DownloadBundle@. If @BeforeBlockTraffic@ fails, the deployment
-- continues with @BlockTraffic@. If @AfterBlockTraffic@ fails, the
-- deployment continues with @ApplicationStop@.
--
-- If false or not specified, then if a lifecycle event fails during a
-- deployment to an instance, that deployment fails. If deployment to that
-- instance is part of an overall deployment and the number of healthy
-- hosts is not less than the minimum number of healthy hosts, then a
-- deployment to the next instance is attempted.
--
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified
-- for @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in
-- the AppSpec file from the previous successful deployment. (All other
-- scripts are run from the AppSpec file in the current deployment.) If one
-- of these scripts contains an error and does not run successfully, the
-- deployment can fail.
--
-- If the cause of the failure is a script from the last successful
-- deployment that will never run successfully, create a new deployment and
-- use @ignoreApplicationStopFailures@ to specify that the
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
-- failures should be ignored.
--
-- 'applicationName', 'createDeployment_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
newCreateDeployment ::
  -- | 'applicationName'
  Prelude.Text ->
  CreateDeployment
newCreateDeployment pApplicationName_ =
  CreateDeployment'
    { deploymentConfigName =
        Prelude.Nothing,
      fileExistsBehavior = Prelude.Nothing,
      targetInstances = Prelude.Nothing,
      revision = Prelude.Nothing,
      description = Prelude.Nothing,
      autoRollbackConfiguration = Prelude.Nothing,
      updateOutdatedInstancesOnly = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing,
      ignoreApplicationStopFailures = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | The name of a deployment configuration associated with the IAM user or
-- AWS account.
--
-- If not specified, the value configured in the deployment group is used
-- as the default. If the deployment group does not have a deployment
-- configuration associated with it, @CodeDeployDefault@.@OneAtATime@ is
-- used by default.
createDeployment_deploymentConfigName :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_deploymentConfigName = Lens.lens (\CreateDeployment' {deploymentConfigName} -> deploymentConfigName) (\s@CreateDeployment' {} a -> s {deploymentConfigName = a} :: CreateDeployment)

-- | Information about how AWS CodeDeploy handles files that already exist in
-- a deployment target location but weren\'t part of the previous
-- successful deployment.
--
-- The @fileExistsBehavior@ parameter takes any of the following values:
--
-- -   DISALLOW: The deployment fails. This is also the default behavior if
--     no option is specified.
--
-- -   OVERWRITE: The version of the file from the application revision
--     currently being deployed replaces the version already on the
--     instance.
--
-- -   RETAIN: The version of the file already on the instance is kept and
--     used as part of the new deployment.
createDeployment_fileExistsBehavior :: Lens.Lens' CreateDeployment (Prelude.Maybe FileExistsBehavior)
createDeployment_fileExistsBehavior = Lens.lens (\CreateDeployment' {fileExistsBehavior} -> fileExistsBehavior) (\s@CreateDeployment' {} a -> s {fileExistsBehavior = a} :: CreateDeployment)

-- | Information about the instances that belong to the replacement
-- environment in a blue\/green deployment.
createDeployment_targetInstances :: Lens.Lens' CreateDeployment (Prelude.Maybe TargetInstances)
createDeployment_targetInstances = Lens.lens (\CreateDeployment' {targetInstances} -> targetInstances) (\s@CreateDeployment' {} a -> s {targetInstances = a} :: CreateDeployment)

-- | The type and location of the revision to deploy.
createDeployment_revision :: Lens.Lens' CreateDeployment (Prelude.Maybe RevisionLocation)
createDeployment_revision = Lens.lens (\CreateDeployment' {revision} -> revision) (\s@CreateDeployment' {} a -> s {revision = a} :: CreateDeployment)

-- | A comment about the deployment.
createDeployment_description :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_description = Lens.lens (\CreateDeployment' {description} -> description) (\s@CreateDeployment' {} a -> s {description = a} :: CreateDeployment)

-- | Configuration information for an automatic rollback that is added when a
-- deployment is created.
createDeployment_autoRollbackConfiguration :: Lens.Lens' CreateDeployment (Prelude.Maybe AutoRollbackConfiguration)
createDeployment_autoRollbackConfiguration = Lens.lens (\CreateDeployment' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@CreateDeployment' {} a -> s {autoRollbackConfiguration = a} :: CreateDeployment)

-- | Indicates whether to deploy to all instances or only to instances that
-- are not running the latest application revision.
createDeployment_updateOutdatedInstancesOnly :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Bool)
createDeployment_updateOutdatedInstancesOnly = Lens.lens (\CreateDeployment' {updateOutdatedInstancesOnly} -> updateOutdatedInstancesOnly) (\s@CreateDeployment' {} a -> s {updateOutdatedInstancesOnly = a} :: CreateDeployment)

-- | The name of the deployment group.
createDeployment_deploymentGroupName :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_deploymentGroupName = Lens.lens (\CreateDeployment' {deploymentGroupName} -> deploymentGroupName) (\s@CreateDeployment' {} a -> s {deploymentGroupName = a} :: CreateDeployment)

-- | If true, then if an @ApplicationStop@, @BeforeBlockTraffic@, or
-- @AfterBlockTraffic@ deployment lifecycle event to an instance fails,
-- then the deployment continues to the next deployment lifecycle event.
-- For example, if @ApplicationStop@ fails, the deployment continues with
-- @DownloadBundle@. If @BeforeBlockTraffic@ fails, the deployment
-- continues with @BlockTraffic@. If @AfterBlockTraffic@ fails, the
-- deployment continues with @ApplicationStop@.
--
-- If false or not specified, then if a lifecycle event fails during a
-- deployment to an instance, that deployment fails. If deployment to that
-- instance is part of an overall deployment and the number of healthy
-- hosts is not less than the minimum number of healthy hosts, then a
-- deployment to the next instance is attempted.
--
-- During a deployment, the AWS CodeDeploy agent runs the scripts specified
-- for @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@ in
-- the AppSpec file from the previous successful deployment. (All other
-- scripts are run from the AppSpec file in the current deployment.) If one
-- of these scripts contains an error and does not run successfully, the
-- deployment can fail.
--
-- If the cause of the failure is a script from the last successful
-- deployment that will never run successfully, create a new deployment and
-- use @ignoreApplicationStopFailures@ to specify that the
-- @ApplicationStop@, @BeforeBlockTraffic@, and @AfterBlockTraffic@
-- failures should be ignored.
createDeployment_ignoreApplicationStopFailures :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Bool)
createDeployment_ignoreApplicationStopFailures = Lens.lens (\CreateDeployment' {ignoreApplicationStopFailures} -> ignoreApplicationStopFailures) (\s@CreateDeployment' {} a -> s {ignoreApplicationStopFailures = a} :: CreateDeployment)

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
createDeployment_applicationName :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_applicationName = Lens.lens (\CreateDeployment' {applicationName} -> applicationName) (\s@CreateDeployment' {} a -> s {applicationName = a} :: CreateDeployment)

instance Core.AWSRequest CreateDeployment where
  type
    AWSResponse CreateDeployment =
      CreateDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Prelude.<$> (x Core..?> "deploymentId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeployment where
  hashWithSalt salt' CreateDeployment' {..} =
    salt' `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` ignoreApplicationStopFailures
      `Prelude.hashWithSalt` deploymentGroupName
      `Prelude.hashWithSalt` updateOutdatedInstancesOnly
      `Prelude.hashWithSalt` autoRollbackConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` targetInstances
      `Prelude.hashWithSalt` fileExistsBehavior
      `Prelude.hashWithSalt` deploymentConfigName

instance Prelude.NFData CreateDeployment where
  rnf CreateDeployment' {..} =
    Prelude.rnf deploymentConfigName
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf ignoreApplicationStopFailures
      `Prelude.seq` Prelude.rnf deploymentGroupName
      `Prelude.seq` Prelude.rnf updateOutdatedInstancesOnly
      `Prelude.seq` Prelude.rnf autoRollbackConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf targetInstances
      `Prelude.seq` Prelude.rnf fileExistsBehavior

instance Core.ToHeaders CreateDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.CreateDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deploymentConfigName" Core..=)
              Prelude.<$> deploymentConfigName,
            ("fileExistsBehavior" Core..=)
              Prelude.<$> fileExistsBehavior,
            ("targetInstances" Core..=)
              Prelude.<$> targetInstances,
            ("revision" Core..=) Prelude.<$> revision,
            ("description" Core..=) Prelude.<$> description,
            ("autoRollbackConfiguration" Core..=)
              Prelude.<$> autoRollbackConfiguration,
            ("updateOutdatedInstancesOnly" Core..=)
              Prelude.<$> updateOutdatedInstancesOnly,
            ("deploymentGroupName" Core..=)
              Prelude.<$> deploymentGroupName,
            ("ignoreApplicationStopFailures" Core..=)
              Prelude.<$> ignoreApplicationStopFailures,
            Prelude.Just
              ("applicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath CreateDeployment where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateDeployment@ operation.
--
-- /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'createDeploymentResponse_deploymentId' - The unique ID of a deployment.
--
-- 'httpStatus', 'createDeploymentResponse_httpStatus' - The response's http status code.
newCreateDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeploymentResponse
newCreateDeploymentResponse pHttpStatus_ =
  CreateDeploymentResponse'
    { deploymentId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of a deployment.
createDeploymentResponse_deploymentId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_deploymentId = Lens.lens (\CreateDeploymentResponse' {deploymentId} -> deploymentId) (\s@CreateDeploymentResponse' {} a -> s {deploymentId = a} :: CreateDeploymentResponse)

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Prelude.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

instance Prelude.NFData CreateDeploymentResponse where
  rnf CreateDeploymentResponse' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf httpStatus
