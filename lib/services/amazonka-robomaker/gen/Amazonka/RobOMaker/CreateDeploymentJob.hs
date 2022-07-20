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
-- Module      : Amazonka.RobOMaker.CreateDeploymentJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys a specific version of a robot application to robots in a fleet.
--
-- The robot application must have a numbered @applicationVersion@ for
-- consistency reasons. To create a new version, use
-- @CreateRobotApplicationVersion@ or see
-- <https://docs.aws.amazon.com/robomaker/latest/dg/create-robot-application-version.html Creating a Robot Application Version>.
--
-- After 90 days, deployment jobs expire and will be deleted. They will no
-- longer be accessible.
module Amazonka.RobOMaker.CreateDeploymentJob
  ( -- * Creating a Request
    CreateDeploymentJob (..),
    newCreateDeploymentJob,

    -- * Request Lenses
    createDeploymentJob_tags,
    createDeploymentJob_deploymentConfig,
    createDeploymentJob_clientRequestToken,
    createDeploymentJob_fleet,
    createDeploymentJob_deploymentApplicationConfigs,

    -- * Destructuring the Response
    CreateDeploymentJobResponse (..),
    newCreateDeploymentJobResponse,

    -- * Response Lenses
    createDeploymentJobResponse_tags,
    createDeploymentJobResponse_deploymentApplicationConfigs,
    createDeploymentJobResponse_failureCode,
    createDeploymentJobResponse_fleet,
    createDeploymentJobResponse_arn,
    createDeploymentJobResponse_status,
    createDeploymentJobResponse_deploymentConfig,
    createDeploymentJobResponse_createdAt,
    createDeploymentJobResponse_failureReason,
    createDeploymentJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateDeploymentJob' smart constructor.
data CreateDeploymentJob = CreateDeploymentJob'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- deployment job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The requested deployment configuration.
    deploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet to deploy.
    fleet :: Prelude.Text,
    -- | The deployment application configuration.
    deploymentApplicationConfigs :: Prelude.NonEmpty DeploymentApplicationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDeploymentJob_tags' - A map that contains tag keys and tag values that are attached to the
-- deployment job.
--
-- 'deploymentConfig', 'createDeploymentJob_deploymentConfig' - The requested deployment configuration.
--
-- 'clientRequestToken', 'createDeploymentJob_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'fleet', 'createDeploymentJob_fleet' - The Amazon Resource Name (ARN) of the fleet to deploy.
--
-- 'deploymentApplicationConfigs', 'createDeploymentJob_deploymentApplicationConfigs' - The deployment application configuration.
newCreateDeploymentJob ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'fleet'
  Prelude.Text ->
  -- | 'deploymentApplicationConfigs'
  Prelude.NonEmpty DeploymentApplicationConfig ->
  CreateDeploymentJob
newCreateDeploymentJob
  pClientRequestToken_
  pFleet_
  pDeploymentApplicationConfigs_ =
    CreateDeploymentJob'
      { tags = Prelude.Nothing,
        deploymentConfig = Prelude.Nothing,
        clientRequestToken = pClientRequestToken_,
        fleet = pFleet_,
        deploymentApplicationConfigs =
          Lens.coerced Lens.# pDeploymentApplicationConfigs_
      }

-- | A map that contains tag keys and tag values that are attached to the
-- deployment job.
createDeploymentJob_tags :: Lens.Lens' CreateDeploymentJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeploymentJob_tags = Lens.lens (\CreateDeploymentJob' {tags} -> tags) (\s@CreateDeploymentJob' {} a -> s {tags = a} :: CreateDeploymentJob) Prelude.. Lens.mapping Lens.coerced

-- | The requested deployment configuration.
createDeploymentJob_deploymentConfig :: Lens.Lens' CreateDeploymentJob (Prelude.Maybe DeploymentConfig)
createDeploymentJob_deploymentConfig = Lens.lens (\CreateDeploymentJob' {deploymentConfig} -> deploymentConfig) (\s@CreateDeploymentJob' {} a -> s {deploymentConfig = a} :: CreateDeploymentJob)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createDeploymentJob_clientRequestToken :: Lens.Lens' CreateDeploymentJob Prelude.Text
createDeploymentJob_clientRequestToken = Lens.lens (\CreateDeploymentJob' {clientRequestToken} -> clientRequestToken) (\s@CreateDeploymentJob' {} a -> s {clientRequestToken = a} :: CreateDeploymentJob)

-- | The Amazon Resource Name (ARN) of the fleet to deploy.
createDeploymentJob_fleet :: Lens.Lens' CreateDeploymentJob Prelude.Text
createDeploymentJob_fleet = Lens.lens (\CreateDeploymentJob' {fleet} -> fleet) (\s@CreateDeploymentJob' {} a -> s {fleet = a} :: CreateDeploymentJob)

-- | The deployment application configuration.
createDeploymentJob_deploymentApplicationConfigs :: Lens.Lens' CreateDeploymentJob (Prelude.NonEmpty DeploymentApplicationConfig)
createDeploymentJob_deploymentApplicationConfigs = Lens.lens (\CreateDeploymentJob' {deploymentApplicationConfigs} -> deploymentApplicationConfigs) (\s@CreateDeploymentJob' {} a -> s {deploymentApplicationConfigs = a} :: CreateDeploymentJob) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDeploymentJob where
  type
    AWSResponse CreateDeploymentJob =
      CreateDeploymentJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentJobResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "deploymentApplicationConfigs")
            Prelude.<*> (x Core..?> "failureCode")
            Prelude.<*> (x Core..?> "fleet")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "deploymentConfig")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "failureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeploymentJob where
  hashWithSalt _salt CreateDeploymentJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` deploymentConfig
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fleet
      `Prelude.hashWithSalt` deploymentApplicationConfigs

instance Prelude.NFData CreateDeploymentJob where
  rnf CreateDeploymentJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deploymentConfig
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf deploymentApplicationConfigs

instance Core.ToHeaders CreateDeploymentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDeploymentJob where
  toJSON CreateDeploymentJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("deploymentConfig" Core..=)
              Prelude.<$> deploymentConfig,
            Prelude.Just
              ("clientRequestToken" Core..= clientRequestToken),
            Prelude.Just ("fleet" Core..= fleet),
            Prelude.Just
              ( "deploymentApplicationConfigs"
                  Core..= deploymentApplicationConfigs
              )
          ]
      )

instance Core.ToPath CreateDeploymentJob where
  toPath = Prelude.const "/createDeploymentJob"

instance Core.ToQuery CreateDeploymentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeploymentJobResponse' smart constructor.
data CreateDeploymentJobResponse = CreateDeploymentJobResponse'
  { -- | The list of all tags added to the deployment job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The deployment application configuration.
    deploymentApplicationConfigs :: Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig),
    -- | The failure code of the simulation job if it failed:
    --
    -- [BadPermissionError]
    --     AWS Greengrass requires a service-level role permission to access
    --     other services. The role must include the
    --     <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSGreengrassResourceAccessRolePolicy$jsonEditor AWSGreengrassResourceAccessRolePolicy managed policy>.
    --
    -- [ExtractingBundleFailure]
    --     The robot application could not be extracted from the bundle.
    --
    -- [FailureThresholdBreached]
    --     The percentage of robots that could not be updated exceeded the
    --     percentage set for the deployment.
    --
    -- [GreengrassDeploymentFailed]
    --     The robot application could not be deployed to the robot.
    --
    -- [GreengrassGroupVersionDoesNotExist]
    --     The AWS Greengrass group or version associated with a robot is
    --     missing.
    --
    -- [InternalServerError]
    --     An internal error has occurred. Retry your request, but if the
    --     problem persists, contact us with details.
    --
    -- [MissingRobotApplicationArchitecture]
    --     The robot application does not have a source that matches the
    --     architecture of the robot.
    --
    -- [MissingRobotDeploymentResource]
    --     One or more of the resources specified for the robot application are
    --     missing. For example, does the robot application have the correct
    --     launch package and launch file?
    --
    -- [PostLaunchFileFailure]
    --     The post-launch script failed.
    --
    -- [PreLaunchFileFailure]
    --     The pre-launch script failed.
    --
    -- [ResourceNotFound]
    --     One or more deployment resources are missing. For example, do robot
    --     application source bundles still exist?
    --
    -- [RobotDeploymentNoResponse]
    --     There is no response from the robot. It might not be powered on or
    --     connected to the internet.
    failureCode :: Prelude.Maybe DeploymentJobErrorCode,
    -- | The target fleet for the deployment job.
    fleet :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deployment job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the deployment job.
    status :: Prelude.Maybe DeploymentStatus,
    -- | The deployment configuration.
    deploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | The time, in milliseconds since the epoch, when the fleet was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The failure reason of the deployment job if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDeploymentJobResponse_tags' - The list of all tags added to the deployment job.
--
-- 'deploymentApplicationConfigs', 'createDeploymentJobResponse_deploymentApplicationConfigs' - The deployment application configuration.
--
-- 'failureCode', 'createDeploymentJobResponse_failureCode' - The failure code of the simulation job if it failed:
--
-- [BadPermissionError]
--     AWS Greengrass requires a service-level role permission to access
--     other services. The role must include the
--     <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSGreengrassResourceAccessRolePolicy$jsonEditor AWSGreengrassResourceAccessRolePolicy managed policy>.
--
-- [ExtractingBundleFailure]
--     The robot application could not be extracted from the bundle.
--
-- [FailureThresholdBreached]
--     The percentage of robots that could not be updated exceeded the
--     percentage set for the deployment.
--
-- [GreengrassDeploymentFailed]
--     The robot application could not be deployed to the robot.
--
-- [GreengrassGroupVersionDoesNotExist]
--     The AWS Greengrass group or version associated with a robot is
--     missing.
--
-- [InternalServerError]
--     An internal error has occurred. Retry your request, but if the
--     problem persists, contact us with details.
--
-- [MissingRobotApplicationArchitecture]
--     The robot application does not have a source that matches the
--     architecture of the robot.
--
-- [MissingRobotDeploymentResource]
--     One or more of the resources specified for the robot application are
--     missing. For example, does the robot application have the correct
--     launch package and launch file?
--
-- [PostLaunchFileFailure]
--     The post-launch script failed.
--
-- [PreLaunchFileFailure]
--     The pre-launch script failed.
--
-- [ResourceNotFound]
--     One or more deployment resources are missing. For example, do robot
--     application source bundles still exist?
--
-- [RobotDeploymentNoResponse]
--     There is no response from the robot. It might not be powered on or
--     connected to the internet.
--
-- 'fleet', 'createDeploymentJobResponse_fleet' - The target fleet for the deployment job.
--
-- 'arn', 'createDeploymentJobResponse_arn' - The Amazon Resource Name (ARN) of the deployment job.
--
-- 'status', 'createDeploymentJobResponse_status' - The status of the deployment job.
--
-- 'deploymentConfig', 'createDeploymentJobResponse_deploymentConfig' - The deployment configuration.
--
-- 'createdAt', 'createDeploymentJobResponse_createdAt' - The time, in milliseconds since the epoch, when the fleet was created.
--
-- 'failureReason', 'createDeploymentJobResponse_failureReason' - The failure reason of the deployment job if it failed.
--
-- 'httpStatus', 'createDeploymentJobResponse_httpStatus' - The response's http status code.
newCreateDeploymentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeploymentJobResponse
newCreateDeploymentJobResponse pHttpStatus_ =
  CreateDeploymentJobResponse'
    { tags =
        Prelude.Nothing,
      deploymentApplicationConfigs = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      fleet = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      deploymentConfig = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of all tags added to the deployment job.
createDeploymentJobResponse_tags :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeploymentJobResponse_tags = Lens.lens (\CreateDeploymentJobResponse' {tags} -> tags) (\s@CreateDeploymentJobResponse' {} a -> s {tags = a} :: CreateDeploymentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The deployment application configuration.
createDeploymentJobResponse_deploymentApplicationConfigs :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig))
createDeploymentJobResponse_deploymentApplicationConfigs = Lens.lens (\CreateDeploymentJobResponse' {deploymentApplicationConfigs} -> deploymentApplicationConfigs) (\s@CreateDeploymentJobResponse' {} a -> s {deploymentApplicationConfigs = a} :: CreateDeploymentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The failure code of the simulation job if it failed:
--
-- [BadPermissionError]
--     AWS Greengrass requires a service-level role permission to access
--     other services. The role must include the
--     <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSGreengrassResourceAccessRolePolicy$jsonEditor AWSGreengrassResourceAccessRolePolicy managed policy>.
--
-- [ExtractingBundleFailure]
--     The robot application could not be extracted from the bundle.
--
-- [FailureThresholdBreached]
--     The percentage of robots that could not be updated exceeded the
--     percentage set for the deployment.
--
-- [GreengrassDeploymentFailed]
--     The robot application could not be deployed to the robot.
--
-- [GreengrassGroupVersionDoesNotExist]
--     The AWS Greengrass group or version associated with a robot is
--     missing.
--
-- [InternalServerError]
--     An internal error has occurred. Retry your request, but if the
--     problem persists, contact us with details.
--
-- [MissingRobotApplicationArchitecture]
--     The robot application does not have a source that matches the
--     architecture of the robot.
--
-- [MissingRobotDeploymentResource]
--     One or more of the resources specified for the robot application are
--     missing. For example, does the robot application have the correct
--     launch package and launch file?
--
-- [PostLaunchFileFailure]
--     The post-launch script failed.
--
-- [PreLaunchFileFailure]
--     The pre-launch script failed.
--
-- [ResourceNotFound]
--     One or more deployment resources are missing. For example, do robot
--     application source bundles still exist?
--
-- [RobotDeploymentNoResponse]
--     There is no response from the robot. It might not be powered on or
--     connected to the internet.
createDeploymentJobResponse_failureCode :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe DeploymentJobErrorCode)
createDeploymentJobResponse_failureCode = Lens.lens (\CreateDeploymentJobResponse' {failureCode} -> failureCode) (\s@CreateDeploymentJobResponse' {} a -> s {failureCode = a} :: CreateDeploymentJobResponse)

-- | The target fleet for the deployment job.
createDeploymentJobResponse_fleet :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe Prelude.Text)
createDeploymentJobResponse_fleet = Lens.lens (\CreateDeploymentJobResponse' {fleet} -> fleet) (\s@CreateDeploymentJobResponse' {} a -> s {fleet = a} :: CreateDeploymentJobResponse)

-- | The Amazon Resource Name (ARN) of the deployment job.
createDeploymentJobResponse_arn :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe Prelude.Text)
createDeploymentJobResponse_arn = Lens.lens (\CreateDeploymentJobResponse' {arn} -> arn) (\s@CreateDeploymentJobResponse' {} a -> s {arn = a} :: CreateDeploymentJobResponse)

-- | The status of the deployment job.
createDeploymentJobResponse_status :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe DeploymentStatus)
createDeploymentJobResponse_status = Lens.lens (\CreateDeploymentJobResponse' {status} -> status) (\s@CreateDeploymentJobResponse' {} a -> s {status = a} :: CreateDeploymentJobResponse)

-- | The deployment configuration.
createDeploymentJobResponse_deploymentConfig :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe DeploymentConfig)
createDeploymentJobResponse_deploymentConfig = Lens.lens (\CreateDeploymentJobResponse' {deploymentConfig} -> deploymentConfig) (\s@CreateDeploymentJobResponse' {} a -> s {deploymentConfig = a} :: CreateDeploymentJobResponse)

-- | The time, in milliseconds since the epoch, when the fleet was created.
createDeploymentJobResponse_createdAt :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe Prelude.UTCTime)
createDeploymentJobResponse_createdAt = Lens.lens (\CreateDeploymentJobResponse' {createdAt} -> createdAt) (\s@CreateDeploymentJobResponse' {} a -> s {createdAt = a} :: CreateDeploymentJobResponse) Prelude.. Lens.mapping Core._Time

-- | The failure reason of the deployment job if it failed.
createDeploymentJobResponse_failureReason :: Lens.Lens' CreateDeploymentJobResponse (Prelude.Maybe Prelude.Text)
createDeploymentJobResponse_failureReason = Lens.lens (\CreateDeploymentJobResponse' {failureReason} -> failureReason) (\s@CreateDeploymentJobResponse' {} a -> s {failureReason = a} :: CreateDeploymentJobResponse)

-- | The response's http status code.
createDeploymentJobResponse_httpStatus :: Lens.Lens' CreateDeploymentJobResponse Prelude.Int
createDeploymentJobResponse_httpStatus = Lens.lens (\CreateDeploymentJobResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentJobResponse' {} a -> s {httpStatus = a} :: CreateDeploymentJobResponse)

instance Prelude.NFData CreateDeploymentJobResponse where
  rnf CreateDeploymentJobResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deploymentApplicationConfigs
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf deploymentConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
