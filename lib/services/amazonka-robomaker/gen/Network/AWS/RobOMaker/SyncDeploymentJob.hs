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
-- Module      : Amazonka.RobOMaker.SyncDeploymentJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Syncrhonizes robots in a fleet to the latest deployment. This is helpful
-- if robots were added after a deployment.
module Amazonka.RobOMaker.SyncDeploymentJob
  ( -- * Creating a Request
    SyncDeploymentJob (..),
    newSyncDeploymentJob,

    -- * Request Lenses
    syncDeploymentJob_clientRequestToken,
    syncDeploymentJob_fleet,

    -- * Destructuring the Response
    SyncDeploymentJobResponse (..),
    newSyncDeploymentJobResponse,

    -- * Response Lenses
    syncDeploymentJobResponse_failureReason,
    syncDeploymentJobResponse_status,
    syncDeploymentJobResponse_deploymentApplicationConfigs,
    syncDeploymentJobResponse_arn,
    syncDeploymentJobResponse_createdAt,
    syncDeploymentJobResponse_failureCode,
    syncDeploymentJobResponse_deploymentConfig,
    syncDeploymentJobResponse_fleet,
    syncDeploymentJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newSyncDeploymentJob' smart constructor.
data SyncDeploymentJob = SyncDeploymentJob'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Text,
    -- | The target fleet for the synchronization.
    fleet :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncDeploymentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'syncDeploymentJob_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'fleet', 'syncDeploymentJob_fleet' - The target fleet for the synchronization.
newSyncDeploymentJob ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'fleet'
  Prelude.Text ->
  SyncDeploymentJob
newSyncDeploymentJob pClientRequestToken_ pFleet_ =
  SyncDeploymentJob'
    { clientRequestToken =
        pClientRequestToken_,
      fleet = pFleet_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
syncDeploymentJob_clientRequestToken :: Lens.Lens' SyncDeploymentJob Prelude.Text
syncDeploymentJob_clientRequestToken = Lens.lens (\SyncDeploymentJob' {clientRequestToken} -> clientRequestToken) (\s@SyncDeploymentJob' {} a -> s {clientRequestToken = a} :: SyncDeploymentJob)

-- | The target fleet for the synchronization.
syncDeploymentJob_fleet :: Lens.Lens' SyncDeploymentJob Prelude.Text
syncDeploymentJob_fleet = Lens.lens (\SyncDeploymentJob' {fleet} -> fleet) (\s@SyncDeploymentJob' {} a -> s {fleet = a} :: SyncDeploymentJob)

instance Core.AWSRequest SyncDeploymentJob where
  type
    AWSResponse SyncDeploymentJob =
      SyncDeploymentJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SyncDeploymentJobResponse'
            Prelude.<$> (x Core..?> "failureReason")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "deploymentApplicationConfigs")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "failureCode")
            Prelude.<*> (x Core..?> "deploymentConfig")
            Prelude.<*> (x Core..?> "fleet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SyncDeploymentJob

instance Prelude.NFData SyncDeploymentJob

instance Core.ToHeaders SyncDeploymentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SyncDeploymentJob where
  toJSON SyncDeploymentJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("clientRequestToken" Core..= clientRequestToken),
            Prelude.Just ("fleet" Core..= fleet)
          ]
      )

instance Core.ToPath SyncDeploymentJob where
  toPath = Prelude.const "/syncDeploymentJob"

instance Core.ToQuery SyncDeploymentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSyncDeploymentJobResponse' smart constructor.
data SyncDeploymentJobResponse = SyncDeploymentJobResponse'
  { -- | The failure reason if the job fails.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the synchronization job.
    status :: Prelude.Maybe DeploymentStatus,
    -- | Information about the deployment application configurations.
    deploymentApplicationConfigs :: Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig),
    -- | The Amazon Resource Name (ARN) of the synchronization request.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the fleet was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The failure code if the job fails:
    --
    -- [InternalServiceError]
    --     Internal service error.
    --
    -- [RobotApplicationCrash]
    --     Robot application exited abnormally.
    --
    -- [SimulationApplicationCrash]
    --     Simulation application exited abnormally.
    --
    -- [BadPermissionsRobotApplication]
    --     Robot application bundle could not be downloaded.
    --
    -- [BadPermissionsSimulationApplication]
    --     Simulation application bundle could not be downloaded.
    --
    -- [BadPermissionsS3Output]
    --     Unable to publish outputs to customer-provided S3 bucket.
    --
    -- [BadPermissionsCloudwatchLogs]
    --     Unable to publish logs to customer-provided CloudWatch Logs
    --     resource.
    --
    -- [SubnetIpLimitExceeded]
    --     Subnet IP limit exceeded.
    --
    -- [ENILimitExceeded]
    --     ENI limit exceeded.
    --
    -- [BadPermissionsUserCredentials]
    --     Unable to use the Role provided.
    --
    -- [InvalidBundleRobotApplication]
    --     Robot bundle cannot be extracted (invalid format, bundling error, or
    --     other issue).
    --
    -- [InvalidBundleSimulationApplication]
    --     Simulation bundle cannot be extracted (invalid format, bundling
    --     error, or other issue).
    --
    -- [RobotApplicationVersionMismatchedEtag]
    --     Etag for RobotApplication does not match value during version
    --     creation.
    --
    -- [SimulationApplicationVersionMismatchedEtag]
    --     Etag for SimulationApplication does not match value during version
    --     creation.
    failureCode :: Prelude.Maybe DeploymentJobErrorCode,
    -- | Information about the deployment configuration.
    deploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | The Amazon Resource Name (ARN) of the fleet.
    fleet :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncDeploymentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'syncDeploymentJobResponse_failureReason' - The failure reason if the job fails.
--
-- 'status', 'syncDeploymentJobResponse_status' - The status of the synchronization job.
--
-- 'deploymentApplicationConfigs', 'syncDeploymentJobResponse_deploymentApplicationConfigs' - Information about the deployment application configurations.
--
-- 'arn', 'syncDeploymentJobResponse_arn' - The Amazon Resource Name (ARN) of the synchronization request.
--
-- 'createdAt', 'syncDeploymentJobResponse_createdAt' - The time, in milliseconds since the epoch, when the fleet was created.
--
-- 'failureCode', 'syncDeploymentJobResponse_failureCode' - The failure code if the job fails:
--
-- [InternalServiceError]
--     Internal service error.
--
-- [RobotApplicationCrash]
--     Robot application exited abnormally.
--
-- [SimulationApplicationCrash]
--     Simulation application exited abnormally.
--
-- [BadPermissionsRobotApplication]
--     Robot application bundle could not be downloaded.
--
-- [BadPermissionsSimulationApplication]
--     Simulation application bundle could not be downloaded.
--
-- [BadPermissionsS3Output]
--     Unable to publish outputs to customer-provided S3 bucket.
--
-- [BadPermissionsCloudwatchLogs]
--     Unable to publish logs to customer-provided CloudWatch Logs
--     resource.
--
-- [SubnetIpLimitExceeded]
--     Subnet IP limit exceeded.
--
-- [ENILimitExceeded]
--     ENI limit exceeded.
--
-- [BadPermissionsUserCredentials]
--     Unable to use the Role provided.
--
-- [InvalidBundleRobotApplication]
--     Robot bundle cannot be extracted (invalid format, bundling error, or
--     other issue).
--
-- [InvalidBundleSimulationApplication]
--     Simulation bundle cannot be extracted (invalid format, bundling
--     error, or other issue).
--
-- [RobotApplicationVersionMismatchedEtag]
--     Etag for RobotApplication does not match value during version
--     creation.
--
-- [SimulationApplicationVersionMismatchedEtag]
--     Etag for SimulationApplication does not match value during version
--     creation.
--
-- 'deploymentConfig', 'syncDeploymentJobResponse_deploymentConfig' - Information about the deployment configuration.
--
-- 'fleet', 'syncDeploymentJobResponse_fleet' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'httpStatus', 'syncDeploymentJobResponse_httpStatus' - The response's http status code.
newSyncDeploymentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SyncDeploymentJobResponse
newSyncDeploymentJobResponse pHttpStatus_ =
  SyncDeploymentJobResponse'
    { failureReason =
        Prelude.Nothing,
      status = Prelude.Nothing,
      deploymentApplicationConfigs = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      deploymentConfig = Prelude.Nothing,
      fleet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The failure reason if the job fails.
syncDeploymentJobResponse_failureReason :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe Prelude.Text)
syncDeploymentJobResponse_failureReason = Lens.lens (\SyncDeploymentJobResponse' {failureReason} -> failureReason) (\s@SyncDeploymentJobResponse' {} a -> s {failureReason = a} :: SyncDeploymentJobResponse)

-- | The status of the synchronization job.
syncDeploymentJobResponse_status :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe DeploymentStatus)
syncDeploymentJobResponse_status = Lens.lens (\SyncDeploymentJobResponse' {status} -> status) (\s@SyncDeploymentJobResponse' {} a -> s {status = a} :: SyncDeploymentJobResponse)

-- | Information about the deployment application configurations.
syncDeploymentJobResponse_deploymentApplicationConfigs :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig))
syncDeploymentJobResponse_deploymentApplicationConfigs = Lens.lens (\SyncDeploymentJobResponse' {deploymentApplicationConfigs} -> deploymentApplicationConfigs) (\s@SyncDeploymentJobResponse' {} a -> s {deploymentApplicationConfigs = a} :: SyncDeploymentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the synchronization request.
syncDeploymentJobResponse_arn :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe Prelude.Text)
syncDeploymentJobResponse_arn = Lens.lens (\SyncDeploymentJobResponse' {arn} -> arn) (\s@SyncDeploymentJobResponse' {} a -> s {arn = a} :: SyncDeploymentJobResponse)

-- | The time, in milliseconds since the epoch, when the fleet was created.
syncDeploymentJobResponse_createdAt :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe Prelude.UTCTime)
syncDeploymentJobResponse_createdAt = Lens.lens (\SyncDeploymentJobResponse' {createdAt} -> createdAt) (\s@SyncDeploymentJobResponse' {} a -> s {createdAt = a} :: SyncDeploymentJobResponse) Prelude.. Lens.mapping Core._Time

-- | The failure code if the job fails:
--
-- [InternalServiceError]
--     Internal service error.
--
-- [RobotApplicationCrash]
--     Robot application exited abnormally.
--
-- [SimulationApplicationCrash]
--     Simulation application exited abnormally.
--
-- [BadPermissionsRobotApplication]
--     Robot application bundle could not be downloaded.
--
-- [BadPermissionsSimulationApplication]
--     Simulation application bundle could not be downloaded.
--
-- [BadPermissionsS3Output]
--     Unable to publish outputs to customer-provided S3 bucket.
--
-- [BadPermissionsCloudwatchLogs]
--     Unable to publish logs to customer-provided CloudWatch Logs
--     resource.
--
-- [SubnetIpLimitExceeded]
--     Subnet IP limit exceeded.
--
-- [ENILimitExceeded]
--     ENI limit exceeded.
--
-- [BadPermissionsUserCredentials]
--     Unable to use the Role provided.
--
-- [InvalidBundleRobotApplication]
--     Robot bundle cannot be extracted (invalid format, bundling error, or
--     other issue).
--
-- [InvalidBundleSimulationApplication]
--     Simulation bundle cannot be extracted (invalid format, bundling
--     error, or other issue).
--
-- [RobotApplicationVersionMismatchedEtag]
--     Etag for RobotApplication does not match value during version
--     creation.
--
-- [SimulationApplicationVersionMismatchedEtag]
--     Etag for SimulationApplication does not match value during version
--     creation.
syncDeploymentJobResponse_failureCode :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe DeploymentJobErrorCode)
syncDeploymentJobResponse_failureCode = Lens.lens (\SyncDeploymentJobResponse' {failureCode} -> failureCode) (\s@SyncDeploymentJobResponse' {} a -> s {failureCode = a} :: SyncDeploymentJobResponse)

-- | Information about the deployment configuration.
syncDeploymentJobResponse_deploymentConfig :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe DeploymentConfig)
syncDeploymentJobResponse_deploymentConfig = Lens.lens (\SyncDeploymentJobResponse' {deploymentConfig} -> deploymentConfig) (\s@SyncDeploymentJobResponse' {} a -> s {deploymentConfig = a} :: SyncDeploymentJobResponse)

-- | The Amazon Resource Name (ARN) of the fleet.
syncDeploymentJobResponse_fleet :: Lens.Lens' SyncDeploymentJobResponse (Prelude.Maybe Prelude.Text)
syncDeploymentJobResponse_fleet = Lens.lens (\SyncDeploymentJobResponse' {fleet} -> fleet) (\s@SyncDeploymentJobResponse' {} a -> s {fleet = a} :: SyncDeploymentJobResponse)

-- | The response's http status code.
syncDeploymentJobResponse_httpStatus :: Lens.Lens' SyncDeploymentJobResponse Prelude.Int
syncDeploymentJobResponse_httpStatus = Lens.lens (\SyncDeploymentJobResponse' {httpStatus} -> httpStatus) (\s@SyncDeploymentJobResponse' {} a -> s {httpStatus = a} :: SyncDeploymentJobResponse)

instance Prelude.NFData SyncDeploymentJobResponse
