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
-- Module      : Amazonka.RobOMaker.DescribeSimulationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a simulation job.
module Amazonka.RobOMaker.DescribeSimulationJob
  ( -- * Creating a Request
    DescribeSimulationJob (..),
    newDescribeSimulationJob,

    -- * Request Lenses
    describeSimulationJob_job,

    -- * Destructuring the Response
    DescribeSimulationJobResponse (..),
    newDescribeSimulationJobResponse,

    -- * Response Lenses
    describeSimulationJobResponse_tags,
    describeSimulationJobResponse_lastStartedAt,
    describeSimulationJobResponse_name,
    describeSimulationJobResponse_iamRole,
    describeSimulationJobResponse_maxJobDurationInSeconds,
    describeSimulationJobResponse_failureCode,
    describeSimulationJobResponse_clientRequestToken,
    describeSimulationJobResponse_lastUpdatedAt,
    describeSimulationJobResponse_dataSources,
    describeSimulationJobResponse_vpcConfig,
    describeSimulationJobResponse_arn,
    describeSimulationJobResponse_outputLocation,
    describeSimulationJobResponse_status,
    describeSimulationJobResponse_simulationApplications,
    describeSimulationJobResponse_simulationTimeMillis,
    describeSimulationJobResponse_failureBehavior,
    describeSimulationJobResponse_robotApplications,
    describeSimulationJobResponse_networkInterface,
    describeSimulationJobResponse_loggingConfig,
    describeSimulationJobResponse_failureReason,
    describeSimulationJobResponse_compute,
    describeSimulationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeSimulationJob' smart constructor.
data DescribeSimulationJob = DescribeSimulationJob'
  { -- | The Amazon Resource Name (ARN) of the simulation job to be described.
    job :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSimulationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'describeSimulationJob_job' - The Amazon Resource Name (ARN) of the simulation job to be described.
newDescribeSimulationJob ::
  -- | 'job'
  Prelude.Text ->
  DescribeSimulationJob
newDescribeSimulationJob pJob_ =
  DescribeSimulationJob' {job = pJob_}

-- | The Amazon Resource Name (ARN) of the simulation job to be described.
describeSimulationJob_job :: Lens.Lens' DescribeSimulationJob Prelude.Text
describeSimulationJob_job = Lens.lens (\DescribeSimulationJob' {job} -> job) (\s@DescribeSimulationJob' {} a -> s {job = a} :: DescribeSimulationJob)

instance Core.AWSRequest DescribeSimulationJob where
  type
    AWSResponse DescribeSimulationJob =
      DescribeSimulationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSimulationJobResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "lastStartedAt")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "iamRole")
            Prelude.<*> (x Core..?> "maxJobDurationInSeconds")
            Prelude.<*> (x Core..?> "failureCode")
            Prelude.<*> (x Core..?> "clientRequestToken")
            Prelude.<*> (x Core..?> "lastUpdatedAt")
            Prelude.<*> (x Core..?> "dataSources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "vpcConfig")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "outputLocation")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "simulationApplications")
            Prelude.<*> (x Core..?> "simulationTimeMillis")
            Prelude.<*> (x Core..?> "failureBehavior")
            Prelude.<*> (x Core..?> "robotApplications")
            Prelude.<*> (x Core..?> "networkInterface")
            Prelude.<*> (x Core..?> "loggingConfig")
            Prelude.<*> (x Core..?> "failureReason")
            Prelude.<*> (x Core..?> "compute")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSimulationJob where
  hashWithSalt _salt DescribeSimulationJob' {..} =
    _salt `Prelude.hashWithSalt` job

instance Prelude.NFData DescribeSimulationJob where
  rnf DescribeSimulationJob' {..} = Prelude.rnf job

instance Core.ToHeaders DescribeSimulationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSimulationJob where
  toJSON DescribeSimulationJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Core..= job)]
      )

instance Core.ToPath DescribeSimulationJob where
  toPath = Prelude.const "/describeSimulationJob"

instance Core.ToQuery DescribeSimulationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSimulationJobResponse' smart constructor.
data DescribeSimulationJobResponse = DescribeSimulationJobResponse'
  { -- | The list of all tags added to the specified simulation job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time, in milliseconds since the epoch, when the simulation job was
    -- last started.
    lastStartedAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the simulation job.
    name :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that allows the simulation instance to call the AWS APIs
    -- that are specified in its associated policies on your behalf.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The maximum job duration in seconds. The value must be 8 days (691,200
    -- seconds) or less.
    maxJobDurationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The failure code of the simulation job if it failed:
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
    failureCode :: Prelude.Maybe SimulationJobErrorCode,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the simulation job was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The data sources for the simulation job.
    dataSources :: Prelude.Maybe [DataSource],
    -- | The VPC configuration.
    vpcConfig :: Prelude.Maybe VPCConfigResponse,
    -- | The Amazon Resource Name (ARN) of the simulation job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Location for output files generated by the simulation job.
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | The status of the simulation job.
    status :: Prelude.Maybe SimulationJobStatus,
    -- | A list of simulation applications.
    simulationApplications :: Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig),
    -- | The simulation job execution duration in milliseconds.
    simulationTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The failure behavior for the simulation job.
    failureBehavior :: Prelude.Maybe FailureBehavior,
    -- | A list of robot applications.
    robotApplications :: Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig),
    -- | The network interface information for the simulation job.
    networkInterface :: Prelude.Maybe NetworkInterface,
    -- | The logging configuration.
    loggingConfig :: Prelude.Maybe LoggingConfig,
    -- | Details about why the simulation job failed. For more information about
    -- troubleshooting, see
    -- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting.html Troubleshooting>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Compute information for the simulation job.
    compute :: Prelude.Maybe ComputeResponse,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSimulationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeSimulationJobResponse_tags' - The list of all tags added to the specified simulation job.
--
-- 'lastStartedAt', 'describeSimulationJobResponse_lastStartedAt' - The time, in milliseconds since the epoch, when the simulation job was
-- last started.
--
-- 'name', 'describeSimulationJobResponse_name' - The name of the simulation job.
--
-- 'iamRole', 'describeSimulationJobResponse_iamRole' - The IAM role that allows the simulation instance to call the AWS APIs
-- that are specified in its associated policies on your behalf.
--
-- 'maxJobDurationInSeconds', 'describeSimulationJobResponse_maxJobDurationInSeconds' - The maximum job duration in seconds. The value must be 8 days (691,200
-- seconds) or less.
--
-- 'failureCode', 'describeSimulationJobResponse_failureCode' - The failure code of the simulation job if it failed:
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
-- 'clientRequestToken', 'describeSimulationJobResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'lastUpdatedAt', 'describeSimulationJobResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation job was
-- last updated.
--
-- 'dataSources', 'describeSimulationJobResponse_dataSources' - The data sources for the simulation job.
--
-- 'vpcConfig', 'describeSimulationJobResponse_vpcConfig' - The VPC configuration.
--
-- 'arn', 'describeSimulationJobResponse_arn' - The Amazon Resource Name (ARN) of the simulation job.
--
-- 'outputLocation', 'describeSimulationJobResponse_outputLocation' - Location for output files generated by the simulation job.
--
-- 'status', 'describeSimulationJobResponse_status' - The status of the simulation job.
--
-- 'simulationApplications', 'describeSimulationJobResponse_simulationApplications' - A list of simulation applications.
--
-- 'simulationTimeMillis', 'describeSimulationJobResponse_simulationTimeMillis' - The simulation job execution duration in milliseconds.
--
-- 'failureBehavior', 'describeSimulationJobResponse_failureBehavior' - The failure behavior for the simulation job.
--
-- 'robotApplications', 'describeSimulationJobResponse_robotApplications' - A list of robot applications.
--
-- 'networkInterface', 'describeSimulationJobResponse_networkInterface' - The network interface information for the simulation job.
--
-- 'loggingConfig', 'describeSimulationJobResponse_loggingConfig' - The logging configuration.
--
-- 'failureReason', 'describeSimulationJobResponse_failureReason' - Details about why the simulation job failed. For more information about
-- troubleshooting, see
-- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting.html Troubleshooting>.
--
-- 'compute', 'describeSimulationJobResponse_compute' - Compute information for the simulation job.
--
-- 'httpStatus', 'describeSimulationJobResponse_httpStatus' - The response's http status code.
newDescribeSimulationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSimulationJobResponse
newDescribeSimulationJobResponse pHttpStatus_ =
  DescribeSimulationJobResponse'
    { tags =
        Prelude.Nothing,
      lastStartedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      maxJobDurationInSeconds = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      arn = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      status = Prelude.Nothing,
      simulationApplications = Prelude.Nothing,
      simulationTimeMillis = Prelude.Nothing,
      failureBehavior = Prelude.Nothing,
      robotApplications = Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      loggingConfig = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      compute = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of all tags added to the specified simulation job.
describeSimulationJobResponse_tags :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeSimulationJobResponse_tags = Lens.lens (\DescribeSimulationJobResponse' {tags} -> tags) (\s@DescribeSimulationJobResponse' {} a -> s {tags = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time, in milliseconds since the epoch, when the simulation job was
-- last started.
describeSimulationJobResponse_lastStartedAt :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.UTCTime)
describeSimulationJobResponse_lastStartedAt = Lens.lens (\DescribeSimulationJobResponse' {lastStartedAt} -> lastStartedAt) (\s@DescribeSimulationJobResponse' {} a -> s {lastStartedAt = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the simulation job.
describeSimulationJobResponse_name :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_name = Lens.lens (\DescribeSimulationJobResponse' {name} -> name) (\s@DescribeSimulationJobResponse' {} a -> s {name = a} :: DescribeSimulationJobResponse)

-- | The IAM role that allows the simulation instance to call the AWS APIs
-- that are specified in its associated policies on your behalf.
describeSimulationJobResponse_iamRole :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_iamRole = Lens.lens (\DescribeSimulationJobResponse' {iamRole} -> iamRole) (\s@DescribeSimulationJobResponse' {} a -> s {iamRole = a} :: DescribeSimulationJobResponse)

-- | The maximum job duration in seconds. The value must be 8 days (691,200
-- seconds) or less.
describeSimulationJobResponse_maxJobDurationInSeconds :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Integer)
describeSimulationJobResponse_maxJobDurationInSeconds = Lens.lens (\DescribeSimulationJobResponse' {maxJobDurationInSeconds} -> maxJobDurationInSeconds) (\s@DescribeSimulationJobResponse' {} a -> s {maxJobDurationInSeconds = a} :: DescribeSimulationJobResponse)

-- | The failure code of the simulation job if it failed:
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
describeSimulationJobResponse_failureCode :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe SimulationJobErrorCode)
describeSimulationJobResponse_failureCode = Lens.lens (\DescribeSimulationJobResponse' {failureCode} -> failureCode) (\s@DescribeSimulationJobResponse' {} a -> s {failureCode = a} :: DescribeSimulationJobResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
describeSimulationJobResponse_clientRequestToken :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_clientRequestToken = Lens.lens (\DescribeSimulationJobResponse' {clientRequestToken} -> clientRequestToken) (\s@DescribeSimulationJobResponse' {} a -> s {clientRequestToken = a} :: DescribeSimulationJobResponse)

-- | The time, in milliseconds since the epoch, when the simulation job was
-- last updated.
describeSimulationJobResponse_lastUpdatedAt :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.UTCTime)
describeSimulationJobResponse_lastUpdatedAt = Lens.lens (\DescribeSimulationJobResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeSimulationJobResponse' {} a -> s {lastUpdatedAt = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Core._Time

-- | The data sources for the simulation job.
describeSimulationJobResponse_dataSources :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe [DataSource])
describeSimulationJobResponse_dataSources = Lens.lens (\DescribeSimulationJobResponse' {dataSources} -> dataSources) (\s@DescribeSimulationJobResponse' {} a -> s {dataSources = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The VPC configuration.
describeSimulationJobResponse_vpcConfig :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe VPCConfigResponse)
describeSimulationJobResponse_vpcConfig = Lens.lens (\DescribeSimulationJobResponse' {vpcConfig} -> vpcConfig) (\s@DescribeSimulationJobResponse' {} a -> s {vpcConfig = a} :: DescribeSimulationJobResponse)

-- | The Amazon Resource Name (ARN) of the simulation job.
describeSimulationJobResponse_arn :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_arn = Lens.lens (\DescribeSimulationJobResponse' {arn} -> arn) (\s@DescribeSimulationJobResponse' {} a -> s {arn = a} :: DescribeSimulationJobResponse)

-- | Location for output files generated by the simulation job.
describeSimulationJobResponse_outputLocation :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe OutputLocation)
describeSimulationJobResponse_outputLocation = Lens.lens (\DescribeSimulationJobResponse' {outputLocation} -> outputLocation) (\s@DescribeSimulationJobResponse' {} a -> s {outputLocation = a} :: DescribeSimulationJobResponse)

-- | The status of the simulation job.
describeSimulationJobResponse_status :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe SimulationJobStatus)
describeSimulationJobResponse_status = Lens.lens (\DescribeSimulationJobResponse' {status} -> status) (\s@DescribeSimulationJobResponse' {} a -> s {status = a} :: DescribeSimulationJobResponse)

-- | A list of simulation applications.
describeSimulationJobResponse_simulationApplications :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig))
describeSimulationJobResponse_simulationApplications = Lens.lens (\DescribeSimulationJobResponse' {simulationApplications} -> simulationApplications) (\s@DescribeSimulationJobResponse' {} a -> s {simulationApplications = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The simulation job execution duration in milliseconds.
describeSimulationJobResponse_simulationTimeMillis :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Integer)
describeSimulationJobResponse_simulationTimeMillis = Lens.lens (\DescribeSimulationJobResponse' {simulationTimeMillis} -> simulationTimeMillis) (\s@DescribeSimulationJobResponse' {} a -> s {simulationTimeMillis = a} :: DescribeSimulationJobResponse)

-- | The failure behavior for the simulation job.
describeSimulationJobResponse_failureBehavior :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe FailureBehavior)
describeSimulationJobResponse_failureBehavior = Lens.lens (\DescribeSimulationJobResponse' {failureBehavior} -> failureBehavior) (\s@DescribeSimulationJobResponse' {} a -> s {failureBehavior = a} :: DescribeSimulationJobResponse)

-- | A list of robot applications.
describeSimulationJobResponse_robotApplications :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig))
describeSimulationJobResponse_robotApplications = Lens.lens (\DescribeSimulationJobResponse' {robotApplications} -> robotApplications) (\s@DescribeSimulationJobResponse' {} a -> s {robotApplications = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The network interface information for the simulation job.
describeSimulationJobResponse_networkInterface :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe NetworkInterface)
describeSimulationJobResponse_networkInterface = Lens.lens (\DescribeSimulationJobResponse' {networkInterface} -> networkInterface) (\s@DescribeSimulationJobResponse' {} a -> s {networkInterface = a} :: DescribeSimulationJobResponse)

-- | The logging configuration.
describeSimulationJobResponse_loggingConfig :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe LoggingConfig)
describeSimulationJobResponse_loggingConfig = Lens.lens (\DescribeSimulationJobResponse' {loggingConfig} -> loggingConfig) (\s@DescribeSimulationJobResponse' {} a -> s {loggingConfig = a} :: DescribeSimulationJobResponse)

-- | Details about why the simulation job failed. For more information about
-- troubleshooting, see
-- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting.html Troubleshooting>.
describeSimulationJobResponse_failureReason :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_failureReason = Lens.lens (\DescribeSimulationJobResponse' {failureReason} -> failureReason) (\s@DescribeSimulationJobResponse' {} a -> s {failureReason = a} :: DescribeSimulationJobResponse)

-- | Compute information for the simulation job.
describeSimulationJobResponse_compute :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe ComputeResponse)
describeSimulationJobResponse_compute = Lens.lens (\DescribeSimulationJobResponse' {compute} -> compute) (\s@DescribeSimulationJobResponse' {} a -> s {compute = a} :: DescribeSimulationJobResponse)

-- | The response's http status code.
describeSimulationJobResponse_httpStatus :: Lens.Lens' DescribeSimulationJobResponse Prelude.Int
describeSimulationJobResponse_httpStatus = Lens.lens (\DescribeSimulationJobResponse' {httpStatus} -> httpStatus) (\s@DescribeSimulationJobResponse' {} a -> s {httpStatus = a} :: DescribeSimulationJobResponse)

instance Prelude.NFData DescribeSimulationJobResponse where
  rnf DescribeSimulationJobResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastStartedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf maxJobDurationInSeconds
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf simulationApplications
      `Prelude.seq` Prelude.rnf simulationTimeMillis
      `Prelude.seq` Prelude.rnf failureBehavior
      `Prelude.seq` Prelude.rnf robotApplications
      `Prelude.seq` Prelude.rnf networkInterface
      `Prelude.seq` Prelude.rnf loggingConfig
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf compute
      `Prelude.seq` Prelude.rnf httpStatus
