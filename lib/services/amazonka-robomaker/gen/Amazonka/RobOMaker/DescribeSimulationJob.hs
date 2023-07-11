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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeSimulationJobResponse_arn,
    describeSimulationJobResponse_clientRequestToken,
    describeSimulationJobResponse_compute,
    describeSimulationJobResponse_dataSources,
    describeSimulationJobResponse_failureBehavior,
    describeSimulationJobResponse_failureCode,
    describeSimulationJobResponse_failureReason,
    describeSimulationJobResponse_iamRole,
    describeSimulationJobResponse_lastStartedAt,
    describeSimulationJobResponse_lastUpdatedAt,
    describeSimulationJobResponse_loggingConfig,
    describeSimulationJobResponse_maxJobDurationInSeconds,
    describeSimulationJobResponse_name,
    describeSimulationJobResponse_networkInterface,
    describeSimulationJobResponse_outputLocation,
    describeSimulationJobResponse_robotApplications,
    describeSimulationJobResponse_simulationApplications,
    describeSimulationJobResponse_simulationTimeMillis,
    describeSimulationJobResponse_status,
    describeSimulationJobResponse_tags,
    describeSimulationJobResponse_vpcConfig,
    describeSimulationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "clientRequestToken")
            Prelude.<*> (x Data..?> "compute")
            Prelude.<*> (x Data..?> "dataSources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "failureBehavior")
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "failureReason")
            Prelude.<*> (x Data..?> "iamRole")
            Prelude.<*> (x Data..?> "lastStartedAt")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "loggingConfig")
            Prelude.<*> (x Data..?> "maxJobDurationInSeconds")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "networkInterface")
            Prelude.<*> (x Data..?> "outputLocation")
            Prelude.<*> (x Data..?> "robotApplications")
            Prelude.<*> (x Data..?> "simulationApplications")
            Prelude.<*> (x Data..?> "simulationTimeMillis")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vpcConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSimulationJob where
  hashWithSalt _salt DescribeSimulationJob' {..} =
    _salt `Prelude.hashWithSalt` job

instance Prelude.NFData DescribeSimulationJob where
  rnf DescribeSimulationJob' {..} = Prelude.rnf job

instance Data.ToHeaders DescribeSimulationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSimulationJob where
  toJSON DescribeSimulationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Data..= job)]
      )

instance Data.ToPath DescribeSimulationJob where
  toPath = Prelude.const "/describeSimulationJob"

instance Data.ToQuery DescribeSimulationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSimulationJobResponse' smart constructor.
data DescribeSimulationJobResponse = DescribeSimulationJobResponse'
  { -- | The Amazon Resource Name (ARN) of the simulation job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Compute information for the simulation job.
    compute :: Prelude.Maybe ComputeResponse,
    -- | The data sources for the simulation job.
    dataSources :: Prelude.Maybe [DataSource],
    -- | The failure behavior for the simulation job.
    failureBehavior :: Prelude.Maybe FailureBehavior,
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
    -- | Details about why the simulation job failed. For more information about
    -- troubleshooting, see
    -- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting.html Troubleshooting>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that allows the simulation instance to call the AWS APIs
    -- that are specified in its associated policies on your behalf.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the simulation job was
    -- last started.
    lastStartedAt :: Prelude.Maybe Data.POSIX,
    -- | The time, in milliseconds since the epoch, when the simulation job was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The logging configuration.
    loggingConfig :: Prelude.Maybe LoggingConfig,
    -- | The maximum job duration in seconds. The value must be 8 days (691,200
    -- seconds) or less.
    maxJobDurationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The name of the simulation job.
    name :: Prelude.Maybe Prelude.Text,
    -- | The network interface information for the simulation job.
    networkInterface :: Prelude.Maybe NetworkInterface,
    -- | Location for output files generated by the simulation job.
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | A list of robot applications.
    robotApplications :: Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig),
    -- | A list of simulation applications.
    simulationApplications :: Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig),
    -- | The simulation job execution duration in milliseconds.
    simulationTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The status of the simulation job.
    status :: Prelude.Maybe SimulationJobStatus,
    -- | The list of all tags added to the specified simulation job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The VPC configuration.
    vpcConfig :: Prelude.Maybe VPCConfigResponse,
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
-- 'arn', 'describeSimulationJobResponse_arn' - The Amazon Resource Name (ARN) of the simulation job.
--
-- 'clientRequestToken', 'describeSimulationJobResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'compute', 'describeSimulationJobResponse_compute' - Compute information for the simulation job.
--
-- 'dataSources', 'describeSimulationJobResponse_dataSources' - The data sources for the simulation job.
--
-- 'failureBehavior', 'describeSimulationJobResponse_failureBehavior' - The failure behavior for the simulation job.
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
-- 'failureReason', 'describeSimulationJobResponse_failureReason' - Details about why the simulation job failed. For more information about
-- troubleshooting, see
-- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting.html Troubleshooting>.
--
-- 'iamRole', 'describeSimulationJobResponse_iamRole' - The IAM role that allows the simulation instance to call the AWS APIs
-- that are specified in its associated policies on your behalf.
--
-- 'lastStartedAt', 'describeSimulationJobResponse_lastStartedAt' - The time, in milliseconds since the epoch, when the simulation job was
-- last started.
--
-- 'lastUpdatedAt', 'describeSimulationJobResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation job was
-- last updated.
--
-- 'loggingConfig', 'describeSimulationJobResponse_loggingConfig' - The logging configuration.
--
-- 'maxJobDurationInSeconds', 'describeSimulationJobResponse_maxJobDurationInSeconds' - The maximum job duration in seconds. The value must be 8 days (691,200
-- seconds) or less.
--
-- 'name', 'describeSimulationJobResponse_name' - The name of the simulation job.
--
-- 'networkInterface', 'describeSimulationJobResponse_networkInterface' - The network interface information for the simulation job.
--
-- 'outputLocation', 'describeSimulationJobResponse_outputLocation' - Location for output files generated by the simulation job.
--
-- 'robotApplications', 'describeSimulationJobResponse_robotApplications' - A list of robot applications.
--
-- 'simulationApplications', 'describeSimulationJobResponse_simulationApplications' - A list of simulation applications.
--
-- 'simulationTimeMillis', 'describeSimulationJobResponse_simulationTimeMillis' - The simulation job execution duration in milliseconds.
--
-- 'status', 'describeSimulationJobResponse_status' - The status of the simulation job.
--
-- 'tags', 'describeSimulationJobResponse_tags' - The list of all tags added to the specified simulation job.
--
-- 'vpcConfig', 'describeSimulationJobResponse_vpcConfig' - The VPC configuration.
--
-- 'httpStatus', 'describeSimulationJobResponse_httpStatus' - The response's http status code.
newDescribeSimulationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSimulationJobResponse
newDescribeSimulationJobResponse pHttpStatus_ =
  DescribeSimulationJobResponse'
    { arn =
        Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      compute = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      failureBehavior = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      lastStartedAt = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      loggingConfig = Prelude.Nothing,
      maxJobDurationInSeconds = Prelude.Nothing,
      name = Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      robotApplications = Prelude.Nothing,
      simulationApplications = Prelude.Nothing,
      simulationTimeMillis = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the simulation job.
describeSimulationJobResponse_arn :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_arn = Lens.lens (\DescribeSimulationJobResponse' {arn} -> arn) (\s@DescribeSimulationJobResponse' {} a -> s {arn = a} :: DescribeSimulationJobResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
describeSimulationJobResponse_clientRequestToken :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_clientRequestToken = Lens.lens (\DescribeSimulationJobResponse' {clientRequestToken} -> clientRequestToken) (\s@DescribeSimulationJobResponse' {} a -> s {clientRequestToken = a} :: DescribeSimulationJobResponse)

-- | Compute information for the simulation job.
describeSimulationJobResponse_compute :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe ComputeResponse)
describeSimulationJobResponse_compute = Lens.lens (\DescribeSimulationJobResponse' {compute} -> compute) (\s@DescribeSimulationJobResponse' {} a -> s {compute = a} :: DescribeSimulationJobResponse)

-- | The data sources for the simulation job.
describeSimulationJobResponse_dataSources :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe [DataSource])
describeSimulationJobResponse_dataSources = Lens.lens (\DescribeSimulationJobResponse' {dataSources} -> dataSources) (\s@DescribeSimulationJobResponse' {} a -> s {dataSources = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The failure behavior for the simulation job.
describeSimulationJobResponse_failureBehavior :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe FailureBehavior)
describeSimulationJobResponse_failureBehavior = Lens.lens (\DescribeSimulationJobResponse' {failureBehavior} -> failureBehavior) (\s@DescribeSimulationJobResponse' {} a -> s {failureBehavior = a} :: DescribeSimulationJobResponse)

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

-- | Details about why the simulation job failed. For more information about
-- troubleshooting, see
-- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting.html Troubleshooting>.
describeSimulationJobResponse_failureReason :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_failureReason = Lens.lens (\DescribeSimulationJobResponse' {failureReason} -> failureReason) (\s@DescribeSimulationJobResponse' {} a -> s {failureReason = a} :: DescribeSimulationJobResponse)

-- | The IAM role that allows the simulation instance to call the AWS APIs
-- that are specified in its associated policies on your behalf.
describeSimulationJobResponse_iamRole :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_iamRole = Lens.lens (\DescribeSimulationJobResponse' {iamRole} -> iamRole) (\s@DescribeSimulationJobResponse' {} a -> s {iamRole = a} :: DescribeSimulationJobResponse)

-- | The time, in milliseconds since the epoch, when the simulation job was
-- last started.
describeSimulationJobResponse_lastStartedAt :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.UTCTime)
describeSimulationJobResponse_lastStartedAt = Lens.lens (\DescribeSimulationJobResponse' {lastStartedAt} -> lastStartedAt) (\s@DescribeSimulationJobResponse' {} a -> s {lastStartedAt = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Data._Time

-- | The time, in milliseconds since the epoch, when the simulation job was
-- last updated.
describeSimulationJobResponse_lastUpdatedAt :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.UTCTime)
describeSimulationJobResponse_lastUpdatedAt = Lens.lens (\DescribeSimulationJobResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeSimulationJobResponse' {} a -> s {lastUpdatedAt = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Data._Time

-- | The logging configuration.
describeSimulationJobResponse_loggingConfig :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe LoggingConfig)
describeSimulationJobResponse_loggingConfig = Lens.lens (\DescribeSimulationJobResponse' {loggingConfig} -> loggingConfig) (\s@DescribeSimulationJobResponse' {} a -> s {loggingConfig = a} :: DescribeSimulationJobResponse)

-- | The maximum job duration in seconds. The value must be 8 days (691,200
-- seconds) or less.
describeSimulationJobResponse_maxJobDurationInSeconds :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Integer)
describeSimulationJobResponse_maxJobDurationInSeconds = Lens.lens (\DescribeSimulationJobResponse' {maxJobDurationInSeconds} -> maxJobDurationInSeconds) (\s@DescribeSimulationJobResponse' {} a -> s {maxJobDurationInSeconds = a} :: DescribeSimulationJobResponse)

-- | The name of the simulation job.
describeSimulationJobResponse_name :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Text)
describeSimulationJobResponse_name = Lens.lens (\DescribeSimulationJobResponse' {name} -> name) (\s@DescribeSimulationJobResponse' {} a -> s {name = a} :: DescribeSimulationJobResponse)

-- | The network interface information for the simulation job.
describeSimulationJobResponse_networkInterface :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe NetworkInterface)
describeSimulationJobResponse_networkInterface = Lens.lens (\DescribeSimulationJobResponse' {networkInterface} -> networkInterface) (\s@DescribeSimulationJobResponse' {} a -> s {networkInterface = a} :: DescribeSimulationJobResponse)

-- | Location for output files generated by the simulation job.
describeSimulationJobResponse_outputLocation :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe OutputLocation)
describeSimulationJobResponse_outputLocation = Lens.lens (\DescribeSimulationJobResponse' {outputLocation} -> outputLocation) (\s@DescribeSimulationJobResponse' {} a -> s {outputLocation = a} :: DescribeSimulationJobResponse)

-- | A list of robot applications.
describeSimulationJobResponse_robotApplications :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig))
describeSimulationJobResponse_robotApplications = Lens.lens (\DescribeSimulationJobResponse' {robotApplications} -> robotApplications) (\s@DescribeSimulationJobResponse' {} a -> s {robotApplications = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of simulation applications.
describeSimulationJobResponse_simulationApplications :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig))
describeSimulationJobResponse_simulationApplications = Lens.lens (\DescribeSimulationJobResponse' {simulationApplications} -> simulationApplications) (\s@DescribeSimulationJobResponse' {} a -> s {simulationApplications = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The simulation job execution duration in milliseconds.
describeSimulationJobResponse_simulationTimeMillis :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe Prelude.Integer)
describeSimulationJobResponse_simulationTimeMillis = Lens.lens (\DescribeSimulationJobResponse' {simulationTimeMillis} -> simulationTimeMillis) (\s@DescribeSimulationJobResponse' {} a -> s {simulationTimeMillis = a} :: DescribeSimulationJobResponse)

-- | The status of the simulation job.
describeSimulationJobResponse_status :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe SimulationJobStatus)
describeSimulationJobResponse_status = Lens.lens (\DescribeSimulationJobResponse' {status} -> status) (\s@DescribeSimulationJobResponse' {} a -> s {status = a} :: DescribeSimulationJobResponse)

-- | The list of all tags added to the specified simulation job.
describeSimulationJobResponse_tags :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeSimulationJobResponse_tags = Lens.lens (\DescribeSimulationJobResponse' {tags} -> tags) (\s@DescribeSimulationJobResponse' {} a -> s {tags = a} :: DescribeSimulationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The VPC configuration.
describeSimulationJobResponse_vpcConfig :: Lens.Lens' DescribeSimulationJobResponse (Prelude.Maybe VPCConfigResponse)
describeSimulationJobResponse_vpcConfig = Lens.lens (\DescribeSimulationJobResponse' {vpcConfig} -> vpcConfig) (\s@DescribeSimulationJobResponse' {} a -> s {vpcConfig = a} :: DescribeSimulationJobResponse)

-- | The response's http status code.
describeSimulationJobResponse_httpStatus :: Lens.Lens' DescribeSimulationJobResponse Prelude.Int
describeSimulationJobResponse_httpStatus = Lens.lens (\DescribeSimulationJobResponse' {httpStatus} -> httpStatus) (\s@DescribeSimulationJobResponse' {} a -> s {httpStatus = a} :: DescribeSimulationJobResponse)

instance Prelude.NFData DescribeSimulationJobResponse where
  rnf DescribeSimulationJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf compute
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf failureBehavior
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf lastStartedAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf loggingConfig
      `Prelude.seq` Prelude.rnf maxJobDurationInSeconds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkInterface
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf robotApplications
      `Prelude.seq` Prelude.rnf simulationApplications
      `Prelude.seq` Prelude.rnf simulationTimeMillis
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf httpStatus
