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
-- Module      : Network.AWS.Glue.StartJobRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job run using a job definition.
module Network.AWS.Glue.StartJobRun
  ( -- * Creating a Request
    StartJobRun (..),
    newStartJobRun,

    -- * Request Lenses
    startJobRun_securityConfiguration,
    startJobRun_timeout,
    startJobRun_maxCapacity,
    startJobRun_notificationProperty,
    startJobRun_numberOfWorkers,
    startJobRun_workerType,
    startJobRun_jobRunId,
    startJobRun_arguments,
    startJobRun_allocatedCapacity,
    startJobRun_jobName,

    -- * Destructuring the Response
    StartJobRunResponse (..),
    newStartJobRunResponse,

    -- * Response Lenses
    startJobRunResponse_jobRunId,
    startJobRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartJobRun' smart constructor.
data StartJobRun = StartJobRun'
  { -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job run.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours). This overrides the
    -- timeout value set in the parent job.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The number of AWS Glue data processing units (DPUs) that can be
    -- allocated when this job runs. A DPU is a relative measure of processing
    -- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
    -- For more information, see the
    -- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
    --
    -- The value that can be allocated for @MaxCapacity@ depends on whether you
    -- are running a Python shell job, or an Apache Spark ETL job:
    --
    -- -   When you specify a Python shell job
    --     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
    --     or 1 DPU. The default is 0.0625 DPU.
    --
    -- -   When you specify an Apache Spark ETL job
    --     (@JobCommand.Name@=\"glueetl\"), you can allocate from 2 to 100
    --     DPUs. The default is 10 DPUs. This job type cannot have a fractional
    --     DPU allocation.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | Specifies configuration properties of a job run notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The type of predefined worker that is allocated when a job runs. Accepts
    -- a value of Standard, G.1X, or G.2X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
    --     memory and a 64GB disk, and 1 executor per worker.
    --
    -- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
    --     memory and a 128GB disk, and 1 executor per worker.
    workerType :: Prelude.Maybe WorkerType,
    -- | The ID of a previous @JobRun@ to retry.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | The job arguments specifically for this run. For this job run, they
    -- replace the default arguments set in the job definition itself.
    --
    -- You can specify arguments here that your own job-execution script
    -- consumes, as well as arguments that AWS Glue itself consumes.
    --
    -- For information about how to specify and consume your own Job arguments,
    -- see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
    -- topic in the developer guide.
    --
    -- For information about the key-value pairs that AWS Glue consumes to set
    -- up your job, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
    -- topic in the developer guide.
    arguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) to allocate to this
    -- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
    -- a relative measure of processing power that consists of 4 vCPUs of
    -- compute capacity and 16 GB of memory. For more information, see the
    -- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The name of the job definition to use.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'startJobRun_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
--
-- 'timeout', 'startJobRun_timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
--
-- 'maxCapacity', 'startJobRun_maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job, or an Apache Spark ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\"), you can allocate from 2 to 100
--     DPUs. The default is 10 DPUs. This job type cannot have a fractional
--     DPU allocation.
--
-- 'notificationProperty', 'startJobRun_notificationProperty' - Specifies configuration properties of a job run notification.
--
-- 'numberOfWorkers', 'startJobRun_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'workerType', 'startJobRun_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
--     memory and a 64GB disk, and 1 executor per worker.
--
-- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
--     memory and a 128GB disk, and 1 executor per worker.
--
-- 'jobRunId', 'startJobRun_jobRunId' - The ID of a previous @JobRun@ to retry.
--
-- 'arguments', 'startJobRun_arguments' - The job arguments specifically for this run. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that AWS Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that AWS Glue consumes to set
-- up your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
-- topic in the developer guide.
--
-- 'allocatedCapacity', 'startJobRun_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- 'jobName', 'startJobRun_jobName' - The name of the job definition to use.
newStartJobRun ::
  -- | 'jobName'
  Prelude.Text ->
  StartJobRun
newStartJobRun pJobName_ =
  StartJobRun'
    { securityConfiguration =
        Prelude.Nothing,
      timeout = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      workerType = Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      arguments = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      jobName = pJobName_
    }

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
startJobRun_securityConfiguration :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Text)
startJobRun_securityConfiguration = Lens.lens (\StartJobRun' {securityConfiguration} -> securityConfiguration) (\s@StartJobRun' {} a -> s {securityConfiguration = a} :: StartJobRun)

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
startJobRun_timeout :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Natural)
startJobRun_timeout = Lens.lens (\StartJobRun' {timeout} -> timeout) (\s@StartJobRun' {} a -> s {timeout = a} :: StartJobRun)

-- | The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job, or an Apache Spark ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\"), you can allocate from 2 to 100
--     DPUs. The default is 10 DPUs. This job type cannot have a fractional
--     DPU allocation.
startJobRun_maxCapacity :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Double)
startJobRun_maxCapacity = Lens.lens (\StartJobRun' {maxCapacity} -> maxCapacity) (\s@StartJobRun' {} a -> s {maxCapacity = a} :: StartJobRun)

-- | Specifies configuration properties of a job run notification.
startJobRun_notificationProperty :: Lens.Lens' StartJobRun (Prelude.Maybe NotificationProperty)
startJobRun_notificationProperty = Lens.lens (\StartJobRun' {notificationProperty} -> notificationProperty) (\s@StartJobRun' {} a -> s {notificationProperty = a} :: StartJobRun)

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
startJobRun_numberOfWorkers :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Int)
startJobRun_numberOfWorkers = Lens.lens (\StartJobRun' {numberOfWorkers} -> numberOfWorkers) (\s@StartJobRun' {} a -> s {numberOfWorkers = a} :: StartJobRun)

-- | The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
--     memory and a 64GB disk, and 1 executor per worker.
--
-- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
--     memory and a 128GB disk, and 1 executor per worker.
startJobRun_workerType :: Lens.Lens' StartJobRun (Prelude.Maybe WorkerType)
startJobRun_workerType = Lens.lens (\StartJobRun' {workerType} -> workerType) (\s@StartJobRun' {} a -> s {workerType = a} :: StartJobRun)

-- | The ID of a previous @JobRun@ to retry.
startJobRun_jobRunId :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Text)
startJobRun_jobRunId = Lens.lens (\StartJobRun' {jobRunId} -> jobRunId) (\s@StartJobRun' {} a -> s {jobRunId = a} :: StartJobRun)

-- | The job arguments specifically for this run. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that AWS Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that AWS Glue consumes to set
-- up your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
-- topic in the developer guide.
startJobRun_arguments :: Lens.Lens' StartJobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startJobRun_arguments = Lens.lens (\StartJobRun' {arguments} -> arguments) (\s@StartJobRun' {} a -> s {arguments = a} :: StartJobRun) Prelude.. Lens.mapping Lens._Coerce

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
startJobRun_allocatedCapacity :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Int)
startJobRun_allocatedCapacity = Lens.lens (\StartJobRun' {allocatedCapacity} -> allocatedCapacity) (\s@StartJobRun' {} a -> s {allocatedCapacity = a} :: StartJobRun)

-- | The name of the job definition to use.
startJobRun_jobName :: Lens.Lens' StartJobRun Prelude.Text
startJobRun_jobName = Lens.lens (\StartJobRun' {jobName} -> jobName) (\s@StartJobRun' {} a -> s {jobName = a} :: StartJobRun)

instance Core.AWSRequest StartJobRun where
  type AWSResponse StartJobRun = StartJobRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartJobRunResponse'
            Prelude.<$> (x Core..?> "JobRunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartJobRun

instance Prelude.NFData StartJobRun

instance Core.ToHeaders StartJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.StartJobRun" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartJobRun where
  toJSON StartJobRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityConfiguration" Core..=)
              Prelude.<$> securityConfiguration,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("MaxCapacity" Core..=) Prelude.<$> maxCapacity,
            ("NotificationProperty" Core..=)
              Prelude.<$> notificationProperty,
            ("NumberOfWorkers" Core..=)
              Prelude.<$> numberOfWorkers,
            ("WorkerType" Core..=) Prelude.<$> workerType,
            ("JobRunId" Core..=) Prelude.<$> jobRunId,
            ("Arguments" Core..=) Prelude.<$> arguments,
            ("AllocatedCapacity" Core..=)
              Prelude.<$> allocatedCapacity,
            Prelude.Just ("JobName" Core..= jobName)
          ]
      )

instance Core.ToPath StartJobRun where
  toPath = Prelude.const "/"

instance Core.ToQuery StartJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartJobRunResponse' smart constructor.
data StartJobRunResponse = StartJobRunResponse'
  { -- | The ID assigned to this job run.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobRunId', 'startJobRunResponse_jobRunId' - The ID assigned to this job run.
--
-- 'httpStatus', 'startJobRunResponse_httpStatus' - The response's http status code.
newStartJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartJobRunResponse
newStartJobRunResponse pHttpStatus_ =
  StartJobRunResponse'
    { jobRunId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID assigned to this job run.
startJobRunResponse_jobRunId :: Lens.Lens' StartJobRunResponse (Prelude.Maybe Prelude.Text)
startJobRunResponse_jobRunId = Lens.lens (\StartJobRunResponse' {jobRunId} -> jobRunId) (\s@StartJobRunResponse' {} a -> s {jobRunId = a} :: StartJobRunResponse)

-- | The response's http status code.
startJobRunResponse_httpStatus :: Lens.Lens' StartJobRunResponse Prelude.Int
startJobRunResponse_httpStatus = Lens.lens (\StartJobRunResponse' {httpStatus} -> httpStatus) (\s@StartJobRunResponse' {} a -> s {httpStatus = a} :: StartJobRunResponse)

instance Prelude.NFData StartJobRunResponse
