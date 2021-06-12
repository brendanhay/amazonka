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
-- Module      : Network.AWS.Glue.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new job definition.
module Network.AWS.Glue.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_nonOverridableArguments,
    createJob_securityConfiguration,
    createJob_timeout,
    createJob_maxCapacity,
    createJob_connections,
    createJob_notificationProperty,
    createJob_numberOfWorkers,
    createJob_glueVersion,
    createJob_tags,
    createJob_workerType,
    createJob_description,
    createJob_defaultArguments,
    createJob_allocatedCapacity,
    createJob_executionProperty,
    createJob_maxRetries,
    createJob_logUri,
    createJob_name,
    createJob_role,
    createJob_command,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_name,
    createJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job.
    securityConfiguration :: Core.Maybe Core.Text,
    -- | The job timeout in minutes. This is the maximum time that a job run can
    -- consume resources before it is terminated and enters @TIMEOUT@ status.
    -- The default is 2,880 minutes (48 hours).
    timeout :: Core.Maybe Core.Natural,
    -- | The number of AWS Glue data processing units (DPUs) that can be
    -- allocated when this job runs. A DPU is a relative measure of processing
    -- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
    -- For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
    --
    -- The value that can be allocated for @MaxCapacity@ depends on whether you
    -- are running a Python shell job or an Apache Spark ETL job:
    --
    -- -   When you specify a Python shell job
    --     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
    --     or 1 DPU. The default is 0.0625 DPU.
    --
    -- -   When you specify an Apache Spark ETL job
    --     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
    --     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
    --     100 DPUs. The default is 10 DPUs. This job type cannot have a
    --     fractional DPU allocation.
    maxCapacity :: Core.Maybe Core.Double,
    -- | The connections used for this job.
    connections :: Core.Maybe ConnectionsList,
    -- | Specifies configuration properties of a job notification.
    notificationProperty :: Core.Maybe NotificationProperty,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | Glue version determines the versions of Apache Spark and Python that AWS
    -- Glue supports. The Python version indicates the version supported for
    -- jobs of type Spark.
    --
    -- For more information about the available AWS Glue versions and
    -- corresponding Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    --
    -- Jobs that are created without specifying a Glue version default to Glue
    -- 0.9.
    glueVersion :: Core.Maybe Core.Text,
    -- | The tags to use with this job. You may use tags to limit access to the
    -- job. For more information about tags in AWS Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
    -- in the developer guide.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The type of predefined worker that is allocated when a job runs. Accepts
    -- a value of Standard, G.1X, or G.2X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
    --     of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
    --     of memory, 128 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    workerType :: Core.Maybe WorkerType,
    -- | Description of the job being defined.
    description :: Core.Maybe Core.Text,
    -- | The default arguments for this job.
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
    defaultArguments :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | This parameter is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) to allocate to this
    -- Job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
    -- relative measure of processing power that consists of 4 vCPUs of compute
    -- capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    allocatedCapacity :: Core.Maybe Core.Int,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
    -- allowed for this job.
    executionProperty :: Core.Maybe ExecutionProperty,
    -- | The maximum number of times to retry this job if it fails.
    maxRetries :: Core.Maybe Core.Int,
    -- | This field is reserved for future use.
    logUri :: Core.Maybe Core.Text,
    -- | The name you assign to this job definition. It must be unique in your
    -- account.
    name :: Core.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- this job.
    role' :: Core.Text,
    -- | The @JobCommand@ that executes this job.
    command :: JobCommand
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonOverridableArguments', 'createJob_nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- 'securityConfiguration', 'createJob_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job.
--
-- 'timeout', 'createJob_timeout' - The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
--
-- 'maxCapacity', 'createJob_maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job or an Apache Spark ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
--     100 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- 'connections', 'createJob_connections' - The connections used for this job.
--
-- 'notificationProperty', 'createJob_notificationProperty' - Specifies configuration properties of a job notification.
--
-- 'numberOfWorkers', 'createJob_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'glueVersion', 'createJob_glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available AWS Glue versions and
-- corresponding Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Jobs that are created without specifying a Glue version default to Glue
-- 0.9.
--
-- 'tags', 'createJob_tags' - The tags to use with this job. You may use tags to limit access to the
-- job. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
--
-- 'workerType', 'createJob_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- 'description', 'createJob_description' - Description of the job being defined.
--
-- 'defaultArguments', 'createJob_defaultArguments' - The default arguments for this job.
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
-- 'allocatedCapacity', 'createJob_allocatedCapacity' - This parameter is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this
-- Job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- 'executionProperty', 'createJob_executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
--
-- 'maxRetries', 'createJob_maxRetries' - The maximum number of times to retry this job if it fails.
--
-- 'logUri', 'createJob_logUri' - This field is reserved for future use.
--
-- 'name', 'createJob_name' - The name you assign to this job definition. It must be unique in your
-- account.
--
-- 'role'', 'createJob_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
--
-- 'command', 'createJob_command' - The @JobCommand@ that executes this job.
newCreateJob ::
  -- | 'name'
  Core.Text ->
  -- | 'role''
  Core.Text ->
  -- | 'command'
  JobCommand ->
  CreateJob
newCreateJob pName_ pRole_ pCommand_ =
  CreateJob'
    { nonOverridableArguments = Core.Nothing,
      securityConfiguration = Core.Nothing,
      timeout = Core.Nothing,
      maxCapacity = Core.Nothing,
      connections = Core.Nothing,
      notificationProperty = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      glueVersion = Core.Nothing,
      tags = Core.Nothing,
      workerType = Core.Nothing,
      description = Core.Nothing,
      defaultArguments = Core.Nothing,
      allocatedCapacity = Core.Nothing,
      executionProperty = Core.Nothing,
      maxRetries = Core.Nothing,
      logUri = Core.Nothing,
      name = pName_,
      role' = pRole_,
      command = pCommand_
    }

-- | Non-overridable arguments for this job, specified as name-value pairs.
createJob_nonOverridableArguments :: Lens.Lens' CreateJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
createJob_nonOverridableArguments = Lens.lens (\CreateJob' {nonOverridableArguments} -> nonOverridableArguments) (\s@CreateJob' {} a -> s {nonOverridableArguments = a} :: CreateJob) Core.. Lens.mapping Lens._Coerce

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job.
createJob_securityConfiguration :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
createJob_securityConfiguration = Lens.lens (\CreateJob' {securityConfiguration} -> securityConfiguration) (\s@CreateJob' {} a -> s {securityConfiguration = a} :: CreateJob)

-- | The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
createJob_timeout :: Lens.Lens' CreateJob (Core.Maybe Core.Natural)
createJob_timeout = Lens.lens (\CreateJob' {timeout} -> timeout) (\s@CreateJob' {} a -> s {timeout = a} :: CreateJob)

-- | The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job or an Apache Spark ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
--     100 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
createJob_maxCapacity :: Lens.Lens' CreateJob (Core.Maybe Core.Double)
createJob_maxCapacity = Lens.lens (\CreateJob' {maxCapacity} -> maxCapacity) (\s@CreateJob' {} a -> s {maxCapacity = a} :: CreateJob)

-- | The connections used for this job.
createJob_connections :: Lens.Lens' CreateJob (Core.Maybe ConnectionsList)
createJob_connections = Lens.lens (\CreateJob' {connections} -> connections) (\s@CreateJob' {} a -> s {connections = a} :: CreateJob)

-- | Specifies configuration properties of a job notification.
createJob_notificationProperty :: Lens.Lens' CreateJob (Core.Maybe NotificationProperty)
createJob_notificationProperty = Lens.lens (\CreateJob' {notificationProperty} -> notificationProperty) (\s@CreateJob' {} a -> s {notificationProperty = a} :: CreateJob)

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
createJob_numberOfWorkers :: Lens.Lens' CreateJob (Core.Maybe Core.Int)
createJob_numberOfWorkers = Lens.lens (\CreateJob' {numberOfWorkers} -> numberOfWorkers) (\s@CreateJob' {} a -> s {numberOfWorkers = a} :: CreateJob)

-- | Glue version determines the versions of Apache Spark and Python that AWS
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available AWS Glue versions and
-- corresponding Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Jobs that are created without specifying a Glue version default to Glue
-- 0.9.
createJob_glueVersion :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
createJob_glueVersion = Lens.lens (\CreateJob' {glueVersion} -> glueVersion) (\s@CreateJob' {} a -> s {glueVersion = a} :: CreateJob)

-- | The tags to use with this job. You may use tags to limit access to the
-- job. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
createJob_tags :: Lens.Lens' CreateJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
createJob_tags = Lens.lens (\CreateJob' {tags} -> tags) (\s@CreateJob' {} a -> s {tags = a} :: CreateJob) Core.. Lens.mapping Lens._Coerce

-- | The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
createJob_workerType :: Lens.Lens' CreateJob (Core.Maybe WorkerType)
createJob_workerType = Lens.lens (\CreateJob' {workerType} -> workerType) (\s@CreateJob' {} a -> s {workerType = a} :: CreateJob)

-- | Description of the job being defined.
createJob_description :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
createJob_description = Lens.lens (\CreateJob' {description} -> description) (\s@CreateJob' {} a -> s {description = a} :: CreateJob)

-- | The default arguments for this job.
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
createJob_defaultArguments :: Lens.Lens' CreateJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
createJob_defaultArguments = Lens.lens (\CreateJob' {defaultArguments} -> defaultArguments) (\s@CreateJob' {} a -> s {defaultArguments = a} :: CreateJob) Core.. Lens.mapping Lens._Coerce

-- | This parameter is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this
-- Job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
createJob_allocatedCapacity :: Lens.Lens' CreateJob (Core.Maybe Core.Int)
createJob_allocatedCapacity = Lens.lens (\CreateJob' {allocatedCapacity} -> allocatedCapacity) (\s@CreateJob' {} a -> s {allocatedCapacity = a} :: CreateJob)

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
createJob_executionProperty :: Lens.Lens' CreateJob (Core.Maybe ExecutionProperty)
createJob_executionProperty = Lens.lens (\CreateJob' {executionProperty} -> executionProperty) (\s@CreateJob' {} a -> s {executionProperty = a} :: CreateJob)

-- | The maximum number of times to retry this job if it fails.
createJob_maxRetries :: Lens.Lens' CreateJob (Core.Maybe Core.Int)
createJob_maxRetries = Lens.lens (\CreateJob' {maxRetries} -> maxRetries) (\s@CreateJob' {} a -> s {maxRetries = a} :: CreateJob)

-- | This field is reserved for future use.
createJob_logUri :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
createJob_logUri = Lens.lens (\CreateJob' {logUri} -> logUri) (\s@CreateJob' {} a -> s {logUri = a} :: CreateJob)

-- | The name you assign to this job definition. It must be unique in your
-- account.
createJob_name :: Lens.Lens' CreateJob Core.Text
createJob_name = Lens.lens (\CreateJob' {name} -> name) (\s@CreateJob' {} a -> s {name = a} :: CreateJob)

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
createJob_role :: Lens.Lens' CreateJob Core.Text
createJob_role = Lens.lens (\CreateJob' {role'} -> role') (\s@CreateJob' {} a -> s {role' = a} :: CreateJob)

-- | The @JobCommand@ that executes this job.
createJob_command :: Lens.Lens' CreateJob JobCommand
createJob_command = Lens.lens (\CreateJob' {command} -> command) (\s@CreateJob' {} a -> s {command = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Core.<$> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateJob

instance Core.NFData CreateJob

instance Core.ToHeaders CreateJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NonOverridableArguments" Core..=)
              Core.<$> nonOverridableArguments,
            ("SecurityConfiguration" Core..=)
              Core.<$> securityConfiguration,
            ("Timeout" Core..=) Core.<$> timeout,
            ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("Connections" Core..=) Core.<$> connections,
            ("NotificationProperty" Core..=)
              Core.<$> notificationProperty,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("Tags" Core..=) Core.<$> tags,
            ("WorkerType" Core..=) Core.<$> workerType,
            ("Description" Core..=) Core.<$> description,
            ("DefaultArguments" Core..=)
              Core.<$> defaultArguments,
            ("AllocatedCapacity" Core..=)
              Core.<$> allocatedCapacity,
            ("ExecutionProperty" Core..=)
              Core.<$> executionProperty,
            ("MaxRetries" Core..=) Core.<$> maxRetries,
            ("LogUri" Core..=) Core.<$> logUri,
            Core.Just ("Name" Core..= name),
            Core.Just ("Role" Core..= role'),
            Core.Just ("Command" Core..= command)
          ]
      )

instance Core.ToPath CreateJob where
  toPath = Core.const "/"

instance Core.ToQuery CreateJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The unique name that was provided for this job definition.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createJobResponse_name' - The unique name that was provided for this job definition.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique name that was provided for this job definition.
createJobResponse_name :: Lens.Lens' CreateJobResponse (Core.Maybe Core.Text)
createJobResponse_name = Lens.lens (\CreateJobResponse' {name} -> name) (\s@CreateJobResponse' {} a -> s {name = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Core.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Core.NFData CreateJobResponse
