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
-- Module      : Amazonka.Glue.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new job definition.
module Amazonka.Glue.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_numberOfWorkers,
    createJob_notificationProperty,
    createJob_connections,
    createJob_workerType,
    createJob_securityConfiguration,
    createJob_glueVersion,
    createJob_nonOverridableArguments,
    createJob_logUri,
    createJob_maxRetries,
    createJob_executionProperty,
    createJob_allocatedCapacity,
    createJob_maxCapacity,
    createJob_timeout,
    createJob_defaultArguments,
    createJob_description,
    createJob_tags,
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

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | Specifies configuration properties of a job notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The connections used for this job.
    connections :: Prelude.Maybe ConnectionsList,
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
    workerType :: Prelude.Maybe WorkerType,
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The Python version indicates the version supported for
    -- jobs of type Spark.
    --
    -- For more information about the available Glue versions and corresponding
    -- Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    --
    -- Jobs that are created without specifying a Glue version default to Glue
    -- 0.9.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This field is reserved for future use.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry this job if it fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
    -- allowed for this job.
    executionProperty :: Prelude.Maybe ExecutionProperty,
    -- | This parameter is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of Glue data processing units (DPUs) to allocate to this Job.
    -- You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
    -- relative measure of processing power that consists of 4 vCPUs of compute
    -- capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | For Glue version 1.0 or earlier jobs, using the standard worker type,
    -- the number of Glue data processing units (DPUs) that can be allocated
    -- when this job runs. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
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
    -- For Glue version 2.0 jobs, you cannot instead specify a
    -- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
    -- @Number of workers@.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The job timeout in minutes. This is the maximum time that a job run can
    -- consume resources before it is terminated and enters @TIMEOUT@ status.
    -- The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The default arguments for this job.
    --
    -- You can specify arguments here that your own job-execution script
    -- consumes, as well as arguments that Glue itself consumes.
    --
    -- For information about how to specify and consume your own Job arguments,
    -- see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
    -- topic in the developer guide.
    --
    -- For information about the key-value pairs that Glue consumes to set up
    -- your job, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
    -- topic in the developer guide.
    defaultArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Description of the job being defined.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to use with this job. You may use tags to limit access to the
    -- job. For more information about tags in Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
    -- in the developer guide.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name you assign to this job definition. It must be unique in your
    -- account.
    name :: Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- this job.
    role' :: Prelude.Text,
    -- | The @JobCommand@ that runs this job.
    command :: JobCommand
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfWorkers', 'createJob_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'notificationProperty', 'createJob_notificationProperty' - Specifies configuration properties of a job notification.
--
-- 'connections', 'createJob_connections' - The connections used for this job.
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
-- 'securityConfiguration', 'createJob_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job.
--
-- 'glueVersion', 'createJob_glueVersion' - Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Jobs that are created without specifying a Glue version default to Glue
-- 0.9.
--
-- 'nonOverridableArguments', 'createJob_nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- 'logUri', 'createJob_logUri' - This field is reserved for future use.
--
-- 'maxRetries', 'createJob_maxRetries' - The maximum number of times to retry this job if it fails.
--
-- 'executionProperty', 'createJob_executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
--
-- 'allocatedCapacity', 'createJob_allocatedCapacity' - This parameter is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) to allocate to this Job.
-- You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- 'maxCapacity', 'createJob_maxCapacity' - For Glue version 1.0 or earlier jobs, using the standard worker type,
-- the number of Glue data processing units (DPUs) that can be allocated
-- when this job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
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
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
--
-- 'timeout', 'createJob_timeout' - The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
--
-- 'defaultArguments', 'createJob_defaultArguments' - The default arguments for this job.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
--
-- 'description', 'createJob_description' - Description of the job being defined.
--
-- 'tags', 'createJob_tags' - The tags to use with this job. You may use tags to limit access to the
-- job. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
--
-- 'name', 'createJob_name' - The name you assign to this job definition. It must be unique in your
-- account.
--
-- 'role'', 'createJob_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
--
-- 'command', 'createJob_command' - The @JobCommand@ that runs this job.
newCreateJob ::
  -- | 'name'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'command'
  JobCommand ->
  CreateJob
newCreateJob pName_ pRole_ pCommand_ =
  CreateJob'
    { numberOfWorkers = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      connections = Prelude.Nothing,
      workerType = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      nonOverridableArguments = Prelude.Nothing,
      logUri = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      executionProperty = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      timeout = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      role' = pRole_,
      command = pCommand_
    }

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
createJob_numberOfWorkers :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Int)
createJob_numberOfWorkers = Lens.lens (\CreateJob' {numberOfWorkers} -> numberOfWorkers) (\s@CreateJob' {} a -> s {numberOfWorkers = a} :: CreateJob)

-- | Specifies configuration properties of a job notification.
createJob_notificationProperty :: Lens.Lens' CreateJob (Prelude.Maybe NotificationProperty)
createJob_notificationProperty = Lens.lens (\CreateJob' {notificationProperty} -> notificationProperty) (\s@CreateJob' {} a -> s {notificationProperty = a} :: CreateJob)

-- | The connections used for this job.
createJob_connections :: Lens.Lens' CreateJob (Prelude.Maybe ConnectionsList)
createJob_connections = Lens.lens (\CreateJob' {connections} -> connections) (\s@CreateJob' {} a -> s {connections = a} :: CreateJob)

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
createJob_workerType :: Lens.Lens' CreateJob (Prelude.Maybe WorkerType)
createJob_workerType = Lens.lens (\CreateJob' {workerType} -> workerType) (\s@CreateJob' {} a -> s {workerType = a} :: CreateJob)

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job.
createJob_securityConfiguration :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_securityConfiguration = Lens.lens (\CreateJob' {securityConfiguration} -> securityConfiguration) (\s@CreateJob' {} a -> s {securityConfiguration = a} :: CreateJob)

-- | Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Jobs that are created without specifying a Glue version default to Glue
-- 0.9.
createJob_glueVersion :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_glueVersion = Lens.lens (\CreateJob' {glueVersion} -> glueVersion) (\s@CreateJob' {} a -> s {glueVersion = a} :: CreateJob)

-- | Non-overridable arguments for this job, specified as name-value pairs.
createJob_nonOverridableArguments :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_nonOverridableArguments = Lens.lens (\CreateJob' {nonOverridableArguments} -> nonOverridableArguments) (\s@CreateJob' {} a -> s {nonOverridableArguments = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | This field is reserved for future use.
createJob_logUri :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_logUri = Lens.lens (\CreateJob' {logUri} -> logUri) (\s@CreateJob' {} a -> s {logUri = a} :: CreateJob)

-- | The maximum number of times to retry this job if it fails.
createJob_maxRetries :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Int)
createJob_maxRetries = Lens.lens (\CreateJob' {maxRetries} -> maxRetries) (\s@CreateJob' {} a -> s {maxRetries = a} :: CreateJob)

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
createJob_executionProperty :: Lens.Lens' CreateJob (Prelude.Maybe ExecutionProperty)
createJob_executionProperty = Lens.lens (\CreateJob' {executionProperty} -> executionProperty) (\s@CreateJob' {} a -> s {executionProperty = a} :: CreateJob)

-- | This parameter is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) to allocate to this Job.
-- You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
createJob_allocatedCapacity :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Int)
createJob_allocatedCapacity = Lens.lens (\CreateJob' {allocatedCapacity} -> allocatedCapacity) (\s@CreateJob' {} a -> s {allocatedCapacity = a} :: CreateJob)

-- | For Glue version 1.0 or earlier jobs, using the standard worker type,
-- the number of Glue data processing units (DPUs) that can be allocated
-- when this job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
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
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
createJob_maxCapacity :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Double)
createJob_maxCapacity = Lens.lens (\CreateJob' {maxCapacity} -> maxCapacity) (\s@CreateJob' {} a -> s {maxCapacity = a} :: CreateJob)

-- | The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
createJob_timeout :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Natural)
createJob_timeout = Lens.lens (\CreateJob' {timeout} -> timeout) (\s@CreateJob' {} a -> s {timeout = a} :: CreateJob)

-- | The default arguments for this job.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own Job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
createJob_defaultArguments :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_defaultArguments = Lens.lens (\CreateJob' {defaultArguments} -> defaultArguments) (\s@CreateJob' {} a -> s {defaultArguments = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | Description of the job being defined.
createJob_description :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_description = Lens.lens (\CreateJob' {description} -> description) (\s@CreateJob' {} a -> s {description = a} :: CreateJob)

-- | The tags to use with this job. You may use tags to limit access to the
-- job. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
createJob_tags :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_tags = Lens.lens (\CreateJob' {tags} -> tags) (\s@CreateJob' {} a -> s {tags = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | The name you assign to this job definition. It must be unique in your
-- account.
createJob_name :: Lens.Lens' CreateJob Prelude.Text
createJob_name = Lens.lens (\CreateJob' {name} -> name) (\s@CreateJob' {} a -> s {name = a} :: CreateJob)

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
createJob_role :: Lens.Lens' CreateJob Prelude.Text
createJob_role = Lens.lens (\CreateJob' {role'} -> role') (\s@CreateJob' {} a -> s {role' = a} :: CreateJob)

-- | The @JobCommand@ that runs this job.
createJob_command :: Lens.Lens' CreateJob JobCommand
createJob_command = Lens.lens (\CreateJob' {command} -> command) (\s@CreateJob' {} a -> s {command = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob where
  hashWithSalt _salt CreateJob' {..} =
    _salt `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` notificationProperty
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` nonOverridableArguments
      `Prelude.hashWithSalt` logUri
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` executionProperty
      `Prelude.hashWithSalt` allocatedCapacity
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` defaultArguments
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` command

instance Prelude.NFData CreateJob where
  rnf CreateJob' {..} =
    Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf notificationProperty
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf nonOverridableArguments
      `Prelude.seq` Prelude.rnf logUri
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf executionProperty
      `Prelude.seq` Prelude.rnf allocatedCapacity
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf defaultArguments
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf command

instance Core.ToHeaders CreateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateJob" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NumberOfWorkers" Core..=)
              Prelude.<$> numberOfWorkers,
            ("NotificationProperty" Core..=)
              Prelude.<$> notificationProperty,
            ("Connections" Core..=) Prelude.<$> connections,
            ("WorkerType" Core..=) Prelude.<$> workerType,
            ("SecurityConfiguration" Core..=)
              Prelude.<$> securityConfiguration,
            ("GlueVersion" Core..=) Prelude.<$> glueVersion,
            ("NonOverridableArguments" Core..=)
              Prelude.<$> nonOverridableArguments,
            ("LogUri" Core..=) Prelude.<$> logUri,
            ("MaxRetries" Core..=) Prelude.<$> maxRetries,
            ("ExecutionProperty" Core..=)
              Prelude.<$> executionProperty,
            ("AllocatedCapacity" Core..=)
              Prelude.<$> allocatedCapacity,
            ("MaxCapacity" Core..=) Prelude.<$> maxCapacity,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("DefaultArguments" Core..=)
              Prelude.<$> defaultArguments,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Role" Core..= role'),
            Prelude.Just ("Command" Core..= command)
          ]
      )

instance Core.ToPath CreateJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The unique name that was provided for this job definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique name that was provided for this job definition.
createJobResponse_name :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_name = Lens.lens (\CreateJobResponse' {name} -> name) (\s@CreateJobResponse' {} a -> s {name = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse where
  rnf CreateJobResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
