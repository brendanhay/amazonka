{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies information used to update an existing job definition. The
-- previous job definition is completely overwritten by this information.
--
-- /See:/ 'newJobUpdate' smart constructor.
data JobUpdate = JobUpdate'
  { -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The job timeout in minutes. This is the maximum time that a job run can
    -- consume resources before it is terminated and enters @TIMEOUT@ status.
    -- The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
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
    -- | The connections used for this job.
    connections :: Prelude.Maybe ConnectionsList,
    -- | Specifies the configuration properties of a job notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The @JobCommand@ that runs this job (required).
    command :: Prelude.Maybe JobCommand,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- this job (required).
    role' :: Prelude.Maybe Prelude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The Python version indicates the version supported for
    -- jobs of type Spark.
    --
    -- For more information about the available Glue versions and corresponding
    -- Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
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
    -- | Description of the job being defined.
    description :: Prelude.Maybe Prelude.Text,
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
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
    -- allowed for this job.
    executionProperty :: Prelude.Maybe ExecutionProperty,
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of Glue data processing units (DPUs) to allocate to this job.
    -- You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
    -- relative measure of processing power that consists of 4 vCPUs of compute
    -- capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | This field is reserved for future use.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry this job if it fails.
    maxRetries :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonOverridableArguments', 'jobUpdate_nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- 'securityConfiguration', 'jobUpdate_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job.
--
-- 'timeout', 'jobUpdate_timeout' - The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
--
-- 'maxCapacity', 'jobUpdate_maxCapacity' - For Glue version 1.0 or earlier jobs, using the standard worker type,
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
-- 'connections', 'jobUpdate_connections' - The connections used for this job.
--
-- 'notificationProperty', 'jobUpdate_notificationProperty' - Specifies the configuration properties of a job notification.
--
-- 'numberOfWorkers', 'jobUpdate_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'command', 'jobUpdate_command' - The @JobCommand@ that runs this job (required).
--
-- 'role'', 'jobUpdate_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job (required).
--
-- 'glueVersion', 'jobUpdate_glueVersion' - Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- 'workerType', 'jobUpdate_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
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
-- 'description', 'jobUpdate_description' - Description of the job being defined.
--
-- 'defaultArguments', 'jobUpdate_defaultArguments' - The default arguments for this job.
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
-- 'executionProperty', 'jobUpdate_executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
--
-- 'allocatedCapacity', 'jobUpdate_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) to allocate to this job.
-- You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- 'logUri', 'jobUpdate_logUri' - This field is reserved for future use.
--
-- 'maxRetries', 'jobUpdate_maxRetries' - The maximum number of times to retry this job if it fails.
newJobUpdate ::
  JobUpdate
newJobUpdate =
  JobUpdate'
    { nonOverridableArguments =
        Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      timeout = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      connections = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      command = Prelude.Nothing,
      role' = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      workerType = Prelude.Nothing,
      description = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      executionProperty = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      logUri = Prelude.Nothing,
      maxRetries = Prelude.Nothing
    }

-- | Non-overridable arguments for this job, specified as name-value pairs.
jobUpdate_nonOverridableArguments :: Lens.Lens' JobUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobUpdate_nonOverridableArguments = Lens.lens (\JobUpdate' {nonOverridableArguments} -> nonOverridableArguments) (\s@JobUpdate' {} a -> s {nonOverridableArguments = a} :: JobUpdate) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job.
jobUpdate_securityConfiguration :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_securityConfiguration = Lens.lens (\JobUpdate' {securityConfiguration} -> securityConfiguration) (\s@JobUpdate' {} a -> s {securityConfiguration = a} :: JobUpdate)

-- | The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
jobUpdate_timeout :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Natural)
jobUpdate_timeout = Lens.lens (\JobUpdate' {timeout} -> timeout) (\s@JobUpdate' {} a -> s {timeout = a} :: JobUpdate)

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
jobUpdate_maxCapacity :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Double)
jobUpdate_maxCapacity = Lens.lens (\JobUpdate' {maxCapacity} -> maxCapacity) (\s@JobUpdate' {} a -> s {maxCapacity = a} :: JobUpdate)

-- | The connections used for this job.
jobUpdate_connections :: Lens.Lens' JobUpdate (Prelude.Maybe ConnectionsList)
jobUpdate_connections = Lens.lens (\JobUpdate' {connections} -> connections) (\s@JobUpdate' {} a -> s {connections = a} :: JobUpdate)

-- | Specifies the configuration properties of a job notification.
jobUpdate_notificationProperty :: Lens.Lens' JobUpdate (Prelude.Maybe NotificationProperty)
jobUpdate_notificationProperty = Lens.lens (\JobUpdate' {notificationProperty} -> notificationProperty) (\s@JobUpdate' {} a -> s {notificationProperty = a} :: JobUpdate)

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
jobUpdate_numberOfWorkers :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Int)
jobUpdate_numberOfWorkers = Lens.lens (\JobUpdate' {numberOfWorkers} -> numberOfWorkers) (\s@JobUpdate' {} a -> s {numberOfWorkers = a} :: JobUpdate)

-- | The @JobCommand@ that runs this job (required).
jobUpdate_command :: Lens.Lens' JobUpdate (Prelude.Maybe JobCommand)
jobUpdate_command = Lens.lens (\JobUpdate' {command} -> command) (\s@JobUpdate' {} a -> s {command = a} :: JobUpdate)

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job (required).
jobUpdate_role :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_role = Lens.lens (\JobUpdate' {role'} -> role') (\s@JobUpdate' {} a -> s {role' = a} :: JobUpdate)

-- | Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- jobs of type Spark.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
jobUpdate_glueVersion :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_glueVersion = Lens.lens (\JobUpdate' {glueVersion} -> glueVersion) (\s@JobUpdate' {} a -> s {glueVersion = a} :: JobUpdate)

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
jobUpdate_workerType :: Lens.Lens' JobUpdate (Prelude.Maybe WorkerType)
jobUpdate_workerType = Lens.lens (\JobUpdate' {workerType} -> workerType) (\s@JobUpdate' {} a -> s {workerType = a} :: JobUpdate)

-- | Description of the job being defined.
jobUpdate_description :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_description = Lens.lens (\JobUpdate' {description} -> description) (\s@JobUpdate' {} a -> s {description = a} :: JobUpdate)

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
jobUpdate_defaultArguments :: Lens.Lens' JobUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobUpdate_defaultArguments = Lens.lens (\JobUpdate' {defaultArguments} -> defaultArguments) (\s@JobUpdate' {} a -> s {defaultArguments = a} :: JobUpdate) Prelude.. Lens.mapping Lens._Coerce

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
jobUpdate_executionProperty :: Lens.Lens' JobUpdate (Prelude.Maybe ExecutionProperty)
jobUpdate_executionProperty = Lens.lens (\JobUpdate' {executionProperty} -> executionProperty) (\s@JobUpdate' {} a -> s {executionProperty = a} :: JobUpdate)

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) to allocate to this job.
-- You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a
-- relative measure of processing power that consists of 4 vCPUs of compute
-- capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
jobUpdate_allocatedCapacity :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Int)
jobUpdate_allocatedCapacity = Lens.lens (\JobUpdate' {allocatedCapacity} -> allocatedCapacity) (\s@JobUpdate' {} a -> s {allocatedCapacity = a} :: JobUpdate)

-- | This field is reserved for future use.
jobUpdate_logUri :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Text)
jobUpdate_logUri = Lens.lens (\JobUpdate' {logUri} -> logUri) (\s@JobUpdate' {} a -> s {logUri = a} :: JobUpdate)

-- | The maximum number of times to retry this job if it fails.
jobUpdate_maxRetries :: Lens.Lens' JobUpdate (Prelude.Maybe Prelude.Int)
jobUpdate_maxRetries = Lens.lens (\JobUpdate' {maxRetries} -> maxRetries) (\s@JobUpdate' {} a -> s {maxRetries = a} :: JobUpdate)

instance Prelude.Hashable JobUpdate

instance Prelude.NFData JobUpdate

instance Core.ToJSON JobUpdate where
  toJSON JobUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NonOverridableArguments" Core..=)
              Prelude.<$> nonOverridableArguments,
            ("SecurityConfiguration" Core..=)
              Prelude.<$> securityConfiguration,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("MaxCapacity" Core..=) Prelude.<$> maxCapacity,
            ("Connections" Core..=) Prelude.<$> connections,
            ("NotificationProperty" Core..=)
              Prelude.<$> notificationProperty,
            ("NumberOfWorkers" Core..=)
              Prelude.<$> numberOfWorkers,
            ("Command" Core..=) Prelude.<$> command,
            ("Role" Core..=) Prelude.<$> role',
            ("GlueVersion" Core..=) Prelude.<$> glueVersion,
            ("WorkerType" Core..=) Prelude.<$> workerType,
            ("Description" Core..=) Prelude.<$> description,
            ("DefaultArguments" Core..=)
              Prelude.<$> defaultArguments,
            ("ExecutionProperty" Core..=)
              Prelude.<$> executionProperty,
            ("AllocatedCapacity" Core..=)
              Prelude.<$> allocatedCapacity,
            ("LogUri" Core..=) Prelude.<$> logUri,
            ("MaxRetries" Core..=) Prelude.<$> maxRetries
          ]
      )
