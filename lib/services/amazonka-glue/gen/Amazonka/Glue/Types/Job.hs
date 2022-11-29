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
-- Module      : Amazonka.Glue.Types.Job
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.CodeGenConfigurationNode
import Amazonka.Glue.Types.ConnectionsList
import Amazonka.Glue.Types.ExecutionClass
import Amazonka.Glue.Types.ExecutionProperty
import Amazonka.Glue.Types.JobCommand
import Amazonka.Glue.Types.NotificationProperty
import Amazonka.Glue.Types.SourceControlDetails
import Amazonka.Glue.Types.WorkerType
import qualified Amazonka.Prelude as Prelude

-- | Specifies a job definition.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The time and date that this job definition was created.
    createdOn :: Prelude.Maybe Core.POSIX,
    -- | The job timeout in minutes. This is the maximum time that a job run can
    -- consume resources before it is terminated and enters @TIMEOUT@ status.
    -- The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The name you assign to this job definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The last point in time when this job definition was modified.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
    -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
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
    -- | Specifies configuration properties of a job notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The type of predefined worker that is allocated when a job runs. Accepts
    -- a value of Standard, G.1X, G.2X, or G.025X.
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
    -- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
    --     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for low volume streaming jobs. This
    --     worker type is only available for Glue version 3.0 streaming jobs.
    workerType :: Prelude.Maybe WorkerType,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
    -- allowed for this job.
    executionProperty :: Prelude.Maybe ExecutionProperty,
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of Glue data processing units (DPUs) allocated to runs of
    -- this job. You can allocate a minimum of 2 DPUs; the default is 10. A DPU
    -- is a relative measure of processing power that consists of 4 vCPUs of
    -- compute capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The @JobCommand@ that runs this job.
    command :: Prelude.Maybe JobCommand,
    -- | A description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry this job after a JobRun fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | The representation of a directed acyclic graph on which both the Glue
    -- Studio visual component and Glue Studio code generation is based.
    codeGenConfigurationNodes :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text CodeGenConfigurationNode)),
    -- | The default arguments for this job, specified as name-value pairs.
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
    -- | The details for a source control configuration for a job, allowing
    -- synchronization of job artifacts to or from a remote repository.
    sourceControlDetails :: Prelude.Maybe SourceControlDetails,
    -- | This field is reserved for future use.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The connections used for this job.
    connections :: Prelude.Maybe ConnectionsList,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- this job.
    role' :: Prelude.Maybe Prelude.Text,
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
    -- are running a Python shell job, an Apache Spark ETL job, or an Apache
    -- Spark streaming ETL job:
    --
    -- -   When you specify a Python shell job
    --     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
    --     or 1 DPU. The default is 0.0625 DPU.
    --
    -- -   When you specify an Apache Spark ETL job
    --     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
    --     (@JobCommand.Name@=\"gluestreaming\"), you can allocate a minimum of
    --     2 DPUs. The default is 10 DPUs. This job type cannot have a
    --     fractional DPU allocation.
    --
    -- For Glue version 2.0 jobs, you cannot instead specify a
    -- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
    -- @Number of workers@.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether the job is run with a standard or flexible execution
    -- class. The standard execution class is ideal for time-sensitive
    -- workloads that require fast job startup and dedicated resources.
    --
    -- The flexible execution class is appropriate for time-insensitive jobs
    -- whose start and completion times may vary.
    --
    -- Only jobs with Glue version 3.0 and above and command type @glueetl@
    -- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
    -- execution class is available for Spark jobs.
    executionClass :: Prelude.Maybe ExecutionClass
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'job_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job.
--
-- 'createdOn', 'job_createdOn' - The time and date that this job definition was created.
--
-- 'timeout', 'job_timeout' - The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
--
-- 'name', 'job_name' - The name you assign to this job definition.
--
-- 'lastModifiedOn', 'job_lastModifiedOn' - The last point in time when this job definition was modified.
--
-- 'nonOverridableArguments', 'job_nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- 'numberOfWorkers', 'job_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- 'glueVersion', 'job_glueVersion' - Glue version determines the versions of Apache Spark and Python that
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
-- 'notificationProperty', 'job_notificationProperty' - Specifies configuration properties of a job notification.
--
-- 'workerType', 'job_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, G.2X, or G.025X.
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
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
--
-- 'executionProperty', 'job_executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
--
-- 'allocatedCapacity', 'job_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to runs of
-- this job. You can allocate a minimum of 2 DPUs; the default is 10. A DPU
-- is a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- 'command', 'job_command' - The @JobCommand@ that runs this job.
--
-- 'description', 'job_description' - A description of the job.
--
-- 'maxRetries', 'job_maxRetries' - The maximum number of times to retry this job after a JobRun fails.
--
-- 'codeGenConfigurationNodes', 'job_codeGenConfigurationNodes' - The representation of a directed acyclic graph on which both the Glue
-- Studio visual component and Glue Studio code generation is based.
--
-- 'defaultArguments', 'job_defaultArguments' - The default arguments for this job, specified as name-value pairs.
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
-- 'sourceControlDetails', 'job_sourceControlDetails' - The details for a source control configuration for a job, allowing
-- synchronization of job artifacts to or from a remote repository.
--
-- 'logUri', 'job_logUri' - This field is reserved for future use.
--
-- 'connections', 'job_connections' - The connections used for this job.
--
-- 'role'', 'job_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
--
-- 'maxCapacity', 'job_maxCapacity' - For Glue version 1.0 or earlier jobs, using the standard worker type,
-- the number of Glue data processing units (DPUs) that can be allocated
-- when this job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@.
--
-- The value that can be allocated for @MaxCapacity@ depends on whether you
-- are running a Python shell job, an Apache Spark ETL job, or an Apache
-- Spark streaming ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate a minimum of
--     2 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
--
-- 'executionClass', 'job_executionClass' - Indicates whether the job is run with a standard or flexible execution
-- class. The standard execution class is ideal for time-sensitive
-- workloads that require fast job startup and dedicated resources.
--
-- The flexible execution class is appropriate for time-insensitive jobs
-- whose start and completion times may vary.
--
-- Only jobs with Glue version 3.0 and above and command type @glueetl@
-- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
-- execution class is available for Spark jobs.
newJob ::
  Job
newJob =
  Job'
    { securityConfiguration = Prelude.Nothing,
      createdOn = Prelude.Nothing,
      timeout = Prelude.Nothing,
      name = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      nonOverridableArguments = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      workerType = Prelude.Nothing,
      executionProperty = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      command = Prelude.Nothing,
      description = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      codeGenConfigurationNodes = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      sourceControlDetails = Prelude.Nothing,
      logUri = Prelude.Nothing,
      connections = Prelude.Nothing,
      role' = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      executionClass = Prelude.Nothing
    }

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job.
job_securityConfiguration :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_securityConfiguration = Lens.lens (\Job' {securityConfiguration} -> securityConfiguration) (\s@Job' {} a -> s {securityConfiguration = a} :: Job)

-- | The time and date that this job definition was created.
job_createdOn :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createdOn = Lens.lens (\Job' {createdOn} -> createdOn) (\s@Job' {} a -> s {createdOn = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
job_timeout :: Lens.Lens' Job (Prelude.Maybe Prelude.Natural)
job_timeout = Lens.lens (\Job' {timeout} -> timeout) (\s@Job' {} a -> s {timeout = a} :: Job)

-- | The name you assign to this job definition.
job_name :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_name = Lens.lens (\Job' {name} -> name) (\s@Job' {} a -> s {name = a} :: Job)

-- | The last point in time when this job definition was modified.
job_lastModifiedOn :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastModifiedOn = Lens.lens (\Job' {lastModifiedOn} -> lastModifiedOn) (\s@Job' {} a -> s {lastModifiedOn = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | Non-overridable arguments for this job, specified as name-value pairs.
job_nonOverridableArguments :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_nonOverridableArguments = Lens.lens (\Job' {nonOverridableArguments} -> nonOverridableArguments) (\s@Job' {} a -> s {nonOverridableArguments = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
job_numberOfWorkers :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_numberOfWorkers = Lens.lens (\Job' {numberOfWorkers} -> numberOfWorkers) (\s@Job' {} a -> s {numberOfWorkers = a} :: Job)

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
job_glueVersion :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_glueVersion = Lens.lens (\Job' {glueVersion} -> glueVersion) (\s@Job' {} a -> s {glueVersion = a} :: Job)

-- | Specifies configuration properties of a job notification.
job_notificationProperty :: Lens.Lens' Job (Prelude.Maybe NotificationProperty)
job_notificationProperty = Lens.lens (\Job' {notificationProperty} -> notificationProperty) (\s@Job' {} a -> s {notificationProperty = a} :: Job)

-- | The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, G.2X, or G.025X.
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
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
job_workerType :: Lens.Lens' Job (Prelude.Maybe WorkerType)
job_workerType = Lens.lens (\Job' {workerType} -> workerType) (\s@Job' {} a -> s {workerType = a} :: Job)

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
job_executionProperty :: Lens.Lens' Job (Prelude.Maybe ExecutionProperty)
job_executionProperty = Lens.lens (\Job' {executionProperty} -> executionProperty) (\s@Job' {} a -> s {executionProperty = a} :: Job)

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to runs of
-- this job. You can allocate a minimum of 2 DPUs; the default is 10. A DPU
-- is a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
job_allocatedCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_allocatedCapacity = Lens.lens (\Job' {allocatedCapacity} -> allocatedCapacity) (\s@Job' {} a -> s {allocatedCapacity = a} :: Job)

-- | The @JobCommand@ that runs this job.
job_command :: Lens.Lens' Job (Prelude.Maybe JobCommand)
job_command = Lens.lens (\Job' {command} -> command) (\s@Job' {} a -> s {command = a} :: Job)

-- | A description of the job.
job_description :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_description = Lens.lens (\Job' {description} -> description) (\s@Job' {} a -> s {description = a} :: Job)

-- | The maximum number of times to retry this job after a JobRun fails.
job_maxRetries :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_maxRetries = Lens.lens (\Job' {maxRetries} -> maxRetries) (\s@Job' {} a -> s {maxRetries = a} :: Job)

-- | The representation of a directed acyclic graph on which both the Glue
-- Studio visual component and Glue Studio code generation is based.
job_codeGenConfigurationNodes :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text CodeGenConfigurationNode))
job_codeGenConfigurationNodes = Lens.lens (\Job' {codeGenConfigurationNodes} -> codeGenConfigurationNodes) (\s@Job' {} a -> s {codeGenConfigurationNodes = a} :: Job) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The default arguments for this job, specified as name-value pairs.
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
job_defaultArguments :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_defaultArguments = Lens.lens (\Job' {defaultArguments} -> defaultArguments) (\s@Job' {} a -> s {defaultArguments = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The details for a source control configuration for a job, allowing
-- synchronization of job artifacts to or from a remote repository.
job_sourceControlDetails :: Lens.Lens' Job (Prelude.Maybe SourceControlDetails)
job_sourceControlDetails = Lens.lens (\Job' {sourceControlDetails} -> sourceControlDetails) (\s@Job' {} a -> s {sourceControlDetails = a} :: Job)

-- | This field is reserved for future use.
job_logUri :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_logUri = Lens.lens (\Job' {logUri} -> logUri) (\s@Job' {} a -> s {logUri = a} :: Job)

-- | The connections used for this job.
job_connections :: Lens.Lens' Job (Prelude.Maybe ConnectionsList)
job_connections = Lens.lens (\Job' {connections} -> connections) (\s@Job' {} a -> s {connections = a} :: Job)

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
job_role :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_role = Lens.lens (\Job' {role'} -> role') (\s@Job' {} a -> s {role' = a} :: Job)

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
-- are running a Python shell job, an Apache Spark ETL job, or an Apache
-- Spark streaming ETL job:
--
-- -   When you specify a Python shell job
--     (@JobCommand.Name@=\"pythonshell\"), you can allocate either 0.0625
--     or 1 DPU. The default is 0.0625 DPU.
--
-- -   When you specify an Apache Spark ETL job
--     (@JobCommand.Name@=\"glueetl\") or Apache Spark streaming ETL job
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate a minimum of
--     2 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- For Glue version 2.0 jobs, you cannot instead specify a
-- @Maximum capacity@. Instead, you should specify a @Worker type@ and the
-- @Number of workers@.
job_maxCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Double)
job_maxCapacity = Lens.lens (\Job' {maxCapacity} -> maxCapacity) (\s@Job' {} a -> s {maxCapacity = a} :: Job)

-- | Indicates whether the job is run with a standard or flexible execution
-- class. The standard execution class is ideal for time-sensitive
-- workloads that require fast job startup and dedicated resources.
--
-- The flexible execution class is appropriate for time-insensitive jobs
-- whose start and completion times may vary.
--
-- Only jobs with Glue version 3.0 and above and command type @glueetl@
-- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
-- execution class is available for Spark jobs.
job_executionClass :: Lens.Lens' Job (Prelude.Maybe ExecutionClass)
job_executionClass = Lens.lens (\Job' {executionClass} -> executionClass) (\s@Job' {} a -> s {executionClass = a} :: Job)

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Core..:? "SecurityConfiguration")
            Prelude.<*> (x Core..:? "CreatedOn")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LastModifiedOn")
            Prelude.<*> ( x Core..:? "NonOverridableArguments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "NumberOfWorkers")
            Prelude.<*> (x Core..:? "GlueVersion")
            Prelude.<*> (x Core..:? "NotificationProperty")
            Prelude.<*> (x Core..:? "WorkerType")
            Prelude.<*> (x Core..:? "ExecutionProperty")
            Prelude.<*> (x Core..:? "AllocatedCapacity")
            Prelude.<*> (x Core..:? "Command")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "MaxRetries")
            Prelude.<*> ( x Core..:? "CodeGenConfigurationNodes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "DefaultArguments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SourceControlDetails")
            Prelude.<*> (x Core..:? "LogUri")
            Prelude.<*> (x Core..:? "Connections")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "MaxCapacity")
            Prelude.<*> (x Core..:? "ExecutionClass")
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` nonOverridableArguments
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` notificationProperty
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` executionProperty
      `Prelude.hashWithSalt` allocatedCapacity
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` codeGenConfigurationNodes
      `Prelude.hashWithSalt` defaultArguments
      `Prelude.hashWithSalt` sourceControlDetails
      `Prelude.hashWithSalt` logUri
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` executionClass

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf nonOverridableArguments
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf notificationProperty
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf executionProperty
      `Prelude.seq` Prelude.rnf allocatedCapacity
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf codeGenConfigurationNodes
      `Prelude.seq` Prelude.rnf defaultArguments
      `Prelude.seq` Prelude.rnf sourceControlDetails
      `Prelude.seq` Prelude.rnf logUri
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf
        executionClass
