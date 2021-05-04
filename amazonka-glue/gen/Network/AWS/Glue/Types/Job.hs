{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Job where

import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a job definition.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time and date that this job definition was created.
    createdOn :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The job timeout in minutes. This is the maximum time that a job run can
    -- consume resources before it is terminated and enters @TIMEOUT@ status.
    -- The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The number of AWS Glue data processing units (DPUs) that can be
    -- allocated when this job runs. A DPU is a relative measure of processing
    -- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
    -- For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
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
    --     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
    --     100 DPUs. The default is 10 DPUs. This job type cannot have a
    --     fractional DPU allocation.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The connections used for this job.
    connections :: Prelude.Maybe ConnectionsList,
    -- | Specifies configuration properties of a job notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The last point in time when this job definition was modified.
    lastModifiedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The @JobCommand@ that executes this job.
    command :: Prelude.Maybe JobCommand,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The name you assign to this job definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- this job.
    role' :: Prelude.Maybe Prelude.Text,
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
    -- | A description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The default arguments for this job, specified as name-value pairs.
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
    defaultArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) allocated to runs of
    -- this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU
    -- is a relative measure of processing power that consists of 4 vCPUs of
    -- compute capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
    -- allowed for this job.
    executionProperty :: Prelude.Maybe ExecutionProperty,
    -- | The maximum number of times to retry this job after a JobRun fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | This field is reserved for future use.
    logUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonOverridableArguments', 'job_nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
--
-- 'createdOn', 'job_createdOn' - The time and date that this job definition was created.
--
-- 'securityConfiguration', 'job_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job.
--
-- 'timeout', 'job_timeout' - The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
--
-- 'maxCapacity', 'job_maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
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
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
--     100 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
--
-- 'connections', 'job_connections' - The connections used for this job.
--
-- 'notificationProperty', 'job_notificationProperty' - Specifies configuration properties of a job notification.
--
-- 'lastModifiedOn', 'job_lastModifiedOn' - The last point in time when this job definition was modified.
--
-- 'command', 'job_command' - The @JobCommand@ that executes this job.
--
-- 'numberOfWorkers', 'job_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'name', 'job_name' - The name you assign to this job definition.
--
-- 'role'', 'job_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
--
-- 'glueVersion', 'job_glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS
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
-- 'workerType', 'job_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
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
-- 'description', 'job_description' - A description of the job.
--
-- 'defaultArguments', 'job_defaultArguments' - The default arguments for this job, specified as name-value pairs.
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
-- 'allocatedCapacity', 'job_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to runs of
-- this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU
-- is a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- 'executionProperty', 'job_executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
--
-- 'maxRetries', 'job_maxRetries' - The maximum number of times to retry this job after a JobRun fails.
--
-- 'logUri', 'job_logUri' - This field is reserved for future use.
newJob ::
  Job
newJob =
  Job'
    { nonOverridableArguments = Prelude.Nothing,
      createdOn = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      timeout = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      connections = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      command = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      name = Prelude.Nothing,
      role' = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      workerType = Prelude.Nothing,
      description = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      executionProperty = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      logUri = Prelude.Nothing
    }

-- | Non-overridable arguments for this job, specified as name-value pairs.
job_nonOverridableArguments :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_nonOverridableArguments = Lens.lens (\Job' {nonOverridableArguments} -> nonOverridableArguments) (\s@Job' {} a -> s {nonOverridableArguments = a} :: Job) Prelude.. Lens.mapping Prelude._Coerce

-- | The time and date that this job definition was created.
job_createdOn :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createdOn = Lens.lens (\Job' {createdOn} -> createdOn) (\s@Job' {} a -> s {createdOn = a} :: Job) Prelude.. Lens.mapping Prelude._Time

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job.
job_securityConfiguration :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_securityConfiguration = Lens.lens (\Job' {securityConfiguration} -> securityConfiguration) (\s@Job' {} a -> s {securityConfiguration = a} :: Job)

-- | The job timeout in minutes. This is the maximum time that a job run can
-- consume resources before it is terminated and enters @TIMEOUT@ status.
-- The default is 2,880 minutes (48 hours).
job_timeout :: Lens.Lens' Job (Prelude.Maybe Prelude.Natural)
job_timeout = Lens.lens (\Job' {timeout} -> timeout) (\s@Job' {} a -> s {timeout = a} :: Job)

-- | The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
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
--     (@JobCommand.Name@=\"gluestreaming\"), you can allocate from 2 to
--     100 DPUs. The default is 10 DPUs. This job type cannot have a
--     fractional DPU allocation.
job_maxCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Double)
job_maxCapacity = Lens.lens (\Job' {maxCapacity} -> maxCapacity) (\s@Job' {} a -> s {maxCapacity = a} :: Job)

-- | The connections used for this job.
job_connections :: Lens.Lens' Job (Prelude.Maybe ConnectionsList)
job_connections = Lens.lens (\Job' {connections} -> connections) (\s@Job' {} a -> s {connections = a} :: Job)

-- | Specifies configuration properties of a job notification.
job_notificationProperty :: Lens.Lens' Job (Prelude.Maybe NotificationProperty)
job_notificationProperty = Lens.lens (\Job' {notificationProperty} -> notificationProperty) (\s@Job' {} a -> s {notificationProperty = a} :: Job)

-- | The last point in time when this job definition was modified.
job_lastModifiedOn :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastModifiedOn = Lens.lens (\Job' {lastModifiedOn} -> lastModifiedOn) (\s@Job' {} a -> s {lastModifiedOn = a} :: Job) Prelude.. Lens.mapping Prelude._Time

-- | The @JobCommand@ that executes this job.
job_command :: Lens.Lens' Job (Prelude.Maybe JobCommand)
job_command = Lens.lens (\Job' {command} -> command) (\s@Job' {} a -> s {command = a} :: Job)

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
job_numberOfWorkers :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_numberOfWorkers = Lens.lens (\Job' {numberOfWorkers} -> numberOfWorkers) (\s@Job' {} a -> s {numberOfWorkers = a} :: Job)

-- | The name you assign to this job definition.
job_name :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_name = Lens.lens (\Job' {name} -> name) (\s@Job' {} a -> s {name = a} :: Job)

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- this job.
job_role :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_role = Lens.lens (\Job' {role'} -> role') (\s@Job' {} a -> s {role' = a} :: Job)

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
job_glueVersion :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_glueVersion = Lens.lens (\Job' {glueVersion} -> glueVersion) (\s@Job' {} a -> s {glueVersion = a} :: Job)

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
job_workerType :: Lens.Lens' Job (Prelude.Maybe WorkerType)
job_workerType = Lens.lens (\Job' {workerType} -> workerType) (\s@Job' {} a -> s {workerType = a} :: Job)

-- | A description of the job.
job_description :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_description = Lens.lens (\Job' {description} -> description) (\s@Job' {} a -> s {description = a} :: Job)

-- | The default arguments for this job, specified as name-value pairs.
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
job_defaultArguments :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_defaultArguments = Lens.lens (\Job' {defaultArguments} -> defaultArguments) (\s@Job' {} a -> s {defaultArguments = a} :: Job) Prelude.. Lens.mapping Prelude._Coerce

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to runs of
-- this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU
-- is a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
job_allocatedCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_allocatedCapacity = Lens.lens (\Job' {allocatedCapacity} -> allocatedCapacity) (\s@Job' {} a -> s {allocatedCapacity = a} :: Job)

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs
-- allowed for this job.
job_executionProperty :: Lens.Lens' Job (Prelude.Maybe ExecutionProperty)
job_executionProperty = Lens.lens (\Job' {executionProperty} -> executionProperty) (\s@Job' {} a -> s {executionProperty = a} :: Job)

-- | The maximum number of times to retry this job after a JobRun fails.
job_maxRetries :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_maxRetries = Lens.lens (\Job' {maxRetries} -> maxRetries) (\s@Job' {} a -> s {maxRetries = a} :: Job)

-- | This field is reserved for future use.
job_logUri :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_logUri = Lens.lens (\Job' {logUri} -> logUri) (\s@Job' {} a -> s {logUri = a} :: Job)

instance Prelude.FromJSON Job where
  parseJSON =
    Prelude.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> ( x Prelude..:? "NonOverridableArguments"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CreatedOn")
            Prelude.<*> (x Prelude..:? "SecurityConfiguration")
            Prelude.<*> (x Prelude..:? "Timeout")
            Prelude.<*> (x Prelude..:? "MaxCapacity")
            Prelude.<*> (x Prelude..:? "Connections")
            Prelude.<*> (x Prelude..:? "NotificationProperty")
            Prelude.<*> (x Prelude..:? "LastModifiedOn")
            Prelude.<*> (x Prelude..:? "Command")
            Prelude.<*> (x Prelude..:? "NumberOfWorkers")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Role")
            Prelude.<*> (x Prelude..:? "GlueVersion")
            Prelude.<*> (x Prelude..:? "WorkerType")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> ( x Prelude..:? "DefaultArguments"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AllocatedCapacity")
            Prelude.<*> (x Prelude..:? "ExecutionProperty")
            Prelude.<*> (x Prelude..:? "MaxRetries")
            Prelude.<*> (x Prelude..:? "LogUri")
      )

instance Prelude.Hashable Job

instance Prelude.NFData Job
