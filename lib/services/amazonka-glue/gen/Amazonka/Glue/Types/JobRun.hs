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
-- Module      : Amazonka.Glue.Types.JobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JobRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ExecutionClass
import Amazonka.Glue.Types.JobRunState
import Amazonka.Glue.Types.NotificationProperty
import Amazonka.Glue.Types.Predecessor
import Amazonka.Glue.Types.WorkerType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a job run.
--
-- /See:/ 'newJobRun' smart constructor.
data JobRun = JobRun'
  { -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of Glue data processing units (DPUs) allocated to this
    -- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
    -- a relative measure of processing power that consists of 4 vCPUs of
    -- compute capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The job arguments associated with this run. For this job run, they
    -- replace the default arguments set in the job definition itself.
    --
    -- You can specify arguments here that your own job-execution script
    -- consumes, as well as arguments that Glue itself consumes.
    --
    -- For information about how to specify and consume your own job arguments,
    -- see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
    -- topic in the developer guide.
    --
    -- For information about the key-value pairs that Glue consumes to set up
    -- your job, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
    -- topic in the developer guide.
    arguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of the attempt to run this job.
    attempt :: Prelude.Maybe Prelude.Int,
    -- | The date and time that this job run completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | This field populates only for Auto Scaling job runs, and represents the
    -- total time each executor ran during the lifecycle of a job run in
    -- seconds, multiplied by a DPU factor (1 for @G.1X@, 2 for @G.2X@, or 0.25
    -- for @G.025X@ workers). This value may be different than the
    -- @executionEngineRuntime@ * @MaxCapacity@ as in the case of Auto Scaling
    -- jobs, as the number of executors running at a given time may be less
    -- than the @MaxCapacity@. Therefore, it is possible that the value of
    -- @DPUSeconds@ is less than @executionEngineRuntime@ * @MaxCapacity@.
    dPUSeconds :: Prelude.Maybe Prelude.Double,
    -- | An error message associated with this job run.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the job is run with a standard or flexible execution
    -- class. The standard execution-class is ideal for time-sensitive
    -- workloads that require fast job startup and dedicated resources.
    --
    -- The flexible execution class is appropriate for time-insensitive jobs
    -- whose start and completion times may vary.
    --
    -- Only jobs with Glue version 3.0 and above and command type @glueetl@
    -- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
    -- execution class is available for Spark jobs.
    executionClass :: Prelude.Maybe ExecutionClass,
    -- | The amount of time (in seconds) that the job run consumed resources.
    executionTime :: Prelude.Maybe Prelude.Int,
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
    -- | The ID of this job run.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the job definition being used in this run.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the job run. For more information about the
    -- statuses of jobs that have terminated abnormally, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html Glue Job Run Statuses>.
    jobRunState :: Prelude.Maybe JobRunState,
    -- | The last time that this job run was modified.
    lastModifiedOn :: Prelude.Maybe Data.POSIX,
    -- | The name of the log group for secure logging that can be server-side
    -- encrypted in Amazon CloudWatch using KMS. This name can be
    -- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
    -- you add a role name and @SecurityConfiguration@ name (in other words,
    -- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
    -- that security configuration is used to encrypt the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The number of Glue data processing units (DPUs) that can be allocated
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
    --     (@JobCommand.Name@=\"glueetl\"), you can allocate a minimum of 2
    --     DPUs. The default is 10 DPUs. This job type cannot have a fractional
    --     DPU allocation.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | Specifies configuration properties of a job run notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | A list of predecessors to this job run.
    predecessorRuns :: Prelude.Maybe [Predecessor],
    -- | The ID of the previous run of this job. For example, the @JobRunId@
    -- specified in the @StartJobRun@ action.
    previousRunId :: Prelude.Maybe Prelude.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job run.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which this job run was started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. This value overrides the timeout value set in the parent job.
    --
    -- Streaming jobs do not have a timeout. The default for non-streaming jobs
    -- is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The name of the trigger that started this job run.
    triggerName :: Prelude.Maybe Prelude.Text,
    -- | The type of predefined worker that is allocated when a job runs. Accepts
    -- a value of Standard, G.1X, G.2X, or G.025X.
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
    -- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
    --     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for low volume streaming jobs. This
    --     worker type is only available for Glue version 3.0 streaming jobs.
    workerType :: Prelude.Maybe WorkerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedCapacity', 'jobRun_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- 'arguments', 'jobRun_arguments' - The job arguments associated with this run. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
--
-- 'attempt', 'jobRun_attempt' - The number of the attempt to run this job.
--
-- 'completedOn', 'jobRun_completedOn' - The date and time that this job run completed.
--
-- 'dPUSeconds', 'jobRun_dPUSeconds' - This field populates only for Auto Scaling job runs, and represents the
-- total time each executor ran during the lifecycle of a job run in
-- seconds, multiplied by a DPU factor (1 for @G.1X@, 2 for @G.2X@, or 0.25
-- for @G.025X@ workers). This value may be different than the
-- @executionEngineRuntime@ * @MaxCapacity@ as in the case of Auto Scaling
-- jobs, as the number of executors running at a given time may be less
-- than the @MaxCapacity@. Therefore, it is possible that the value of
-- @DPUSeconds@ is less than @executionEngineRuntime@ * @MaxCapacity@.
--
-- 'errorMessage', 'jobRun_errorMessage' - An error message associated with this job run.
--
-- 'executionClass', 'jobRun_executionClass' - Indicates whether the job is run with a standard or flexible execution
-- class. The standard execution-class is ideal for time-sensitive
-- workloads that require fast job startup and dedicated resources.
--
-- The flexible execution class is appropriate for time-insensitive jobs
-- whose start and completion times may vary.
--
-- Only jobs with Glue version 3.0 and above and command type @glueetl@
-- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
-- execution class is available for Spark jobs.
--
-- 'executionTime', 'jobRun_executionTime' - The amount of time (in seconds) that the job run consumed resources.
--
-- 'glueVersion', 'jobRun_glueVersion' - Glue version determines the versions of Apache Spark and Python that
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
-- 'id', 'jobRun_id' - The ID of this job run.
--
-- 'jobName', 'jobRun_jobName' - The name of the job definition being used in this run.
--
-- 'jobRunState', 'jobRun_jobRunState' - The current state of the job run. For more information about the
-- statuses of jobs that have terminated abnormally, see
-- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html Glue Job Run Statuses>.
--
-- 'lastModifiedOn', 'jobRun_lastModifiedOn' - The last time that this job run was modified.
--
-- 'logGroupName', 'jobRun_logGroupName' - The name of the log group for secure logging that can be server-side
-- encrypted in Amazon CloudWatch using KMS. This name can be
-- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
-- you add a role name and @SecurityConfiguration@ name (in other words,
-- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
-- that security configuration is used to encrypt the log group.
--
-- 'maxCapacity', 'jobRun_maxCapacity' - The number of Glue data processing units (DPUs) that can be allocated
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
--     (@JobCommand.Name@=\"glueetl\"), you can allocate a minimum of 2
--     DPUs. The default is 10 DPUs. This job type cannot have a fractional
--     DPU allocation.
--
-- 'notificationProperty', 'jobRun_notificationProperty' - Specifies configuration properties of a job run notification.
--
-- 'numberOfWorkers', 'jobRun_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- 'predecessorRuns', 'jobRun_predecessorRuns' - A list of predecessors to this job run.
--
-- 'previousRunId', 'jobRun_previousRunId' - The ID of the previous run of this job. For example, the @JobRunId@
-- specified in the @StartJobRun@ action.
--
-- 'securityConfiguration', 'jobRun_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
--
-- 'startedOn', 'jobRun_startedOn' - The date and time at which this job run was started.
--
-- 'timeout', 'jobRun_timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. This value overrides the timeout value set in the parent job.
--
-- Streaming jobs do not have a timeout. The default for non-streaming jobs
-- is 2,880 minutes (48 hours).
--
-- 'triggerName', 'jobRun_triggerName' - The name of the trigger that started this job run.
--
-- 'workerType', 'jobRun_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, G.2X, or G.025X.
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
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
newJobRun ::
  JobRun
newJobRun =
  JobRun'
    { allocatedCapacity = Prelude.Nothing,
      arguments = Prelude.Nothing,
      attempt = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      dPUSeconds = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      executionClass = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobRunState = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      predecessorRuns = Prelude.Nothing,
      previousRunId = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      timeout = Prelude.Nothing,
      triggerName = Prelude.Nothing,
      workerType = Prelude.Nothing
    }

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
jobRun_allocatedCapacity :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_allocatedCapacity = Lens.lens (\JobRun' {allocatedCapacity} -> allocatedCapacity) (\s@JobRun' {} a -> s {allocatedCapacity = a} :: JobRun)

-- | The job arguments associated with this run. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that Glue itself consumes.
--
-- For information about how to specify and consume your own job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that Glue consumes to set up
-- your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by Glue>
-- topic in the developer guide.
jobRun_arguments :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobRun_arguments = Lens.lens (\JobRun' {arguments} -> arguments) (\s@JobRun' {} a -> s {arguments = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | The number of the attempt to run this job.
jobRun_attempt :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_attempt = Lens.lens (\JobRun' {attempt} -> attempt) (\s@JobRun' {} a -> s {attempt = a} :: JobRun)

-- | The date and time that this job run completed.
jobRun_completedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_completedOn = Lens.lens (\JobRun' {completedOn} -> completedOn) (\s@JobRun' {} a -> s {completedOn = a} :: JobRun) Prelude.. Lens.mapping Data._Time

-- | This field populates only for Auto Scaling job runs, and represents the
-- total time each executor ran during the lifecycle of a job run in
-- seconds, multiplied by a DPU factor (1 for @G.1X@, 2 for @G.2X@, or 0.25
-- for @G.025X@ workers). This value may be different than the
-- @executionEngineRuntime@ * @MaxCapacity@ as in the case of Auto Scaling
-- jobs, as the number of executors running at a given time may be less
-- than the @MaxCapacity@. Therefore, it is possible that the value of
-- @DPUSeconds@ is less than @executionEngineRuntime@ * @MaxCapacity@.
jobRun_dPUSeconds :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Double)
jobRun_dPUSeconds = Lens.lens (\JobRun' {dPUSeconds} -> dPUSeconds) (\s@JobRun' {} a -> s {dPUSeconds = a} :: JobRun)

-- | An error message associated with this job run.
jobRun_errorMessage :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_errorMessage = Lens.lens (\JobRun' {errorMessage} -> errorMessage) (\s@JobRun' {} a -> s {errorMessage = a} :: JobRun)

-- | Indicates whether the job is run with a standard or flexible execution
-- class. The standard execution-class is ideal for time-sensitive
-- workloads that require fast job startup and dedicated resources.
--
-- The flexible execution class is appropriate for time-insensitive jobs
-- whose start and completion times may vary.
--
-- Only jobs with Glue version 3.0 and above and command type @glueetl@
-- will be allowed to set @ExecutionClass@ to @FLEX@. The flexible
-- execution class is available for Spark jobs.
jobRun_executionClass :: Lens.Lens' JobRun (Prelude.Maybe ExecutionClass)
jobRun_executionClass = Lens.lens (\JobRun' {executionClass} -> executionClass) (\s@JobRun' {} a -> s {executionClass = a} :: JobRun)

-- | The amount of time (in seconds) that the job run consumed resources.
jobRun_executionTime :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_executionTime = Lens.lens (\JobRun' {executionTime} -> executionTime) (\s@JobRun' {} a -> s {executionTime = a} :: JobRun)

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
jobRun_glueVersion :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_glueVersion = Lens.lens (\JobRun' {glueVersion} -> glueVersion) (\s@JobRun' {} a -> s {glueVersion = a} :: JobRun)

-- | The ID of this job run.
jobRun_id :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_id = Lens.lens (\JobRun' {id} -> id) (\s@JobRun' {} a -> s {id = a} :: JobRun)

-- | The name of the job definition being used in this run.
jobRun_jobName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_jobName = Lens.lens (\JobRun' {jobName} -> jobName) (\s@JobRun' {} a -> s {jobName = a} :: JobRun)

-- | The current state of the job run. For more information about the
-- statuses of jobs that have terminated abnormally, see
-- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html Glue Job Run Statuses>.
jobRun_jobRunState :: Lens.Lens' JobRun (Prelude.Maybe JobRunState)
jobRun_jobRunState = Lens.lens (\JobRun' {jobRunState} -> jobRunState) (\s@JobRun' {} a -> s {jobRunState = a} :: JobRun)

-- | The last time that this job run was modified.
jobRun_lastModifiedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_lastModifiedOn = Lens.lens (\JobRun' {lastModifiedOn} -> lastModifiedOn) (\s@JobRun' {} a -> s {lastModifiedOn = a} :: JobRun) Prelude.. Lens.mapping Data._Time

-- | The name of the log group for secure logging that can be server-side
-- encrypted in Amazon CloudWatch using KMS. This name can be
-- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
-- you add a role name and @SecurityConfiguration@ name (in other words,
-- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
-- that security configuration is used to encrypt the log group.
jobRun_logGroupName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_logGroupName = Lens.lens (\JobRun' {logGroupName} -> logGroupName) (\s@JobRun' {} a -> s {logGroupName = a} :: JobRun)

-- | The number of Glue data processing units (DPUs) that can be allocated
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
--     (@JobCommand.Name@=\"glueetl\"), you can allocate a minimum of 2
--     DPUs. The default is 10 DPUs. This job type cannot have a fractional
--     DPU allocation.
jobRun_maxCapacity :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Double)
jobRun_maxCapacity = Lens.lens (\JobRun' {maxCapacity} -> maxCapacity) (\s@JobRun' {} a -> s {maxCapacity = a} :: JobRun)

-- | Specifies configuration properties of a job run notification.
jobRun_notificationProperty :: Lens.Lens' JobRun (Prelude.Maybe NotificationProperty)
jobRun_notificationProperty = Lens.lens (\JobRun' {notificationProperty} -> notificationProperty) (\s@JobRun' {} a -> s {notificationProperty = a} :: JobRun)

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
jobRun_numberOfWorkers :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_numberOfWorkers = Lens.lens (\JobRun' {numberOfWorkers} -> numberOfWorkers) (\s@JobRun' {} a -> s {numberOfWorkers = a} :: JobRun)

-- | A list of predecessors to this job run.
jobRun_predecessorRuns :: Lens.Lens' JobRun (Prelude.Maybe [Predecessor])
jobRun_predecessorRuns = Lens.lens (\JobRun' {predecessorRuns} -> predecessorRuns) (\s@JobRun' {} a -> s {predecessorRuns = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the previous run of this job. For example, the @JobRunId@
-- specified in the @StartJobRun@ action.
jobRun_previousRunId :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_previousRunId = Lens.lens (\JobRun' {previousRunId} -> previousRunId) (\s@JobRun' {} a -> s {previousRunId = a} :: JobRun)

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
jobRun_securityConfiguration :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_securityConfiguration = Lens.lens (\JobRun' {securityConfiguration} -> securityConfiguration) (\s@JobRun' {} a -> s {securityConfiguration = a} :: JobRun)

-- | The date and time at which this job run was started.
jobRun_startedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_startedOn = Lens.lens (\JobRun' {startedOn} -> startedOn) (\s@JobRun' {} a -> s {startedOn = a} :: JobRun) Prelude.. Lens.mapping Data._Time

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. This value overrides the timeout value set in the parent job.
--
-- Streaming jobs do not have a timeout. The default for non-streaming jobs
-- is 2,880 minutes (48 hours).
jobRun_timeout :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Natural)
jobRun_timeout = Lens.lens (\JobRun' {timeout} -> timeout) (\s@JobRun' {} a -> s {timeout = a} :: JobRun)

-- | The name of the trigger that started this job run.
jobRun_triggerName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_triggerName = Lens.lens (\JobRun' {triggerName} -> triggerName) (\s@JobRun' {} a -> s {triggerName = a} :: JobRun)

-- | The type of predefined worker that is allocated when a job runs. Accepts
-- a value of Standard, G.1X, G.2X, or G.025X.
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
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
jobRun_workerType :: Lens.Lens' JobRun (Prelude.Maybe WorkerType)
jobRun_workerType = Lens.lens (\JobRun' {workerType} -> workerType) (\s@JobRun' {} a -> s {workerType = a} :: JobRun)

instance Data.FromJSON JobRun where
  parseJSON =
    Data.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Prelude.<$> (x Data..:? "AllocatedCapacity")
            Prelude.<*> (x Data..:? "Arguments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Attempt")
            Prelude.<*> (x Data..:? "CompletedOn")
            Prelude.<*> (x Data..:? "DPUSeconds")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ExecutionClass")
            Prelude.<*> (x Data..:? "ExecutionTime")
            Prelude.<*> (x Data..:? "GlueVersion")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobRunState")
            Prelude.<*> (x Data..:? "LastModifiedOn")
            Prelude.<*> (x Data..:? "LogGroupName")
            Prelude.<*> (x Data..:? "MaxCapacity")
            Prelude.<*> (x Data..:? "NotificationProperty")
            Prelude.<*> (x Data..:? "NumberOfWorkers")
            Prelude.<*> ( x Data..:? "PredecessorRuns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PreviousRunId")
            Prelude.<*> (x Data..:? "SecurityConfiguration")
            Prelude.<*> (x Data..:? "StartedOn")
            Prelude.<*> (x Data..:? "Timeout")
            Prelude.<*> (x Data..:? "TriggerName")
            Prelude.<*> (x Data..:? "WorkerType")
      )

instance Prelude.Hashable JobRun where
  hashWithSalt _salt JobRun' {..} =
    _salt `Prelude.hashWithSalt` allocatedCapacity
      `Prelude.hashWithSalt` arguments
      `Prelude.hashWithSalt` attempt
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` dPUSeconds
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` executionClass
      `Prelude.hashWithSalt` executionTime
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobRunState
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` notificationProperty
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` predecessorRuns
      `Prelude.hashWithSalt` previousRunId
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` triggerName
      `Prelude.hashWithSalt` workerType

instance Prelude.NFData JobRun where
  rnf JobRun' {..} =
    Prelude.rnf allocatedCapacity
      `Prelude.seq` Prelude.rnf arguments
      `Prelude.seq` Prelude.rnf attempt
      `Prelude.seq` Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf dPUSeconds
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf executionClass
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRunState
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf notificationProperty
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf predecessorRuns
      `Prelude.seq` Prelude.rnf previousRunId
      `Prelude.seq` Prelude.rnf
        securityConfiguration
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf triggerName
      `Prelude.seq` Prelude.rnf workerType
