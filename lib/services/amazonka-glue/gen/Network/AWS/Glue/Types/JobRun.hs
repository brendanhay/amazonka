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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JobRun where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.JobRunState
import Amazonka.Glue.Types.NotificationProperty
import Amazonka.Glue.Types.Predecessor
import Amazonka.Glue.Types.WorkerType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a job run.
--
-- /See:/ 'newJobRun' smart constructor.
data JobRun = JobRun'
  { -- | The date and time that this job run completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The name of the trigger that started this job run.
    triggerName :: Prelude.Maybe Prelude.Text,
    -- | Specifies configuration properties of a job run notification.
    notificationProperty :: Prelude.Maybe NotificationProperty,
    -- | The last time that this job run was modified.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
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
    -- | The name of the job definition being used in this run.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which this job run was started.
    startedOn :: Prelude.Maybe Core.POSIX,
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
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job run.
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
    -- | The current state of the job run. For more information about the
    -- statuses of jobs that have terminated abnormally, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html Glue Job Run Statuses>.
    jobRunState :: Prelude.Maybe JobRunState,
    -- | The name of the log group for secure logging that can be server-side
    -- encrypted in Amazon CloudWatch using KMS. This name can be
    -- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
    -- you add a role name and @SecurityConfiguration@ name (in other words,
    -- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
    -- that security configuration is used to encrypt the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The amount of time (in seconds) that the job run consumed resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | A list of predecessors to this job run.
    predecessorRuns :: Prelude.Maybe [Predecessor],
    -- | The ID of the previous run of this job. For example, the @JobRunId@
    -- specified in the @StartJobRun@ action.
    previousRunId :: Prelude.Maybe Prelude.Text,
    -- | The ID of this job run.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of the attempt to run this job.
    attempt :: Prelude.Maybe Prelude.Int,
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of Glue data processing units (DPUs) allocated to this
    -- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
    -- a relative measure of processing power that consists of 4 vCPUs of
    -- compute capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    allocatedCapacity :: Prelude.Maybe Prelude.Int,
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
    --     (@JobCommand.Name@=\"glueetl\"), you can allocate from 2 to 100
    --     DPUs. The default is 10 DPUs. This job type cannot have a fractional
    --     DPU allocation.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours). This overrides the
    -- timeout value set in the parent job.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | An error message associated with this job run.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'completedOn', 'jobRun_completedOn' - The date and time that this job run completed.
--
-- 'numberOfWorkers', 'jobRun_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'triggerName', 'jobRun_triggerName' - The name of the trigger that started this job run.
--
-- 'notificationProperty', 'jobRun_notificationProperty' - Specifies configuration properties of a job run notification.
--
-- 'lastModifiedOn', 'jobRun_lastModifiedOn' - The last time that this job run was modified.
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
-- 'jobName', 'jobRun_jobName' - The name of the job definition being used in this run.
--
-- 'startedOn', 'jobRun_startedOn' - The date and time at which this job run was started.
--
-- 'workerType', 'jobRun_workerType' - The type of predefined worker that is allocated when a job runs. Accepts
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
-- 'securityConfiguration', 'jobRun_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
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
-- 'jobRunState', 'jobRun_jobRunState' - The current state of the job run. For more information about the
-- statuses of jobs that have terminated abnormally, see
-- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html Glue Job Run Statuses>.
--
-- 'logGroupName', 'jobRun_logGroupName' - The name of the log group for secure logging that can be server-side
-- encrypted in Amazon CloudWatch using KMS. This name can be
-- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
-- you add a role name and @SecurityConfiguration@ name (in other words,
-- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
-- that security configuration is used to encrypt the log group.
--
-- 'executionTime', 'jobRun_executionTime' - The amount of time (in seconds) that the job run consumed resources.
--
-- 'predecessorRuns', 'jobRun_predecessorRuns' - A list of predecessors to this job run.
--
-- 'previousRunId', 'jobRun_previousRunId' - The ID of the previous run of this job. For example, the @JobRunId@
-- specified in the @StartJobRun@ action.
--
-- 'id', 'jobRun_id' - The ID of this job run.
--
-- 'attempt', 'jobRun_attempt' - The number of the attempt to run this job.
--
-- 'allocatedCapacity', 'jobRun_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
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
--     (@JobCommand.Name@=\"glueetl\"), you can allocate from 2 to 100
--     DPUs. The default is 10 DPUs. This job type cannot have a fractional
--     DPU allocation.
--
-- 'timeout', 'jobRun_timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
--
-- 'errorMessage', 'jobRun_errorMessage' - An error message associated with this job run.
newJobRun ::
  JobRun
newJobRun =
  JobRun'
    { completedOn = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      triggerName = Prelude.Nothing,
      notificationProperty = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      arguments = Prelude.Nothing,
      jobName = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      workerType = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      jobRunState = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      predecessorRuns = Prelude.Nothing,
      previousRunId = Prelude.Nothing,
      id = Prelude.Nothing,
      attempt = Prelude.Nothing,
      allocatedCapacity = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      timeout = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The date and time that this job run completed.
jobRun_completedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_completedOn = Lens.lens (\JobRun' {completedOn} -> completedOn) (\s@JobRun' {} a -> s {completedOn = a} :: JobRun) Prelude.. Lens.mapping Core._Time

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
jobRun_numberOfWorkers :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_numberOfWorkers = Lens.lens (\JobRun' {numberOfWorkers} -> numberOfWorkers) (\s@JobRun' {} a -> s {numberOfWorkers = a} :: JobRun)

-- | The name of the trigger that started this job run.
jobRun_triggerName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_triggerName = Lens.lens (\JobRun' {triggerName} -> triggerName) (\s@JobRun' {} a -> s {triggerName = a} :: JobRun)

-- | Specifies configuration properties of a job run notification.
jobRun_notificationProperty :: Lens.Lens' JobRun (Prelude.Maybe NotificationProperty)
jobRun_notificationProperty = Lens.lens (\JobRun' {notificationProperty} -> notificationProperty) (\s@JobRun' {} a -> s {notificationProperty = a} :: JobRun)

-- | The last time that this job run was modified.
jobRun_lastModifiedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_lastModifiedOn = Lens.lens (\JobRun' {lastModifiedOn} -> lastModifiedOn) (\s@JobRun' {} a -> s {lastModifiedOn = a} :: JobRun) Prelude.. Lens.mapping Core._Time

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

-- | The name of the job definition being used in this run.
jobRun_jobName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_jobName = Lens.lens (\JobRun' {jobName} -> jobName) (\s@JobRun' {} a -> s {jobName = a} :: JobRun)

-- | The date and time at which this job run was started.
jobRun_startedOn :: Lens.Lens' JobRun (Prelude.Maybe Prelude.UTCTime)
jobRun_startedOn = Lens.lens (\JobRun' {startedOn} -> startedOn) (\s@JobRun' {} a -> s {startedOn = a} :: JobRun) Prelude.. Lens.mapping Core._Time

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
jobRun_workerType :: Lens.Lens' JobRun (Prelude.Maybe WorkerType)
jobRun_workerType = Lens.lens (\JobRun' {workerType} -> workerType) (\s@JobRun' {} a -> s {workerType = a} :: JobRun)

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
jobRun_securityConfiguration :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_securityConfiguration = Lens.lens (\JobRun' {securityConfiguration} -> securityConfiguration) (\s@JobRun' {} a -> s {securityConfiguration = a} :: JobRun)

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

-- | The current state of the job run. For more information about the
-- statuses of jobs that have terminated abnormally, see
-- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html Glue Job Run Statuses>.
jobRun_jobRunState :: Lens.Lens' JobRun (Prelude.Maybe JobRunState)
jobRun_jobRunState = Lens.lens (\JobRun' {jobRunState} -> jobRunState) (\s@JobRun' {} a -> s {jobRunState = a} :: JobRun)

-- | The name of the log group for secure logging that can be server-side
-- encrypted in Amazon CloudWatch using KMS. This name can be
-- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
-- you add a role name and @SecurityConfiguration@ name (in other words,
-- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
-- that security configuration is used to encrypt the log group.
jobRun_logGroupName :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_logGroupName = Lens.lens (\JobRun' {logGroupName} -> logGroupName) (\s@JobRun' {} a -> s {logGroupName = a} :: JobRun)

-- | The amount of time (in seconds) that the job run consumed resources.
jobRun_executionTime :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_executionTime = Lens.lens (\JobRun' {executionTime} -> executionTime) (\s@JobRun' {} a -> s {executionTime = a} :: JobRun)

-- | A list of predecessors to this job run.
jobRun_predecessorRuns :: Lens.Lens' JobRun (Prelude.Maybe [Predecessor])
jobRun_predecessorRuns = Lens.lens (\JobRun' {predecessorRuns} -> predecessorRuns) (\s@JobRun' {} a -> s {predecessorRuns = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the previous run of this job. For example, the @JobRunId@
-- specified in the @StartJobRun@ action.
jobRun_previousRunId :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_previousRunId = Lens.lens (\JobRun' {previousRunId} -> previousRunId) (\s@JobRun' {} a -> s {previousRunId = a} :: JobRun)

-- | The ID of this job run.
jobRun_id :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_id = Lens.lens (\JobRun' {id} -> id) (\s@JobRun' {} a -> s {id = a} :: JobRun)

-- | The number of the attempt to run this job.
jobRun_attempt :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_attempt = Lens.lens (\JobRun' {attempt} -> attempt) (\s@JobRun' {} a -> s {attempt = a} :: JobRun)

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of Glue data processing units (DPUs) allocated to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
jobRun_allocatedCapacity :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_allocatedCapacity = Lens.lens (\JobRun' {allocatedCapacity} -> allocatedCapacity) (\s@JobRun' {} a -> s {allocatedCapacity = a} :: JobRun)

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
--     (@JobCommand.Name@=\"glueetl\"), you can allocate from 2 to 100
--     DPUs. The default is 10 DPUs. This job type cannot have a fractional
--     DPU allocation.
jobRun_maxCapacity :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Double)
jobRun_maxCapacity = Lens.lens (\JobRun' {maxCapacity} -> maxCapacity) (\s@JobRun' {} a -> s {maxCapacity = a} :: JobRun)

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
jobRun_timeout :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Natural)
jobRun_timeout = Lens.lens (\JobRun' {timeout} -> timeout) (\s@JobRun' {} a -> s {timeout = a} :: JobRun)

-- | An error message associated with this job run.
jobRun_errorMessage :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_errorMessage = Lens.lens (\JobRun' {errorMessage} -> errorMessage) (\s@JobRun' {} a -> s {errorMessage = a} :: JobRun)

instance Core.FromJSON JobRun where
  parseJSON =
    Core.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Prelude.<$> (x Core..:? "CompletedOn")
            Prelude.<*> (x Core..:? "NumberOfWorkers")
            Prelude.<*> (x Core..:? "TriggerName")
            Prelude.<*> (x Core..:? "NotificationProperty")
            Prelude.<*> (x Core..:? "LastModifiedOn")
            Prelude.<*> (x Core..:? "Arguments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "StartedOn")
            Prelude.<*> (x Core..:? "WorkerType")
            Prelude.<*> (x Core..:? "SecurityConfiguration")
            Prelude.<*> (x Core..:? "GlueVersion")
            Prelude.<*> (x Core..:? "JobRunState")
            Prelude.<*> (x Core..:? "LogGroupName")
            Prelude.<*> (x Core..:? "ExecutionTime")
            Prelude.<*> ( x Core..:? "PredecessorRuns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PreviousRunId")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Attempt")
            Prelude.<*> (x Core..:? "AllocatedCapacity")
            Prelude.<*> (x Core..:? "MaxCapacity")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable JobRun

instance Prelude.NFData JobRun
