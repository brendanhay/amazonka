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
-- Module      : Network.AWS.Glue.Types.JobRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobRun where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.Predecessor
import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens

-- | Contains information about a job run.
--
-- /See:/ 'newJobRun' smart constructor.
data JobRun = JobRun'
  { -- | A list of predecessors to this job run.
    predecessorRuns :: Core.Maybe [Predecessor],
    -- | The amount of time (in seconds) that the job run consumed resources.
    executionTime :: Core.Maybe Core.Int,
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- job run.
    securityConfiguration :: Core.Maybe Core.Text,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours). This overrides the
    -- timeout value set in the parent job.
    timeout :: Core.Maybe Core.Natural,
    -- | The number of AWS Glue data processing units (DPUs) that can be
    -- allocated when this job runs. A DPU is a relative measure of processing
    -- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
    -- For more information, see the
    -- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
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
    maxCapacity :: Core.Maybe Core.Double,
    -- | The ID of this job run.
    id :: Core.Maybe Core.Text,
    -- | Specifies configuration properties of a job run notification.
    notificationProperty :: Core.Maybe NotificationProperty,
    -- | The last time that this job run was modified.
    lastModifiedOn :: Core.Maybe Core.POSIX,
    -- | The name of the trigger that started this job run.
    triggerName :: Core.Maybe Core.Text,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | The name of the log group for secure logging that can be server-side
    -- encrypted in Amazon CloudWatch using AWS KMS. This name can be
    -- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
    -- you add a role name and @SecurityConfiguration@ name (in other words,
    -- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
    -- that security configuration is used to encrypt the log group.
    logGroupName :: Core.Maybe Core.Text,
    -- | The date and time that this job run completed.
    completedOn :: Core.Maybe Core.POSIX,
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
    -- | The current state of the job run. For more information about the
    -- statuses of jobs that have terminated abnormally, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html AWS Glue Job Run Statuses>.
    jobRunState :: Core.Maybe JobRunState,
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
    workerType :: Core.Maybe WorkerType,
    -- | An error message associated with this job run.
    errorMessage :: Core.Maybe Core.Text,
    -- | The date and time at which this job run was started.
    startedOn :: Core.Maybe Core.POSIX,
    -- | The name of the job definition being used in this run.
    jobName :: Core.Maybe Core.Text,
    -- | The job arguments associated with this run. For this job run, they
    -- replace the default arguments set in the job definition itself.
    --
    -- You can specify arguments here that your own job-execution script
    -- consumes, as well as arguments that AWS Glue itself consumes.
    --
    -- For information about how to specify and consume your own job arguments,
    -- see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
    -- topic in the developer guide.
    --
    -- For information about the key-value pairs that AWS Glue consumes to set
    -- up your job, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
    -- topic in the developer guide.
    arguments :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) allocated to this
    -- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
    -- a relative measure of processing power that consists of 4 vCPUs of
    -- compute capacity and 16 GB of memory. For more information, see the
    -- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    allocatedCapacity :: Core.Maybe Core.Int,
    -- | The ID of the previous run of this job. For example, the @JobRunId@
    -- specified in the @StartJobRun@ action.
    previousRunId :: Core.Maybe Core.Text,
    -- | The number of the attempt to run this job.
    attempt :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predecessorRuns', 'jobRun_predecessorRuns' - A list of predecessors to this job run.
--
-- 'executionTime', 'jobRun_executionTime' - The amount of time (in seconds) that the job run consumed resources.
--
-- 'securityConfiguration', 'jobRun_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
--
-- 'timeout', 'jobRun_timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
--
-- 'maxCapacity', 'jobRun_maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
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
-- 'id', 'jobRun_id' - The ID of this job run.
--
-- 'notificationProperty', 'jobRun_notificationProperty' - Specifies configuration properties of a job run notification.
--
-- 'lastModifiedOn', 'jobRun_lastModifiedOn' - The last time that this job run was modified.
--
-- 'triggerName', 'jobRun_triggerName' - The name of the trigger that started this job run.
--
-- 'numberOfWorkers', 'jobRun_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'logGroupName', 'jobRun_logGroupName' - The name of the log group for secure logging that can be server-side
-- encrypted in Amazon CloudWatch using AWS KMS. This name can be
-- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
-- you add a role name and @SecurityConfiguration@ name (in other words,
-- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
-- that security configuration is used to encrypt the log group.
--
-- 'completedOn', 'jobRun_completedOn' - The date and time that this job run completed.
--
-- 'glueVersion', 'jobRun_glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS
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
-- 'jobRunState', 'jobRun_jobRunState' - The current state of the job run. For more information about the
-- statuses of jobs that have terminated abnormally, see
-- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html AWS Glue Job Run Statuses>.
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
-- 'errorMessage', 'jobRun_errorMessage' - An error message associated with this job run.
--
-- 'startedOn', 'jobRun_startedOn' - The date and time at which this job run was started.
--
-- 'jobName', 'jobRun_jobName' - The name of the job definition being used in this run.
--
-- 'arguments', 'jobRun_arguments' - The job arguments associated with this run. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that AWS Glue itself consumes.
--
-- For information about how to specify and consume your own job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that AWS Glue consumes to set
-- up your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
-- topic in the developer guide.
--
-- 'allocatedCapacity', 'jobRun_allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- 'previousRunId', 'jobRun_previousRunId' - The ID of the previous run of this job. For example, the @JobRunId@
-- specified in the @StartJobRun@ action.
--
-- 'attempt', 'jobRun_attempt' - The number of the attempt to run this job.
newJobRun ::
  JobRun
newJobRun =
  JobRun'
    { predecessorRuns = Core.Nothing,
      executionTime = Core.Nothing,
      securityConfiguration = Core.Nothing,
      timeout = Core.Nothing,
      maxCapacity = Core.Nothing,
      id = Core.Nothing,
      notificationProperty = Core.Nothing,
      lastModifiedOn = Core.Nothing,
      triggerName = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      logGroupName = Core.Nothing,
      completedOn = Core.Nothing,
      glueVersion = Core.Nothing,
      jobRunState = Core.Nothing,
      workerType = Core.Nothing,
      errorMessage = Core.Nothing,
      startedOn = Core.Nothing,
      jobName = Core.Nothing,
      arguments = Core.Nothing,
      allocatedCapacity = Core.Nothing,
      previousRunId = Core.Nothing,
      attempt = Core.Nothing
    }

-- | A list of predecessors to this job run.
jobRun_predecessorRuns :: Lens.Lens' JobRun (Core.Maybe [Predecessor])
jobRun_predecessorRuns = Lens.lens (\JobRun' {predecessorRuns} -> predecessorRuns) (\s@JobRun' {} a -> s {predecessorRuns = a} :: JobRun) Core.. Lens.mapping Lens._Coerce

-- | The amount of time (in seconds) that the job run consumed resources.
jobRun_executionTime :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jobRun_executionTime = Lens.lens (\JobRun' {executionTime} -> executionTime) (\s@JobRun' {} a -> s {executionTime = a} :: JobRun)

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- job run.
jobRun_securityConfiguration :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_securityConfiguration = Lens.lens (\JobRun' {securityConfiguration} -> securityConfiguration) (\s@JobRun' {} a -> s {securityConfiguration = a} :: JobRun)

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours). This overrides the
-- timeout value set in the parent job.
jobRun_timeout :: Lens.Lens' JobRun (Core.Maybe Core.Natural)
jobRun_timeout = Lens.lens (\JobRun' {timeout} -> timeout) (\s@JobRun' {} a -> s {timeout = a} :: JobRun)

-- | The number of AWS Glue data processing units (DPUs) that can be
-- allocated when this job runs. A DPU is a relative measure of processing
-- power that consists of 4 vCPUs of compute capacity and 16 GB of memory.
-- For more information, see the
-- <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
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
jobRun_maxCapacity :: Lens.Lens' JobRun (Core.Maybe Core.Double)
jobRun_maxCapacity = Lens.lens (\JobRun' {maxCapacity} -> maxCapacity) (\s@JobRun' {} a -> s {maxCapacity = a} :: JobRun)

-- | The ID of this job run.
jobRun_id :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_id = Lens.lens (\JobRun' {id} -> id) (\s@JobRun' {} a -> s {id = a} :: JobRun)

-- | Specifies configuration properties of a job run notification.
jobRun_notificationProperty :: Lens.Lens' JobRun (Core.Maybe NotificationProperty)
jobRun_notificationProperty = Lens.lens (\JobRun' {notificationProperty} -> notificationProperty) (\s@JobRun' {} a -> s {notificationProperty = a} :: JobRun)

-- | The last time that this job run was modified.
jobRun_lastModifiedOn :: Lens.Lens' JobRun (Core.Maybe Core.UTCTime)
jobRun_lastModifiedOn = Lens.lens (\JobRun' {lastModifiedOn} -> lastModifiedOn) (\s@JobRun' {} a -> s {lastModifiedOn = a} :: JobRun) Core.. Lens.mapping Core._Time

-- | The name of the trigger that started this job run.
jobRun_triggerName :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_triggerName = Lens.lens (\JobRun' {triggerName} -> triggerName) (\s@JobRun' {} a -> s {triggerName = a} :: JobRun)

-- | The number of workers of a defined @workerType@ that are allocated when
-- a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
jobRun_numberOfWorkers :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jobRun_numberOfWorkers = Lens.lens (\JobRun' {numberOfWorkers} -> numberOfWorkers) (\s@JobRun' {} a -> s {numberOfWorkers = a} :: JobRun)

-- | The name of the log group for secure logging that can be server-side
-- encrypted in Amazon CloudWatch using AWS KMS. This name can be
-- @\/aws-glue\/jobs\/@, in which case the default encryption is @NONE@. If
-- you add a role name and @SecurityConfiguration@ name (in other words,
-- @\/aws-glue\/jobs-yourRoleName-yourSecurityConfigurationName\/@), then
-- that security configuration is used to encrypt the log group.
jobRun_logGroupName :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_logGroupName = Lens.lens (\JobRun' {logGroupName} -> logGroupName) (\s@JobRun' {} a -> s {logGroupName = a} :: JobRun)

-- | The date and time that this job run completed.
jobRun_completedOn :: Lens.Lens' JobRun (Core.Maybe Core.UTCTime)
jobRun_completedOn = Lens.lens (\JobRun' {completedOn} -> completedOn) (\s@JobRun' {} a -> s {completedOn = a} :: JobRun) Core.. Lens.mapping Core._Time

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
jobRun_glueVersion :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_glueVersion = Lens.lens (\JobRun' {glueVersion} -> glueVersion) (\s@JobRun' {} a -> s {glueVersion = a} :: JobRun)

-- | The current state of the job run. For more information about the
-- statuses of jobs that have terminated abnormally, see
-- <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html AWS Glue Job Run Statuses>.
jobRun_jobRunState :: Lens.Lens' JobRun (Core.Maybe JobRunState)
jobRun_jobRunState = Lens.lens (\JobRun' {jobRunState} -> jobRunState) (\s@JobRun' {} a -> s {jobRunState = a} :: JobRun)

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
jobRun_workerType :: Lens.Lens' JobRun (Core.Maybe WorkerType)
jobRun_workerType = Lens.lens (\JobRun' {workerType} -> workerType) (\s@JobRun' {} a -> s {workerType = a} :: JobRun)

-- | An error message associated with this job run.
jobRun_errorMessage :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_errorMessage = Lens.lens (\JobRun' {errorMessage} -> errorMessage) (\s@JobRun' {} a -> s {errorMessage = a} :: JobRun)

-- | The date and time at which this job run was started.
jobRun_startedOn :: Lens.Lens' JobRun (Core.Maybe Core.UTCTime)
jobRun_startedOn = Lens.lens (\JobRun' {startedOn} -> startedOn) (\s@JobRun' {} a -> s {startedOn = a} :: JobRun) Core.. Lens.mapping Core._Time

-- | The name of the job definition being used in this run.
jobRun_jobName :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_jobName = Lens.lens (\JobRun' {jobName} -> jobName) (\s@JobRun' {} a -> s {jobName = a} :: JobRun)

-- | The job arguments associated with this run. For this job run, they
-- replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script
-- consumes, as well as arguments that AWS Glue itself consumes.
--
-- For information about how to specify and consume your own job arguments,
-- see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python>
-- topic in the developer guide.
--
-- For information about the key-value pairs that AWS Glue consumes to set
-- up your job, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue>
-- topic in the developer guide.
jobRun_arguments :: Lens.Lens' JobRun (Core.Maybe (Core.HashMap Core.Text Core.Text))
jobRun_arguments = Lens.lens (\JobRun' {arguments} -> arguments) (\s@JobRun' {} a -> s {arguments = a} :: JobRun) Core.. Lens.mapping Lens._Coerce

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to this
-- JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is
-- a relative measure of processing power that consists of 4 vCPUs of
-- compute capacity and 16 GB of memory. For more information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
jobRun_allocatedCapacity :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jobRun_allocatedCapacity = Lens.lens (\JobRun' {allocatedCapacity} -> allocatedCapacity) (\s@JobRun' {} a -> s {allocatedCapacity = a} :: JobRun)

-- | The ID of the previous run of this job. For example, the @JobRunId@
-- specified in the @StartJobRun@ action.
jobRun_previousRunId :: Lens.Lens' JobRun (Core.Maybe Core.Text)
jobRun_previousRunId = Lens.lens (\JobRun' {previousRunId} -> previousRunId) (\s@JobRun' {} a -> s {previousRunId = a} :: JobRun)

-- | The number of the attempt to run this job.
jobRun_attempt :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jobRun_attempt = Lens.lens (\JobRun' {attempt} -> attempt) (\s@JobRun' {} a -> s {attempt = a} :: JobRun)

instance Core.FromJSON JobRun where
  parseJSON =
    Core.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Core.<$> (x Core..:? "PredecessorRuns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ExecutionTime")
            Core.<*> (x Core..:? "SecurityConfiguration")
            Core.<*> (x Core..:? "Timeout")
            Core.<*> (x Core..:? "MaxCapacity")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "NotificationProperty")
            Core.<*> (x Core..:? "LastModifiedOn")
            Core.<*> (x Core..:? "TriggerName")
            Core.<*> (x Core..:? "NumberOfWorkers")
            Core.<*> (x Core..:? "LogGroupName")
            Core.<*> (x Core..:? "CompletedOn")
            Core.<*> (x Core..:? "GlueVersion")
            Core.<*> (x Core..:? "JobRunState")
            Core.<*> (x Core..:? "WorkerType")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "StartedOn")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "Arguments" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AllocatedCapacity")
            Core.<*> (x Core..:? "PreviousRunId")
            Core.<*> (x Core..:? "Attempt")
      )

instance Core.Hashable JobRun

instance Core.NFData JobRun
