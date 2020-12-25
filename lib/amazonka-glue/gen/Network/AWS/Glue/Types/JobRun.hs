{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobRun
  ( JobRun (..),

    -- * Smart constructor
    mkJobRun,

    -- * Lenses
    jrAllocatedCapacity,
    jrArguments,
    jrAttempt,
    jrCompletedOn,
    jrErrorMessage,
    jrExecutionTime,
    jrGlueVersion,
    jrId,
    jrJobName,
    jrJobRunState,
    jrLastModifiedOn,
    jrLogGroupName,
    jrMaxCapacity,
    jrNotificationProperty,
    jrNumberOfWorkers,
    jrPredecessorRuns,
    jrPreviousRunId,
    jrSecurityConfiguration,
    jrStartedOn,
    jrTimeout,
    jrTriggerName,
    jrWorkerType,
  )
where

import qualified Network.AWS.Glue.Types.ErrorString as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.GlueVersionString as Types
import qualified Network.AWS.Glue.Types.IdString as Types
import qualified Network.AWS.Glue.Types.JobRunState as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.NotificationProperty as Types
import qualified Network.AWS.Glue.Types.Predecessor as Types
import qualified Network.AWS.Glue.Types.WorkerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a job run.
--
-- /See:/ 'mkJobRun' smart constructor.
data JobRun = JobRun'
  { -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) allocated to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    allocatedCapacity :: Core.Maybe Core.Int,
    -- | The job arguments associated with this run. For this job run, they replace the default arguments set in the job definition itself.
    --
    -- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
    -- For information about how to specify and consume your own job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
    -- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
    arguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString),
    -- | The number of the attempt to run this job.
    attempt :: Core.Maybe Core.Int,
    -- | The date and time that this job run completed.
    completedOn :: Core.Maybe Core.NominalDiffTime,
    -- | An error message associated with this job run.
    errorMessage :: Core.Maybe Types.ErrorString,
    -- | The amount of time (in seconds) that the job run consumed resources.
    executionTime :: Core.Maybe Core.Int,
    -- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
    --
    -- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
    -- Jobs that are created without specifying a Glue version default to Glue 0.9.
    glueVersion :: Core.Maybe Types.GlueVersionString,
    -- | The ID of this job run.
    id :: Core.Maybe Types.IdString,
    -- | The name of the job definition being used in this run.
    jobName :: Core.Maybe Types.NameString,
    -- | The current state of the job run. For more information about the statuses of jobs that have terminated abnormally, see <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html AWS Glue Job Run Statuses> .
    jobRunState :: Core.Maybe Types.JobRunState,
    -- | The last time that this job run was modified.
    lastModifiedOn :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the log group for secure logging that can be server-side encrypted in Amazon CloudWatch using AWS KMS. This name can be @/aws-glue/jobs/@ , in which case the default encryption is @NONE@ . If you add a role name and @SecurityConfiguration@ name (in other words, @/aws-glue/jobs-yourRoleName-yourSecurityConfigurationName/@ ), then that security configuration is used to encrypt the log group.
    logGroupName :: Core.Maybe Types.GenericString,
    -- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
    -- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job or an Apache Spark ETL job:
    --
    --     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
    --
    --
    --     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
    maxCapacity :: Core.Maybe Core.Double,
    -- | Specifies configuration properties of a job run notification.
    notificationProperty :: Core.Maybe Types.NotificationProperty,
    -- | The number of workers of a defined @workerType@ that are allocated when a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | A list of predecessors to this job run.
    predecessorRuns :: Core.Maybe [Types.Predecessor],
    -- | The ID of the previous run of this job. For example, the @JobRunId@ specified in the @StartJobRun@ action.
    previousRunId :: Core.Maybe Types.IdString,
    -- | The name of the @SecurityConfiguration@ structure to be used with this job run.
    securityConfiguration :: Core.Maybe Types.NameString,
    -- | The date and time at which this job run was started.
    startedOn :: Core.Maybe Core.NominalDiffTime,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
    timeout :: Core.Maybe Core.Natural,
    -- | The name of the trigger that started this job run.
    triggerName :: Core.Maybe Types.NameString,
    -- | The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.
    --
    --
    --     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
    --
    --
    --     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
    --
    --
    --     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
    workerType :: Core.Maybe Types.WorkerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'JobRun' value with any optional fields omitted.
mkJobRun ::
  JobRun
mkJobRun =
  JobRun'
    { allocatedCapacity = Core.Nothing,
      arguments = Core.Nothing,
      attempt = Core.Nothing,
      completedOn = Core.Nothing,
      errorMessage = Core.Nothing,
      executionTime = Core.Nothing,
      glueVersion = Core.Nothing,
      id = Core.Nothing,
      jobName = Core.Nothing,
      jobRunState = Core.Nothing,
      lastModifiedOn = Core.Nothing,
      logGroupName = Core.Nothing,
      maxCapacity = Core.Nothing,
      notificationProperty = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      predecessorRuns = Core.Nothing,
      previousRunId = Core.Nothing,
      securityConfiguration = Core.Nothing,
      startedOn = Core.Nothing,
      timeout = Core.Nothing,
      triggerName = Core.Nothing,
      workerType = Core.Nothing
    }

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrAllocatedCapacity :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jrAllocatedCapacity = Lens.field @"allocatedCapacity"
{-# DEPRECATED jrAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

-- | The job arguments associated with this run. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrArguments :: Lens.Lens' JobRun (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
jrArguments = Lens.field @"arguments"
{-# DEPRECATED jrArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

-- | The number of the attempt to run this job.
--
-- /Note:/ Consider using 'attempt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrAttempt :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jrAttempt = Lens.field @"attempt"
{-# DEPRECATED jrAttempt "Use generic-lens or generic-optics with 'attempt' instead." #-}

-- | The date and time that this job run completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrCompletedOn :: Lens.Lens' JobRun (Core.Maybe Core.NominalDiffTime)
jrCompletedOn = Lens.field @"completedOn"
{-# DEPRECATED jrCompletedOn "Use generic-lens or generic-optics with 'completedOn' instead." #-}

-- | An error message associated with this job run.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrErrorMessage :: Lens.Lens' JobRun (Core.Maybe Types.ErrorString)
jrErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED jrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The amount of time (in seconds) that the job run consumed resources.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrExecutionTime :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jrExecutionTime = Lens.field @"executionTime"
{-# DEPRECATED jrExecutionTime "Use generic-lens or generic-optics with 'executionTime' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrGlueVersion :: Lens.Lens' JobRun (Core.Maybe Types.GlueVersionString)
jrGlueVersion = Lens.field @"glueVersion"
{-# DEPRECATED jrGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The ID of this job run.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrId :: Lens.Lens' JobRun (Core.Maybe Types.IdString)
jrId = Lens.field @"id"
{-# DEPRECATED jrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the job definition being used in this run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrJobName :: Lens.Lens' JobRun (Core.Maybe Types.NameString)
jrJobName = Lens.field @"jobName"
{-# DEPRECATED jrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The current state of the job run. For more information about the statuses of jobs that have terminated abnormally, see <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html AWS Glue Job Run Statuses> .
--
-- /Note:/ Consider using 'jobRunState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrJobRunState :: Lens.Lens' JobRun (Core.Maybe Types.JobRunState)
jrJobRunState = Lens.field @"jobRunState"
{-# DEPRECATED jrJobRunState "Use generic-lens or generic-optics with 'jobRunState' instead." #-}

-- | The last time that this job run was modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLastModifiedOn :: Lens.Lens' JobRun (Core.Maybe Core.NominalDiffTime)
jrLastModifiedOn = Lens.field @"lastModifiedOn"
{-# DEPRECATED jrLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | The name of the log group for secure logging that can be server-side encrypted in Amazon CloudWatch using AWS KMS. This name can be @/aws-glue/jobs/@ , in which case the default encryption is @NONE@ . If you add a role name and @SecurityConfiguration@ name (in other words, @/aws-glue/jobs-yourRoleName-yourSecurityConfigurationName/@ ), then that security configuration is used to encrypt the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLogGroupName :: Lens.Lens' JobRun (Core.Maybe Types.GenericString)
jrLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED jrLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
-- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job or an Apache Spark ETL job:
--
--     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
--
--
--     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
--
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrMaxCapacity :: Lens.Lens' JobRun (Core.Maybe Core.Double)
jrMaxCapacity = Lens.field @"maxCapacity"
{-# DEPRECATED jrMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | Specifies configuration properties of a job run notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrNotificationProperty :: Lens.Lens' JobRun (Core.Maybe Types.NotificationProperty)
jrNotificationProperty = Lens.field @"notificationProperty"
{-# DEPRECATED jrNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrNumberOfWorkers :: Lens.Lens' JobRun (Core.Maybe Core.Int)
jrNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# DEPRECATED jrNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | A list of predecessors to this job run.
--
-- /Note:/ Consider using 'predecessorRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrPredecessorRuns :: Lens.Lens' JobRun (Core.Maybe [Types.Predecessor])
jrPredecessorRuns = Lens.field @"predecessorRuns"
{-# DEPRECATED jrPredecessorRuns "Use generic-lens or generic-optics with 'predecessorRuns' instead." #-}

-- | The ID of the previous run of this job. For example, the @JobRunId@ specified in the @StartJobRun@ action.
--
-- /Note:/ Consider using 'previousRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrPreviousRunId :: Lens.Lens' JobRun (Core.Maybe Types.IdString)
jrPreviousRunId = Lens.field @"previousRunId"
{-# DEPRECATED jrPreviousRunId "Use generic-lens or generic-optics with 'previousRunId' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job run.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrSecurityConfiguration :: Lens.Lens' JobRun (Core.Maybe Types.NameString)
jrSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED jrSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The date and time at which this job run was started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrStartedOn :: Lens.Lens' JobRun (Core.Maybe Core.NominalDiffTime)
jrStartedOn = Lens.field @"startedOn"
{-# DEPRECATED jrStartedOn "Use generic-lens or generic-optics with 'startedOn' instead." #-}

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrTimeout :: Lens.Lens' JobRun (Core.Maybe Core.Natural)
jrTimeout = Lens.field @"timeout"
{-# DEPRECATED jrTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The name of the trigger that started this job run.
--
-- /Note:/ Consider using 'triggerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrTriggerName :: Lens.Lens' JobRun (Core.Maybe Types.NameString)
jrTriggerName = Lens.field @"triggerName"
{-# DEPRECATED jrTriggerName "Use generic-lens or generic-optics with 'triggerName' instead." #-}

-- | The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
--
--
--     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
--
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrWorkerType :: Lens.Lens' JobRun (Core.Maybe Types.WorkerType)
jrWorkerType = Lens.field @"workerType"
{-# DEPRECATED jrWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

instance Core.FromJSON JobRun where
  parseJSON =
    Core.withObject "JobRun" Core.$
      \x ->
        JobRun'
          Core.<$> (x Core..:? "AllocatedCapacity")
          Core.<*> (x Core..:? "Arguments")
          Core.<*> (x Core..:? "Attempt")
          Core.<*> (x Core..:? "CompletedOn")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "ExecutionTime")
          Core.<*> (x Core..:? "GlueVersion")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "JobRunState")
          Core.<*> (x Core..:? "LastModifiedOn")
          Core.<*> (x Core..:? "LogGroupName")
          Core.<*> (x Core..:? "MaxCapacity")
          Core.<*> (x Core..:? "NotificationProperty")
          Core.<*> (x Core..:? "NumberOfWorkers")
          Core.<*> (x Core..:? "PredecessorRuns")
          Core.<*> (x Core..:? "PreviousRunId")
          Core.<*> (x Core..:? "SecurityConfiguration")
          Core.<*> (x Core..:? "StartedOn")
          Core.<*> (x Core..:? "Timeout")
          Core.<*> (x Core..:? "TriggerName")
          Core.<*> (x Core..:? "WorkerType")
