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
    jrCompletedOn,
    jrNumberOfWorkers,
    jrTriggerName,
    jrNotificationProperty,
    jrLastModifiedOn,
    jrArguments,
    jrJobName,
    jrStartedOn,
    jrWorkerType,
    jrSecurityConfiguration,
    jrGlueVersion,
    jrJobRunState,
    jrLogGroupName,
    jrExecutionTime,
    jrPredecessorRuns,
    jrPreviousRunId,
    jrId,
    jrAttempt,
    jrAllocatedCapacity,
    jrMaxCapacity,
    jrTimeout,
    jrErrorMessage,
  )
where

import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.Predecessor
import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a job run.
--
-- /See:/ 'mkJobRun' smart constructor.
data JobRun = JobRun'
  { completedOn :: Lude.Maybe Lude.Timestamp,
    numberOfWorkers :: Lude.Maybe Lude.Int,
    triggerName :: Lude.Maybe Lude.Text,
    notificationProperty :: Lude.Maybe NotificationProperty,
    lastModifiedOn :: Lude.Maybe Lude.Timestamp,
    arguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    jobName :: Lude.Maybe Lude.Text,
    startedOn :: Lude.Maybe Lude.Timestamp,
    workerType :: Lude.Maybe WorkerType,
    securityConfiguration :: Lude.Maybe Lude.Text,
    glueVersion :: Lude.Maybe Lude.Text,
    jobRunState :: Lude.Maybe JobRunState,
    logGroupName :: Lude.Maybe Lude.Text,
    executionTime :: Lude.Maybe Lude.Int,
    predecessorRuns :: Lude.Maybe [Predecessor],
    previousRunId :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    attempt :: Lude.Maybe Lude.Int,
    allocatedCapacity :: Lude.Maybe Lude.Int,
    maxCapacity :: Lude.Maybe Lude.Double,
    timeout :: Lude.Maybe Lude.Natural,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobRun' with the minimum fields required to make a request.
--
-- * 'allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
-- * 'arguments' - The job arguments associated with this run. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
-- * 'attempt' - The number of the attempt to run this job.
-- * 'completedOn' - The date and time that this job run completed.
-- * 'errorMessage' - An error message associated with this job run.
-- * 'executionTime' - The amount of time (in seconds) that the job run consumed resources.
-- * 'glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
-- * 'id' - The ID of this job run.
-- * 'jobName' - The name of the job definition being used in this run.
-- * 'jobRunState' - The current state of the job run. For more information about the statuses of jobs that have terminated abnormally, see <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html AWS Glue Job Run Statuses> .
-- * 'lastModifiedOn' - The last time that this job run was modified.
-- * 'logGroupName' - The name of the log group for secure logging that can be server-side encrypted in Amazon CloudWatch using AWS KMS. This name can be @/aws-glue/jobs/@ , in which case the default encryption is @NONE@ . If you add a role name and @SecurityConfiguration@ name (in other words, @/aws-glue/jobs-yourRoleName-yourSecurityConfigurationName/@ ), then that security configuration is used to encrypt the log group.
-- * 'maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
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
-- * 'notificationProperty' - Specifies configuration properties of a job run notification.
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
-- * 'predecessorRuns' - A list of predecessors to this job run.
-- * 'previousRunId' - The ID of the previous run of this job. For example, the @JobRunId@ specified in the @StartJobRun@ action.
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this job run.
-- * 'startedOn' - The date and time at which this job run was started.
-- * 'timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
-- * 'triggerName' - The name of the trigger that started this job run.
-- * 'workerType' - The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
--
--
--     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
mkJobRun ::
  JobRun
mkJobRun =
  JobRun'
    { completedOn = Lude.Nothing,
      numberOfWorkers = Lude.Nothing,
      triggerName = Lude.Nothing,
      notificationProperty = Lude.Nothing,
      lastModifiedOn = Lude.Nothing,
      arguments = Lude.Nothing,
      jobName = Lude.Nothing,
      startedOn = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      glueVersion = Lude.Nothing,
      jobRunState = Lude.Nothing,
      logGroupName = Lude.Nothing,
      executionTime = Lude.Nothing,
      predecessorRuns = Lude.Nothing,
      previousRunId = Lude.Nothing,
      id = Lude.Nothing,
      attempt = Lude.Nothing,
      allocatedCapacity = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The date and time that this job run completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrCompletedOn :: Lens.Lens' JobRun (Lude.Maybe Lude.Timestamp)
jrCompletedOn = Lens.lens (completedOn :: JobRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedOn = a} :: JobRun)
{-# DEPRECATED jrCompletedOn "Use generic-lens or generic-optics with 'completedOn' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrNumberOfWorkers :: Lens.Lens' JobRun (Lude.Maybe Lude.Int)
jrNumberOfWorkers = Lens.lens (numberOfWorkers :: JobRun -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: JobRun)
{-# DEPRECATED jrNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The name of the trigger that started this job run.
--
-- /Note:/ Consider using 'triggerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrTriggerName :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrTriggerName = Lens.lens (triggerName :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {triggerName = a} :: JobRun)
{-# DEPRECATED jrTriggerName "Use generic-lens or generic-optics with 'triggerName' instead." #-}

-- | Specifies configuration properties of a job run notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrNotificationProperty :: Lens.Lens' JobRun (Lude.Maybe NotificationProperty)
jrNotificationProperty = Lens.lens (notificationProperty :: JobRun -> Lude.Maybe NotificationProperty) (\s a -> s {notificationProperty = a} :: JobRun)
{-# DEPRECATED jrNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The last time that this job run was modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLastModifiedOn :: Lens.Lens' JobRun (Lude.Maybe Lude.Timestamp)
jrLastModifiedOn = Lens.lens (lastModifiedOn :: JobRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedOn = a} :: JobRun)
{-# DEPRECATED jrLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | The job arguments associated with this run. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrArguments :: Lens.Lens' JobRun (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jrArguments = Lens.lens (arguments :: JobRun -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {arguments = a} :: JobRun)
{-# DEPRECATED jrArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

-- | The name of the job definition being used in this run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrJobName :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrJobName = Lens.lens (jobName :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: JobRun)
{-# DEPRECATED jrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The date and time at which this job run was started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrStartedOn :: Lens.Lens' JobRun (Lude.Maybe Lude.Timestamp)
jrStartedOn = Lens.lens (startedOn :: JobRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedOn = a} :: JobRun)
{-# DEPRECATED jrStartedOn "Use generic-lens or generic-optics with 'startedOn' instead." #-}

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
jrWorkerType :: Lens.Lens' JobRun (Lude.Maybe WorkerType)
jrWorkerType = Lens.lens (workerType :: JobRun -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: JobRun)
{-# DEPRECATED jrWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job run.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrSecurityConfiguration :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrSecurityConfiguration = Lens.lens (securityConfiguration :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: JobRun)
{-# DEPRECATED jrSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrGlueVersion :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrGlueVersion = Lens.lens (glueVersion :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: JobRun)
{-# DEPRECATED jrGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The current state of the job run. For more information about the statuses of jobs that have terminated abnormally, see <https://docs.aws.amazon.com/glue/latest/dg/job-run-statuses.html AWS Glue Job Run Statuses> .
--
-- /Note:/ Consider using 'jobRunState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrJobRunState :: Lens.Lens' JobRun (Lude.Maybe JobRunState)
jrJobRunState = Lens.lens (jobRunState :: JobRun -> Lude.Maybe JobRunState) (\s a -> s {jobRunState = a} :: JobRun)
{-# DEPRECATED jrJobRunState "Use generic-lens or generic-optics with 'jobRunState' instead." #-}

-- | The name of the log group for secure logging that can be server-side encrypted in Amazon CloudWatch using AWS KMS. This name can be @/aws-glue/jobs/@ , in which case the default encryption is @NONE@ . If you add a role name and @SecurityConfiguration@ name (in other words, @/aws-glue/jobs-yourRoleName-yourSecurityConfigurationName/@ ), then that security configuration is used to encrypt the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrLogGroupName :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrLogGroupName = Lens.lens (logGroupName :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: JobRun)
{-# DEPRECATED jrLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The amount of time (in seconds) that the job run consumed resources.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrExecutionTime :: Lens.Lens' JobRun (Lude.Maybe Lude.Int)
jrExecutionTime = Lens.lens (executionTime :: JobRun -> Lude.Maybe Lude.Int) (\s a -> s {executionTime = a} :: JobRun)
{-# DEPRECATED jrExecutionTime "Use generic-lens or generic-optics with 'executionTime' instead." #-}

-- | A list of predecessors to this job run.
--
-- /Note:/ Consider using 'predecessorRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrPredecessorRuns :: Lens.Lens' JobRun (Lude.Maybe [Predecessor])
jrPredecessorRuns = Lens.lens (predecessorRuns :: JobRun -> Lude.Maybe [Predecessor]) (\s a -> s {predecessorRuns = a} :: JobRun)
{-# DEPRECATED jrPredecessorRuns "Use generic-lens or generic-optics with 'predecessorRuns' instead." #-}

-- | The ID of the previous run of this job. For example, the @JobRunId@ specified in the @StartJobRun@ action.
--
-- /Note:/ Consider using 'previousRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrPreviousRunId :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrPreviousRunId = Lens.lens (previousRunId :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {previousRunId = a} :: JobRun)
{-# DEPRECATED jrPreviousRunId "Use generic-lens or generic-optics with 'previousRunId' instead." #-}

-- | The ID of this job run.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrId :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrId = Lens.lens (id :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: JobRun)
{-# DEPRECATED jrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The number of the attempt to run this job.
--
-- /Note:/ Consider using 'attempt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrAttempt :: Lens.Lens' JobRun (Lude.Maybe Lude.Int)
jrAttempt = Lens.lens (attempt :: JobRun -> Lude.Maybe Lude.Int) (\s a -> s {attempt = a} :: JobRun)
{-# DEPRECATED jrAttempt "Use generic-lens or generic-optics with 'attempt' instead." #-}

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrAllocatedCapacity :: Lens.Lens' JobRun (Lude.Maybe Lude.Int)
jrAllocatedCapacity = Lens.lens (allocatedCapacity :: JobRun -> Lude.Maybe Lude.Int) (\s a -> s {allocatedCapacity = a} :: JobRun)
{-# DEPRECATED jrAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

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
jrMaxCapacity :: Lens.Lens' JobRun (Lude.Maybe Lude.Double)
jrMaxCapacity = Lens.lens (maxCapacity :: JobRun -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: JobRun)
{-# DEPRECATED jrMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrTimeout :: Lens.Lens' JobRun (Lude.Maybe Lude.Natural)
jrTimeout = Lens.lens (timeout :: JobRun -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: JobRun)
{-# DEPRECATED jrTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | An error message associated with this job run.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jrErrorMessage :: Lens.Lens' JobRun (Lude.Maybe Lude.Text)
jrErrorMessage = Lens.lens (errorMessage :: JobRun -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: JobRun)
{-# DEPRECATED jrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON JobRun where
  parseJSON =
    Lude.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Lude.<$> (x Lude..:? "CompletedOn")
            Lude.<*> (x Lude..:? "NumberOfWorkers")
            Lude.<*> (x Lude..:? "TriggerName")
            Lude.<*> (x Lude..:? "NotificationProperty")
            Lude.<*> (x Lude..:? "LastModifiedOn")
            Lude.<*> (x Lude..:? "Arguments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "StartedOn")
            Lude.<*> (x Lude..:? "WorkerType")
            Lude.<*> (x Lude..:? "SecurityConfiguration")
            Lude.<*> (x Lude..:? "GlueVersion")
            Lude.<*> (x Lude..:? "JobRunState")
            Lude.<*> (x Lude..:? "LogGroupName")
            Lude.<*> (x Lude..:? "ExecutionTime")
            Lude.<*> (x Lude..:? "PredecessorRuns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PreviousRunId")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Attempt")
            Lude.<*> (x Lude..:? "AllocatedCapacity")
            Lude.<*> (x Lude..:? "MaxCapacity")
            Lude.<*> (x Lude..:? "Timeout")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
