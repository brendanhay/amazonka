{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Job
  ( Job (..),

    -- * Smart constructor
    mkJob,

    -- * Lenses
    jNumberOfWorkers,
    jCommand,
    jNotificationProperty,
    jLastModifiedOn,
    jConnections,
    jWorkerType,
    jSecurityConfiguration,
    jGlueVersion,
    jNonOverridableArguments,
    jRole,
    jName,
    jLogURI,
    jMaxRetries,
    jExecutionProperty,
    jAllocatedCapacity,
    jMaxCapacity,
    jTimeout,
    jDefaultArguments,
    jDescription,
    jCreatedOn,
  )
where

import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a job definition.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { -- | The number of workers of a defined @workerType@ that are allocated when a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
    numberOfWorkers :: Lude.Maybe Lude.Int,
    -- | The @JobCommand@ that executes this job.
    command :: Lude.Maybe JobCommand,
    -- | Specifies configuration properties of a job notification.
    notificationProperty :: Lude.Maybe NotificationProperty,
    -- | The last point in time when this job definition was modified.
    lastModifiedOn :: Lude.Maybe Lude.Timestamp,
    -- | The connections used for this job.
    connections :: Lude.Maybe ConnectionsList,
    -- | The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.
    --
    --
    --     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
    --
    --
    --     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
    --
    --
    --     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
    workerType :: Lude.Maybe WorkerType,
    -- | The name of the @SecurityConfiguration@ structure to be used with this job.
    securityConfiguration :: Lude.Maybe Lude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
    --
    -- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
    -- Jobs that are created without specifying a Glue version default to Glue 0.9.
    glueVersion :: Lude.Maybe Lude.Text,
    -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job.
    role' :: Lude.Maybe Lude.Text,
    -- | The name you assign to this job definition.
    name :: Lude.Maybe Lude.Text,
    -- | This field is reserved for future use.
    logURI :: Lude.Maybe Lude.Text,
    -- | The maximum number of times to retry this job after a JobRun fails.
    maxRetries :: Lude.Maybe Lude.Int,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
    executionProperty :: Lude.Maybe ExecutionProperty,
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) allocated to runs of this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    allocatedCapacity :: Lude.Maybe Lude.Int,
    -- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
    -- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job, an Apache Spark ETL job, or an Apache Spark streaming ETL job:
    --
    --     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
    --
    --
    --     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
    maxCapacity :: Lude.Maybe Lude.Double,
    -- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
    timeout :: Lude.Maybe Lude.Natural,
    -- | The default arguments for this job, specified as name-value pairs.
    --
    -- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
    -- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
    -- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
    defaultArguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A description of the job.
    description :: Lude.Maybe Lude.Text,
    -- | The time and date that this job definition was created.
    createdOn :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
-- * 'command' - The @JobCommand@ that executes this job.
-- * 'notificationProperty' - Specifies configuration properties of a job notification.
-- * 'lastModifiedOn' - The last point in time when this job definition was modified.
-- * 'connections' - The connections used for this job.
-- * 'workerType' - The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
--     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this job.
-- * 'glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
-- * 'nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
-- * 'role'' - The name or Amazon Resource Name (ARN) of the IAM role associated with this job.
-- * 'name' - The name you assign to this job definition.
-- * 'logURI' - This field is reserved for future use.
-- * 'maxRetries' - The maximum number of times to retry this job after a JobRun fails.
-- * 'executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
-- * 'allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to runs of this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
-- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job, an Apache Spark ETL job, or an Apache Spark streaming ETL job:
--
--     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
--
--
--     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
--
-- * 'timeout' - The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
-- * 'defaultArguments' - The default arguments for this job, specified as name-value pairs.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
-- * 'description' - A description of the job.
-- * 'createdOn' - The time and date that this job definition was created.
mkJob ::
  Job
mkJob =
  Job'
    { numberOfWorkers = Lude.Nothing,
      command = Lude.Nothing,
      notificationProperty = Lude.Nothing,
      lastModifiedOn = Lude.Nothing,
      connections = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      glueVersion = Lude.Nothing,
      nonOverridableArguments = Lude.Nothing,
      role' = Lude.Nothing,
      name = Lude.Nothing,
      logURI = Lude.Nothing,
      maxRetries = Lude.Nothing,
      executionProperty = Lude.Nothing,
      allocatedCapacity = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      defaultArguments = Lude.Nothing,
      description = Lude.Nothing,
      createdOn = Lude.Nothing
    }

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNumberOfWorkers :: Lens.Lens' Job (Lude.Maybe Lude.Int)
jNumberOfWorkers = Lens.lens (numberOfWorkers :: Job -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: Job)
{-# DEPRECATED jNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The @JobCommand@ that executes this job.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCommand :: Lens.Lens' Job (Lude.Maybe JobCommand)
jCommand = Lens.lens (command :: Job -> Lude.Maybe JobCommand) (\s a -> s {command = a} :: Job)
{-# DEPRECATED jCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | Specifies configuration properties of a job notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNotificationProperty :: Lens.Lens' Job (Lude.Maybe NotificationProperty)
jNotificationProperty = Lens.lens (notificationProperty :: Job -> Lude.Maybe NotificationProperty) (\s a -> s {notificationProperty = a} :: Job)
{-# DEPRECATED jNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The last point in time when this job definition was modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLastModifiedOn :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jLastModifiedOn = Lens.lens (lastModifiedOn :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedOn = a} :: Job)
{-# DEPRECATED jLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | The connections used for this job.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jConnections :: Lens.Lens' Job (Lude.Maybe ConnectionsList)
jConnections = Lens.lens (connections :: Job -> Lude.Maybe ConnectionsList) (\s a -> s {connections = a} :: Job)
{-# DEPRECATED jConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

-- | The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
--     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jWorkerType :: Lens.Lens' Job (Lude.Maybe WorkerType)
jWorkerType = Lens.lens (workerType :: Job -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: Job)
{-# DEPRECATED jWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jSecurityConfiguration :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jSecurityConfiguration = Lens.lens (securityConfiguration :: Job -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: Job)
{-# DEPRECATED jSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jGlueVersion :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jGlueVersion = Lens.lens (glueVersion :: Job -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: Job)
{-# DEPRECATED jGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | Non-overridable arguments for this job, specified as name-value pairs.
--
-- /Note:/ Consider using 'nonOverridableArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNonOverridableArguments :: Lens.Lens' Job (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jNonOverridableArguments = Lens.lens (nonOverridableArguments :: Job -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {nonOverridableArguments = a} :: Job)
{-# DEPRECATED jNonOverridableArguments "Use generic-lens or generic-optics with 'nonOverridableArguments' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jRole :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jRole = Lens.lens (role' :: Job -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: Job)
{-# DEPRECATED jRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The name you assign to this job definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jName :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jName = Lens.lens (name :: Job -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Job)
{-# DEPRECATED jName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | This field is reserved for future use.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLogURI :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jLogURI = Lens.lens (logURI :: Job -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: Job)
{-# DEPRECATED jLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | The maximum number of times to retry this job after a JobRun fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jMaxRetries :: Lens.Lens' Job (Lude.Maybe Lude.Int)
jMaxRetries = Lens.lens (maxRetries :: Job -> Lude.Maybe Lude.Int) (\s a -> s {maxRetries = a} :: Job)
{-# DEPRECATED jMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
--
-- /Note:/ Consider using 'executionProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jExecutionProperty :: Lens.Lens' Job (Lude.Maybe ExecutionProperty)
jExecutionProperty = Lens.lens (executionProperty :: Job -> Lude.Maybe ExecutionProperty) (\s a -> s {executionProperty = a} :: Job)
{-# DEPRECATED jExecutionProperty "Use generic-lens or generic-optics with 'executionProperty' instead." #-}

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to runs of this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAllocatedCapacity :: Lens.Lens' Job (Lude.Maybe Lude.Int)
jAllocatedCapacity = Lens.lens (allocatedCapacity :: Job -> Lude.Maybe Lude.Int) (\s a -> s {allocatedCapacity = a} :: Job)
{-# DEPRECATED jAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
-- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job, an Apache Spark ETL job, or an Apache Spark streaming ETL job:
--
--     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
--
--
--     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
--
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jMaxCapacity :: Lens.Lens' Job (Lude.Maybe Lude.Double)
jMaxCapacity = Lens.lens (maxCapacity :: Job -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: Job)
{-# DEPRECATED jMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTimeout :: Lens.Lens' Job (Lude.Maybe Lude.Natural)
jTimeout = Lens.lens (timeout :: Job -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: Job)
{-# DEPRECATED jTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The default arguments for this job, specified as name-value pairs.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'defaultArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDefaultArguments :: Lens.Lens' Job (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jDefaultArguments = Lens.lens (defaultArguments :: Job -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {defaultArguments = a} :: Job)
{-# DEPRECATED jDefaultArguments "Use generic-lens or generic-optics with 'defaultArguments' instead." #-}

-- | A description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDescription :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jDescription = Lens.lens (description :: Job -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Job)
{-# DEPRECATED jDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The time and date that this job definition was created.
--
-- /Note:/ Consider using 'createdOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreatedOn :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jCreatedOn = Lens.lens (createdOn :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdOn = a} :: Job)
{-# DEPRECATED jCreatedOn "Use generic-lens or generic-optics with 'createdOn' instead." #-}

instance Lude.FromJSON Job where
  parseJSON =
    Lude.withObject
      "Job"
      ( \x ->
          Job'
            Lude.<$> (x Lude..:? "NumberOfWorkers")
            Lude.<*> (x Lude..:? "Command")
            Lude.<*> (x Lude..:? "NotificationProperty")
            Lude.<*> (x Lude..:? "LastModifiedOn")
            Lude.<*> (x Lude..:? "Connections")
            Lude.<*> (x Lude..:? "WorkerType")
            Lude.<*> (x Lude..:? "SecurityConfiguration")
            Lude.<*> (x Lude..:? "GlueVersion")
            Lude.<*> (x Lude..:? "NonOverridableArguments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Role")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "LogUri")
            Lude.<*> (x Lude..:? "MaxRetries")
            Lude.<*> (x Lude..:? "ExecutionProperty")
            Lude.<*> (x Lude..:? "AllocatedCapacity")
            Lude.<*> (x Lude..:? "MaxCapacity")
            Lude.<*> (x Lude..:? "Timeout")
            Lude.<*> (x Lude..:? "DefaultArguments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "CreatedOn")
      )
