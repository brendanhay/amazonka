{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobUpdate
  ( JobUpdate (..),

    -- * Smart constructor
    mkJobUpdate,

    -- * Lenses
    juNumberOfWorkers,
    juCommand,
    juNotificationProperty,
    juConnections,
    juWorkerType,
    juSecurityConfiguration,
    juGlueVersion,
    juNonOverridableArguments,
    juRole,
    juLogURI,
    juMaxRetries,
    juExecutionProperty,
    juAllocatedCapacity,
    juMaxCapacity,
    juTimeout,
    juDefaultArguments,
    juDescription,
  )
where

import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies information used to update an existing job definition. The previous job definition is completely overwritten by this information.
--
-- /See:/ 'mkJobUpdate' smart constructor.
data JobUpdate = JobUpdate'
  { numberOfWorkers :: Lude.Maybe Lude.Int,
    command :: Lude.Maybe JobCommand,
    notificationProperty :: Lude.Maybe NotificationProperty,
    connections :: Lude.Maybe ConnectionsList,
    workerType :: Lude.Maybe WorkerType,
    securityConfiguration :: Lude.Maybe Lude.Text,
    glueVersion :: Lude.Maybe Lude.Text,
    nonOverridableArguments ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    role' :: Lude.Maybe Lude.Text,
    logURI :: Lude.Maybe Lude.Text,
    maxRetries :: Lude.Maybe Lude.Int,
    executionProperty :: Lude.Maybe ExecutionProperty,
    allocatedCapacity :: Lude.Maybe Lude.Int,
    maxCapacity :: Lude.Maybe Lude.Double,
    timeout :: Lude.Maybe Lude.Natural,
    defaultArguments ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobUpdate' with the minimum fields required to make a request.
--
-- * 'allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
-- * 'command' - The @JobCommand@ that executes this job (required).
-- * 'connections' - The connections used for this job.
-- * 'defaultArguments' - The default arguments for this job.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
-- * 'description' - Description of the job being defined.
-- * 'executionProperty' - An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
-- * 'glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- * 'logURI' - This field is reserved for future use.
-- * 'maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
-- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job or an Apache Spark ETL job:
--
--     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
--
--
--     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
--
-- * 'maxRetries' - The maximum number of times to retry this job if it fails.
-- * 'nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
-- * 'notificationProperty' - Specifies the configuration properties of a job notification.
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
-- * 'role'' - The name or Amazon Resource Name (ARN) of the IAM role associated with this job (required).
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this job.
-- * 'timeout' - The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
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
mkJobUpdate ::
  JobUpdate
mkJobUpdate =
  JobUpdate'
    { numberOfWorkers = Lude.Nothing,
      command = Lude.Nothing,
      notificationProperty = Lude.Nothing,
      connections = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      glueVersion = Lude.Nothing,
      nonOverridableArguments = Lude.Nothing,
      role' = Lude.Nothing,
      logURI = Lude.Nothing,
      maxRetries = Lude.Nothing,
      executionProperty = Lude.Nothing,
      allocatedCapacity = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      defaultArguments = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juNumberOfWorkers :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Int)
juNumberOfWorkers = Lens.lens (numberOfWorkers :: JobUpdate -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: JobUpdate)
{-# DEPRECATED juNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The @JobCommand@ that executes this job (required).
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juCommand :: Lens.Lens' JobUpdate (Lude.Maybe JobCommand)
juCommand = Lens.lens (command :: JobUpdate -> Lude.Maybe JobCommand) (\s a -> s {command = a} :: JobUpdate)
{-# DEPRECATED juCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | Specifies the configuration properties of a job notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juNotificationProperty :: Lens.Lens' JobUpdate (Lude.Maybe NotificationProperty)
juNotificationProperty = Lens.lens (notificationProperty :: JobUpdate -> Lude.Maybe NotificationProperty) (\s a -> s {notificationProperty = a} :: JobUpdate)
{-# DEPRECATED juNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The connections used for this job.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juConnections :: Lens.Lens' JobUpdate (Lude.Maybe ConnectionsList)
juConnections = Lens.lens (connections :: JobUpdate -> Lude.Maybe ConnectionsList) (\s a -> s {connections = a} :: JobUpdate)
{-# DEPRECATED juConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

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
juWorkerType :: Lens.Lens' JobUpdate (Lude.Maybe WorkerType)
juWorkerType = Lens.lens (workerType :: JobUpdate -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: JobUpdate)
{-# DEPRECATED juWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juSecurityConfiguration :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Text)
juSecurityConfiguration = Lens.lens (securityConfiguration :: JobUpdate -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: JobUpdate)
{-# DEPRECATED juSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juGlueVersion :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Text)
juGlueVersion = Lens.lens (glueVersion :: JobUpdate -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: JobUpdate)
{-# DEPRECATED juGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | Non-overridable arguments for this job, specified as name-value pairs.
--
-- /Note:/ Consider using 'nonOverridableArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juNonOverridableArguments :: Lens.Lens' JobUpdate (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
juNonOverridableArguments = Lens.lens (nonOverridableArguments :: JobUpdate -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {nonOverridableArguments = a} :: JobUpdate)
{-# DEPRECATED juNonOverridableArguments "Use generic-lens or generic-optics with 'nonOverridableArguments' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job (required).
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juRole :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Text)
juRole = Lens.lens (role' :: JobUpdate -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: JobUpdate)
{-# DEPRECATED juRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | This field is reserved for future use.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juLogURI :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Text)
juLogURI = Lens.lens (logURI :: JobUpdate -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: JobUpdate)
{-# DEPRECATED juLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | The maximum number of times to retry this job if it fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juMaxRetries :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Int)
juMaxRetries = Lens.lens (maxRetries :: JobUpdate -> Lude.Maybe Lude.Int) (\s a -> s {maxRetries = a} :: JobUpdate)
{-# DEPRECATED juMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
--
-- /Note:/ Consider using 'executionProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juExecutionProperty :: Lens.Lens' JobUpdate (Lude.Maybe ExecutionProperty)
juExecutionProperty = Lens.lens (executionProperty :: JobUpdate -> Lude.Maybe ExecutionProperty) (\s a -> s {executionProperty = a} :: JobUpdate)
{-# DEPRECATED juExecutionProperty "Use generic-lens or generic-optics with 'executionProperty' instead." #-}

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juAllocatedCapacity :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Int)
juAllocatedCapacity = Lens.lens (allocatedCapacity :: JobUpdate -> Lude.Maybe Lude.Int) (\s a -> s {allocatedCapacity = a} :: JobUpdate)
{-# DEPRECATED juAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
-- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job or an Apache Spark ETL job:
--
--     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
--
--
--     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
--
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juMaxCapacity :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Double)
juMaxCapacity = Lens.lens (maxCapacity :: JobUpdate -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: JobUpdate)
{-# DEPRECATED juMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juTimeout :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Natural)
juTimeout = Lens.lens (timeout :: JobUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: JobUpdate)
{-# DEPRECATED juTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The default arguments for this job.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'defaultArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juDefaultArguments :: Lens.Lens' JobUpdate (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
juDefaultArguments = Lens.lens (defaultArguments :: JobUpdate -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {defaultArguments = a} :: JobUpdate)
{-# DEPRECATED juDefaultArguments "Use generic-lens or generic-optics with 'defaultArguments' instead." #-}

-- | Description of the job being defined.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juDescription :: Lens.Lens' JobUpdate (Lude.Maybe Lude.Text)
juDescription = Lens.lens (description :: JobUpdate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: JobUpdate)
{-# DEPRECATED juDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToJSON JobUpdate where
  toJSON JobUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberOfWorkers" Lude..=) Lude.<$> numberOfWorkers,
            ("Command" Lude..=) Lude.<$> command,
            ("NotificationProperty" Lude..=) Lude.<$> notificationProperty,
            ("Connections" Lude..=) Lude.<$> connections,
            ("WorkerType" Lude..=) Lude.<$> workerType,
            ("SecurityConfiguration" Lude..=) Lude.<$> securityConfiguration,
            ("GlueVersion" Lude..=) Lude.<$> glueVersion,
            ("NonOverridableArguments" Lude..=)
              Lude.<$> nonOverridableArguments,
            ("Role" Lude..=) Lude.<$> role',
            ("LogUri" Lude..=) Lude.<$> logURI,
            ("MaxRetries" Lude..=) Lude.<$> maxRetries,
            ("ExecutionProperty" Lude..=) Lude.<$> executionProperty,
            ("AllocatedCapacity" Lude..=) Lude.<$> allocatedCapacity,
            ("MaxCapacity" Lude..=) Lude.<$> maxCapacity,
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("DefaultArguments" Lude..=) Lude.<$> defaultArguments,
            ("Description" Lude..=) Lude.<$> description
          ]
      )
