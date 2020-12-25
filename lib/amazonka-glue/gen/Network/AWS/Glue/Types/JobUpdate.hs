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
    juAllocatedCapacity,
    juCommand,
    juConnections,
    juDefaultArguments,
    juDescription,
    juExecutionProperty,
    juGlueVersion,
    juLogUri,
    juMaxCapacity,
    juMaxRetries,
    juNonOverridableArguments,
    juNotificationProperty,
    juNumberOfWorkers,
    juRole,
    juSecurityConfiguration,
    juTimeout,
    juWorkerType,
  )
where

import qualified Network.AWS.Glue.Types.ConnectionsList as Types
import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.ExecutionProperty as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.GlueVersionString as Types
import qualified Network.AWS.Glue.Types.JobCommand as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.NotificationProperty as Types
import qualified Network.AWS.Glue.Types.Role as Types
import qualified Network.AWS.Glue.Types.UriString as Types
import qualified Network.AWS.Glue.Types.WorkerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies information used to update an existing job definition. The previous job definition is completely overwritten by this information.
--
-- /See:/ 'mkJobUpdate' smart constructor.
data JobUpdate = JobUpdate'
  { -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) to allocate to this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    allocatedCapacity :: Core.Maybe Core.Int,
    -- | The @JobCommand@ that executes this job (required).
    command :: Core.Maybe Types.JobCommand,
    -- | The connections used for this job.
    connections :: Core.Maybe Types.ConnectionsList,
    -- | The default arguments for this job.
    --
    -- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
    -- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
    -- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
    defaultArguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString),
    -- | Description of the job being defined.
    description :: Core.Maybe Types.DescriptionString,
    -- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
    executionProperty :: Core.Maybe Types.ExecutionProperty,
    -- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
    --
    -- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
    glueVersion :: Core.Maybe Types.GlueVersionString,
    -- | This field is reserved for future use.
    logUri :: Core.Maybe Types.UriString,
    -- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
    -- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job or an Apache Spark ETL job:
    --
    --     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
    --
    --
    --     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl") or Apache Spark streaming ETL job (@JobCommand.Name@ ="gluestreaming"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
    maxCapacity :: Core.Maybe Core.Double,
    -- | The maximum number of times to retry this job if it fails.
    maxRetries :: Core.Maybe Core.Int,
    -- | Non-overridable arguments for this job, specified as name-value pairs.
    nonOverridableArguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString),
    -- | Specifies the configuration properties of a job notification.
    notificationProperty :: Core.Maybe Types.NotificationProperty,
    -- | The number of workers of a defined @workerType@ that are allocated when a job runs.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job (required).
    role' :: Core.Maybe Types.Role,
    -- | The name of the @SecurityConfiguration@ structure to be used with this job.
    securityConfiguration :: Core.Maybe Types.NameString,
    -- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
    timeout :: Core.Maybe Core.Natural,
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
    workerType :: Core.Maybe Types.WorkerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobUpdate' value with any optional fields omitted.
mkJobUpdate ::
  JobUpdate
mkJobUpdate =
  JobUpdate'
    { allocatedCapacity = Core.Nothing,
      command = Core.Nothing,
      connections = Core.Nothing,
      defaultArguments = Core.Nothing,
      description = Core.Nothing,
      executionProperty = Core.Nothing,
      glueVersion = Core.Nothing,
      logUri = Core.Nothing,
      maxCapacity = Core.Nothing,
      maxRetries = Core.Nothing,
      nonOverridableArguments = Core.Nothing,
      notificationProperty = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      role' = Core.Nothing,
      securityConfiguration = Core.Nothing,
      timeout = Core.Nothing,
      workerType = Core.Nothing
    }

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juAllocatedCapacity :: Lens.Lens' JobUpdate (Core.Maybe Core.Int)
juAllocatedCapacity = Lens.field @"allocatedCapacity"
{-# DEPRECATED juAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

-- | The @JobCommand@ that executes this job (required).
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juCommand :: Lens.Lens' JobUpdate (Core.Maybe Types.JobCommand)
juCommand = Lens.field @"command"
{-# DEPRECATED juCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The connections used for this job.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juConnections :: Lens.Lens' JobUpdate (Core.Maybe Types.ConnectionsList)
juConnections = Lens.field @"connections"
{-# DEPRECATED juConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

-- | The default arguments for this job.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'defaultArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juDefaultArguments :: Lens.Lens' JobUpdate (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
juDefaultArguments = Lens.field @"defaultArguments"
{-# DEPRECATED juDefaultArguments "Use generic-lens or generic-optics with 'defaultArguments' instead." #-}

-- | Description of the job being defined.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juDescription :: Lens.Lens' JobUpdate (Core.Maybe Types.DescriptionString)
juDescription = Lens.field @"description"
{-# DEPRECATED juDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
--
-- /Note:/ Consider using 'executionProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juExecutionProperty :: Lens.Lens' JobUpdate (Core.Maybe Types.ExecutionProperty)
juExecutionProperty = Lens.field @"executionProperty"
{-# DEPRECATED juExecutionProperty "Use generic-lens or generic-optics with 'executionProperty' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juGlueVersion :: Lens.Lens' JobUpdate (Core.Maybe Types.GlueVersionString)
juGlueVersion = Lens.field @"glueVersion"
{-# DEPRECATED juGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | This field is reserved for future use.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juLogUri :: Lens.Lens' JobUpdate (Core.Maybe Types.UriString)
juLogUri = Lens.field @"logUri"
{-# DEPRECATED juLogUri "Use generic-lens or generic-optics with 'logUri' instead." #-}

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
juMaxCapacity :: Lens.Lens' JobUpdate (Core.Maybe Core.Double)
juMaxCapacity = Lens.field @"maxCapacity"
{-# DEPRECATED juMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The maximum number of times to retry this job if it fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juMaxRetries :: Lens.Lens' JobUpdate (Core.Maybe Core.Int)
juMaxRetries = Lens.field @"maxRetries"
{-# DEPRECATED juMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | Non-overridable arguments for this job, specified as name-value pairs.
--
-- /Note:/ Consider using 'nonOverridableArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juNonOverridableArguments :: Lens.Lens' JobUpdate (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
juNonOverridableArguments = Lens.field @"nonOverridableArguments"
{-# DEPRECATED juNonOverridableArguments "Use generic-lens or generic-optics with 'nonOverridableArguments' instead." #-}

-- | Specifies the configuration properties of a job notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juNotificationProperty :: Lens.Lens' JobUpdate (Core.Maybe Types.NotificationProperty)
juNotificationProperty = Lens.field @"notificationProperty"
{-# DEPRECATED juNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juNumberOfWorkers :: Lens.Lens' JobUpdate (Core.Maybe Core.Int)
juNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# DEPRECATED juNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job (required).
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juRole :: Lens.Lens' JobUpdate (Core.Maybe Types.Role)
juRole = Lens.field @"role'"
{-# DEPRECATED juRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juSecurityConfiguration :: Lens.Lens' JobUpdate (Core.Maybe Types.NameString)
juSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED juSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
juTimeout :: Lens.Lens' JobUpdate (Core.Maybe Core.Natural)
juTimeout = Lens.field @"timeout"
{-# DEPRECATED juTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

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
juWorkerType :: Lens.Lens' JobUpdate (Core.Maybe Types.WorkerType)
juWorkerType = Lens.field @"workerType"
{-# DEPRECATED juWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

instance Core.FromJSON JobUpdate where
  toJSON JobUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllocatedCapacity" Core..=) Core.<$> allocatedCapacity,
            ("Command" Core..=) Core.<$> command,
            ("Connections" Core..=) Core.<$> connections,
            ("DefaultArguments" Core..=) Core.<$> defaultArguments,
            ("Description" Core..=) Core.<$> description,
            ("ExecutionProperty" Core..=) Core.<$> executionProperty,
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("LogUri" Core..=) Core.<$> logUri,
            ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("MaxRetries" Core..=) Core.<$> maxRetries,
            ("NonOverridableArguments" Core..=)
              Core.<$> nonOverridableArguments,
            ("NotificationProperty" Core..=) Core.<$> notificationProperty,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("Role" Core..=) Core.<$> role',
            ("SecurityConfiguration" Core..=) Core.<$> securityConfiguration,
            ("Timeout" Core..=) Core.<$> timeout,
            ("WorkerType" Core..=) Core.<$> workerType
          ]
      )
