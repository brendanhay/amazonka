{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Job
  ( Job (..)
  -- * Smart constructor
  , mkJob
  -- * Lenses
  , jAllocatedCapacity
  , jCommand
  , jConnections
  , jCreatedOn
  , jDefaultArguments
  , jDescription
  , jExecutionProperty
  , jGlueVersion
  , jLastModifiedOn
  , jLogUri
  , jMaxCapacity
  , jMaxRetries
  , jName
  , jNonOverridableArguments
  , jNotificationProperty
  , jNumberOfWorkers
  , jRole
  , jSecurityConfiguration
  , jTimeout
  , jWorkerType
  ) where

import qualified Network.AWS.Glue.Types.ConnectionsList as Types
import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.ExecutionProperty as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.GlueVersionString as Types
import qualified Network.AWS.Glue.Types.JobCommand as Types
import qualified Network.AWS.Glue.Types.LogUri as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.NotificationProperty as Types
import qualified Network.AWS.Glue.Types.Role as Types
import qualified Network.AWS.Glue.Types.WorkerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a job definition.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { allocatedCapacity :: Core.Maybe Core.Int
    -- ^ This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to runs of this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
  , command :: Core.Maybe Types.JobCommand
    -- ^ The @JobCommand@ that executes this job.
  , connections :: Core.Maybe Types.ConnectionsList
    -- ^ The connections used for this job.
  , createdOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The time and date that this job definition was created.
  , defaultArguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString)
    -- ^ The default arguments for this job, specified as name-value pairs.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
  , description :: Core.Maybe Types.DescriptionString
    -- ^ A description of the job.
  , executionProperty :: Core.Maybe Types.ExecutionProperty
    -- ^ An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
  , glueVersion :: Core.Maybe Types.GlueVersionString
    -- ^ Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark. 
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
  , lastModifiedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The last point in time when this job definition was modified.
  , logUri :: Core.Maybe Types.LogUri
    -- ^ This field is reserved for future use.
  , maxCapacity :: Core.Maybe Core.Double
    -- ^ The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
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
  , maxRetries :: Core.Maybe Core.Int
    -- ^ The maximum number of times to retry this job after a JobRun fails.
  , name :: Core.Maybe Types.Name
    -- ^ The name you assign to this job definition.
  , nonOverridableArguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString)
    -- ^ Non-overridable arguments for this job, specified as name-value pairs.
  , notificationProperty :: Core.Maybe Types.NotificationProperty
    -- ^ Specifies configuration properties of a job notification.
  , numberOfWorkers :: Core.Maybe Core.Int
    -- ^ The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ . 
  , role' :: Core.Maybe Types.Role
    -- ^ The name or Amazon Resource Name (ARN) of the IAM role associated with this job.
  , securityConfiguration :: Core.Maybe Types.NameString
    -- ^ The name of the @SecurityConfiguration@ structure to be used with this job.
  , timeout :: Core.Maybe Core.Natural
    -- ^ The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
  , workerType :: Core.Maybe Types.WorkerType
    -- ^ The type of predefined worker that is allocated when a job runs. Accepts a value of Standard, G.1X, or G.2X.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Job' value with any optional fields omitted.
mkJob
    :: Job
mkJob
  = Job'{allocatedCapacity = Core.Nothing, command = Core.Nothing,
         connections = Core.Nothing, createdOn = Core.Nothing,
         defaultArguments = Core.Nothing, description = Core.Nothing,
         executionProperty = Core.Nothing, glueVersion = Core.Nothing,
         lastModifiedOn = Core.Nothing, logUri = Core.Nothing,
         maxCapacity = Core.Nothing, maxRetries = Core.Nothing,
         name = Core.Nothing, nonOverridableArguments = Core.Nothing,
         notificationProperty = Core.Nothing,
         numberOfWorkers = Core.Nothing, role' = Core.Nothing,
         securityConfiguration = Core.Nothing, timeout = Core.Nothing,
         workerType = Core.Nothing}

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) allocated to runs of this job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAllocatedCapacity :: Lens.Lens' Job (Core.Maybe Core.Int)
jAllocatedCapacity = Lens.field @"allocatedCapacity"
{-# INLINEABLE jAllocatedCapacity #-}
{-# DEPRECATED allocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead"  #-}

-- | The @JobCommand@ that executes this job.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCommand :: Lens.Lens' Job (Core.Maybe Types.JobCommand)
jCommand = Lens.field @"command"
{-# INLINEABLE jCommand #-}
{-# DEPRECATED command "Use generic-lens or generic-optics with 'command' instead"  #-}

-- | The connections used for this job.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jConnections :: Lens.Lens' Job (Core.Maybe Types.ConnectionsList)
jConnections = Lens.field @"connections"
{-# INLINEABLE jConnections #-}
{-# DEPRECATED connections "Use generic-lens or generic-optics with 'connections' instead"  #-}

-- | The time and date that this job definition was created.
--
-- /Note:/ Consider using 'createdOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreatedOn :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jCreatedOn = Lens.field @"createdOn"
{-# INLINEABLE jCreatedOn #-}
{-# DEPRECATED createdOn "Use generic-lens or generic-optics with 'createdOn' instead"  #-}

-- | The default arguments for this job, specified as name-value pairs.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'defaultArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDefaultArguments :: Lens.Lens' Job (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
jDefaultArguments = Lens.field @"defaultArguments"
{-# INLINEABLE jDefaultArguments #-}
{-# DEPRECATED defaultArguments "Use generic-lens or generic-optics with 'defaultArguments' instead"  #-}

-- | A description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jDescription :: Lens.Lens' Job (Core.Maybe Types.DescriptionString)
jDescription = Lens.field @"description"
{-# INLINEABLE jDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
--
-- /Note:/ Consider using 'executionProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jExecutionProperty :: Lens.Lens' Job (Core.Maybe Types.ExecutionProperty)
jExecutionProperty = Lens.field @"executionProperty"
{-# INLINEABLE jExecutionProperty #-}
{-# DEPRECATED executionProperty "Use generic-lens or generic-optics with 'executionProperty' instead"  #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark. 
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jGlueVersion :: Lens.Lens' Job (Core.Maybe Types.GlueVersionString)
jGlueVersion = Lens.field @"glueVersion"
{-# INLINEABLE jGlueVersion #-}
{-# DEPRECATED glueVersion "Use generic-lens or generic-optics with 'glueVersion' instead"  #-}

-- | The last point in time when this job definition was modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLastModifiedOn :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jLastModifiedOn = Lens.field @"lastModifiedOn"
{-# INLINEABLE jLastModifiedOn #-}
{-# DEPRECATED lastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead"  #-}

-- | This field is reserved for future use.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLogUri :: Lens.Lens' Job (Core.Maybe Types.LogUri)
jLogUri = Lens.field @"logUri"
{-# INLINEABLE jLogUri #-}
{-# DEPRECATED logUri "Use generic-lens or generic-optics with 'logUri' instead"  #-}

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
jMaxCapacity :: Lens.Lens' Job (Core.Maybe Core.Double)
jMaxCapacity = Lens.field @"maxCapacity"
{-# INLINEABLE jMaxCapacity #-}
{-# DEPRECATED maxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead"  #-}

-- | The maximum number of times to retry this job after a JobRun fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jMaxRetries :: Lens.Lens' Job (Core.Maybe Core.Int)
jMaxRetries = Lens.field @"maxRetries"
{-# INLINEABLE jMaxRetries #-}
{-# DEPRECATED maxRetries "Use generic-lens or generic-optics with 'maxRetries' instead"  #-}

-- | The name you assign to this job definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jName :: Lens.Lens' Job (Core.Maybe Types.Name)
jName = Lens.field @"name"
{-# INLINEABLE jName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Non-overridable arguments for this job, specified as name-value pairs.
--
-- /Note:/ Consider using 'nonOverridableArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNonOverridableArguments :: Lens.Lens' Job (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
jNonOverridableArguments = Lens.field @"nonOverridableArguments"
{-# INLINEABLE jNonOverridableArguments #-}
{-# DEPRECATED nonOverridableArguments "Use generic-lens or generic-optics with 'nonOverridableArguments' instead"  #-}

-- | Specifies configuration properties of a job notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNotificationProperty :: Lens.Lens' Job (Core.Maybe Types.NotificationProperty)
jNotificationProperty = Lens.field @"notificationProperty"
{-# INLINEABLE jNotificationProperty #-}
{-# DEPRECATED notificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead"  #-}

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ . 
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNumberOfWorkers :: Lens.Lens' Job (Core.Maybe Core.Int)
jNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# INLINEABLE jNumberOfWorkers #-}
{-# DEPRECATED numberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead"  #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jRole :: Lens.Lens' Job (Core.Maybe Types.Role)
jRole = Lens.field @"role'"
{-# INLINEABLE jRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jSecurityConfiguration :: Lens.Lens' Job (Core.Maybe Types.NameString)
jSecurityConfiguration = Lens.field @"securityConfiguration"
{-# INLINEABLE jSecurityConfiguration #-}
{-# DEPRECATED securityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead"  #-}

-- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTimeout :: Lens.Lens' Job (Core.Maybe Core.Natural)
jTimeout = Lens.field @"timeout"
{-# INLINEABLE jTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

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
jWorkerType :: Lens.Lens' Job (Core.Maybe Types.WorkerType)
jWorkerType = Lens.field @"workerType"
{-# INLINEABLE jWorkerType #-}
{-# DEPRECATED workerType "Use generic-lens or generic-optics with 'workerType' instead"  #-}

instance Core.FromJSON Job where
        parseJSON
          = Core.withObject "Job" Core.$
              \ x ->
                Job' Core.<$>
                  (x Core..:? "AllocatedCapacity") Core.<*> x Core..:? "Command"
                    Core.<*> x Core..:? "Connections"
                    Core.<*> x Core..:? "CreatedOn"
                    Core.<*> x Core..:? "DefaultArguments"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "ExecutionProperty"
                    Core.<*> x Core..:? "GlueVersion"
                    Core.<*> x Core..:? "LastModifiedOn"
                    Core.<*> x Core..:? "LogUri"
                    Core.<*> x Core..:? "MaxCapacity"
                    Core.<*> x Core..:? "MaxRetries"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "NonOverridableArguments"
                    Core.<*> x Core..:? "NotificationProperty"
                    Core.<*> x Core..:? "NumberOfWorkers"
                    Core.<*> x Core..:? "Role"
                    Core.<*> x Core..:? "SecurityConfiguration"
                    Core.<*> x Core..:? "Timeout"
                    Core.<*> x Core..:? "WorkerType"
