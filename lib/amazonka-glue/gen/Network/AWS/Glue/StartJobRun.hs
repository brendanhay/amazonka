{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartJobRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job run using a job definition.
module Network.AWS.Glue.StartJobRun
  ( -- * Creating a request
    StartJobRun (..),
    mkStartJobRun,

    -- ** Request lenses
    sjrJobName,
    sjrAllocatedCapacity,
    sjrArguments,
    sjrJobRunId,
    sjrMaxCapacity,
    sjrNotificationProperty,
    sjrNumberOfWorkers,
    sjrSecurityConfiguration,
    sjrTimeout,
    sjrWorkerType,

    -- * Destructuring the response
    StartJobRunResponse (..),
    mkStartJobRunResponse,

    -- ** Response lenses
    sjrrrsJobRunId,
    sjrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartJobRun' smart constructor.
data StartJobRun = StartJobRun'
  { -- | The name of the job definition to use.
    jobName :: Types.JobName,
    -- | This field is deprecated. Use @MaxCapacity@ instead.
    --
    -- The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    allocatedCapacity :: Core.Maybe Core.Int,
    -- | The job arguments specifically for this run. For this job run, they replace the default arguments set in the job definition itself.
    --
    -- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
    -- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
    -- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
    arguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString),
    -- | The ID of a previous @JobRun@ to retry.
    jobRunId :: Core.Maybe Types.JobRunId,
    -- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    --
    -- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
    -- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job, or an Apache Spark ETL job:
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
    -- | The name of the @SecurityConfiguration@ structure to be used with this job run.
    securityConfiguration :: Core.Maybe Types.NameString,
    -- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
    timeout :: Core.Maybe Core.Natural,
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
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartJobRun' value with any optional fields omitted.
mkStartJobRun ::
  -- | 'jobName'
  Types.JobName ->
  StartJobRun
mkStartJobRun jobName =
  StartJobRun'
    { jobName,
      allocatedCapacity = Core.Nothing,
      arguments = Core.Nothing,
      jobRunId = Core.Nothing,
      maxCapacity = Core.Nothing,
      notificationProperty = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      securityConfiguration = Core.Nothing,
      timeout = Core.Nothing,
      workerType = Core.Nothing
    }

-- | The name of the job definition to use.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrJobName :: Lens.Lens' StartJobRun Types.JobName
sjrJobName = Lens.field @"jobName"
{-# DEPRECATED sjrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrAllocatedCapacity :: Lens.Lens' StartJobRun (Core.Maybe Core.Int)
sjrAllocatedCapacity = Lens.field @"allocatedCapacity"
{-# DEPRECATED sjrAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

-- | The job arguments specifically for this run. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrArguments :: Lens.Lens' StartJobRun (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
sjrArguments = Lens.field @"arguments"
{-# DEPRECATED sjrArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

-- | The ID of a previous @JobRun@ to retry.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrJobRunId :: Lens.Lens' StartJobRun (Core.Maybe Types.JobRunId)
sjrJobRunId = Lens.field @"jobRunId"
{-# DEPRECATED sjrJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- Do not set @Max Capacity@ if using @WorkerType@ and @NumberOfWorkers@ .
-- The value that can be allocated for @MaxCapacity@ depends on whether you are running a Python shell job, or an Apache Spark ETL job:
--
--     * When you specify a Python shell job (@JobCommand.Name@ ="pythonshell"), you can allocate either 0.0625 or 1 DPU. The default is 0.0625 DPU.
--
--
--     * When you specify an Apache Spark ETL job (@JobCommand.Name@ ="glueetl"), you can allocate from 2 to 100 DPUs. The default is 10 DPUs. This job type cannot have a fractional DPU allocation.
--
--
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrMaxCapacity :: Lens.Lens' StartJobRun (Core.Maybe Core.Double)
sjrMaxCapacity = Lens.field @"maxCapacity"
{-# DEPRECATED sjrMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | Specifies configuration properties of a job run notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrNotificationProperty :: Lens.Lens' StartJobRun (Core.Maybe Types.NotificationProperty)
sjrNotificationProperty = Lens.field @"notificationProperty"
{-# DEPRECATED sjrNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrNumberOfWorkers :: Lens.Lens' StartJobRun (Core.Maybe Core.Int)
sjrNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# DEPRECATED sjrNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job run.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrSecurityConfiguration :: Lens.Lens' StartJobRun (Core.Maybe Types.NameString)
sjrSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED sjrSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrTimeout :: Lens.Lens' StartJobRun (Core.Maybe Core.Natural)
sjrTimeout = Lens.field @"timeout"
{-# DEPRECATED sjrTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

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
sjrWorkerType :: Lens.Lens' StartJobRun (Core.Maybe Types.WorkerType)
sjrWorkerType = Lens.field @"workerType"
{-# DEPRECATED sjrWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

instance Core.FromJSON StartJobRun where
  toJSON StartJobRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            ("AllocatedCapacity" Core..=) Core.<$> allocatedCapacity,
            ("Arguments" Core..=) Core.<$> arguments,
            ("JobRunId" Core..=) Core.<$> jobRunId,
            ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("NotificationProperty" Core..=) Core.<$> notificationProperty,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("SecurityConfiguration" Core..=) Core.<$> securityConfiguration,
            ("Timeout" Core..=) Core.<$> timeout,
            ("WorkerType" Core..=) Core.<$> workerType
          ]
      )

instance Core.AWSRequest StartJobRun where
  type Rs StartJobRun = StartJobRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StartJobRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartJobRunResponse'
            Core.<$> (x Core..:? "JobRunId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartJobRunResponse' smart constructor.
data StartJobRunResponse = StartJobRunResponse'
  { -- | The ID assigned to this job run.
    jobRunId :: Core.Maybe Types.JobRunId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartJobRunResponse' value with any optional fields omitted.
mkStartJobRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartJobRunResponse
mkStartJobRunResponse responseStatus =
  StartJobRunResponse' {jobRunId = Core.Nothing, responseStatus}

-- | The ID assigned to this job run.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrrsJobRunId :: Lens.Lens' StartJobRunResponse (Core.Maybe Types.JobRunId)
sjrrrsJobRunId = Lens.field @"jobRunId"
{-# DEPRECATED sjrrrsJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrrsResponseStatus :: Lens.Lens' StartJobRunResponse Core.Int
sjrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sjrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
