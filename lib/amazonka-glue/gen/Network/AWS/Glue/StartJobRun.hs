{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    sjrNumberOfWorkers,
    sjrNotificationProperty,
    sjrArguments,
    sjrWorkerType,
    sjrSecurityConfiguration,
    sjrAllocatedCapacity,
    sjrMaxCapacity,
    sjrTimeout,
    sjrJobRunId,
    sjrJobName,

    -- * Destructuring the response
    StartJobRunResponse (..),
    mkStartJobRunResponse,

    -- ** Response lenses
    sjrrsJobRunId,
    sjrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartJobRun' smart constructor.
data StartJobRun = StartJobRun'
  { numberOfWorkers ::
      Lude.Maybe Lude.Int,
    notificationProperty :: Lude.Maybe NotificationProperty,
    arguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    workerType :: Lude.Maybe WorkerType,
    securityConfiguration :: Lude.Maybe Lude.Text,
    allocatedCapacity :: Lude.Maybe Lude.Int,
    maxCapacity :: Lude.Maybe Lude.Double,
    timeout :: Lude.Maybe Lude.Natural,
    jobRunId :: Lude.Maybe Lude.Text,
    jobName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartJobRun' with the minimum fields required to make a request.
--
-- * 'allocatedCapacity' - This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
-- * 'arguments' - The job arguments specifically for this run. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
-- * 'jobName' - The name of the job definition to use.
-- * 'jobRunId' - The ID of a previous @JobRun@ to retry.
-- * 'maxCapacity' - The number of AWS Glue data processing units (DPUs) that can be allocated when this job runs. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
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
-- * 'notificationProperty' - Specifies configuration properties of a job run notification.
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this job run.
-- * 'timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
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
mkStartJobRun ::
  -- | 'jobName'
  Lude.Text ->
  StartJobRun
mkStartJobRun pJobName_ =
  StartJobRun'
    { numberOfWorkers = Lude.Nothing,
      notificationProperty = Lude.Nothing,
      arguments = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      allocatedCapacity = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      jobRunId = Lude.Nothing,
      jobName = pJobName_
    }

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrNumberOfWorkers :: Lens.Lens' StartJobRun (Lude.Maybe Lude.Int)
sjrNumberOfWorkers = Lens.lens (numberOfWorkers :: StartJobRun -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: StartJobRun)
{-# DEPRECATED sjrNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | Specifies configuration properties of a job run notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrNotificationProperty :: Lens.Lens' StartJobRun (Lude.Maybe NotificationProperty)
sjrNotificationProperty = Lens.lens (notificationProperty :: StartJobRun -> Lude.Maybe NotificationProperty) (\s a -> s {notificationProperty = a} :: StartJobRun)
{-# DEPRECATED sjrNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The job arguments specifically for this run. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrArguments :: Lens.Lens' StartJobRun (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sjrArguments = Lens.lens (arguments :: StartJobRun -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {arguments = a} :: StartJobRun)
{-# DEPRECATED sjrArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

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
sjrWorkerType :: Lens.Lens' StartJobRun (Lude.Maybe WorkerType)
sjrWorkerType = Lens.lens (workerType :: StartJobRun -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: StartJobRun)
{-# DEPRECATED sjrWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job run.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrSecurityConfiguration :: Lens.Lens' StartJobRun (Lude.Maybe Lude.Text)
sjrSecurityConfiguration = Lens.lens (securityConfiguration :: StartJobRun -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: StartJobRun)
{-# DEPRECATED sjrSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | This field is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://docs.aws.amazon.com/https:/aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrAllocatedCapacity :: Lens.Lens' StartJobRun (Lude.Maybe Lude.Int)
sjrAllocatedCapacity = Lens.lens (allocatedCapacity :: StartJobRun -> Lude.Maybe Lude.Int) (\s a -> s {allocatedCapacity = a} :: StartJobRun)
{-# DEPRECATED sjrAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

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
sjrMaxCapacity :: Lens.Lens' StartJobRun (Lude.Maybe Lude.Double)
sjrMaxCapacity = Lens.lens (maxCapacity :: StartJobRun -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: StartJobRun)
{-# DEPRECATED sjrMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrTimeout :: Lens.Lens' StartJobRun (Lude.Maybe Lude.Natural)
sjrTimeout = Lens.lens (timeout :: StartJobRun -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: StartJobRun)
{-# DEPRECATED sjrTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The ID of a previous @JobRun@ to retry.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrJobRunId :: Lens.Lens' StartJobRun (Lude.Maybe Lude.Text)
sjrJobRunId = Lens.lens (jobRunId :: StartJobRun -> Lude.Maybe Lude.Text) (\s a -> s {jobRunId = a} :: StartJobRun)
{-# DEPRECATED sjrJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

-- | The name of the job definition to use.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrJobName :: Lens.Lens' StartJobRun Lude.Text
sjrJobName = Lens.lens (jobName :: StartJobRun -> Lude.Text) (\s a -> s {jobName = a} :: StartJobRun)
{-# DEPRECATED sjrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

instance Lude.AWSRequest StartJobRun where
  type Rs StartJobRun = StartJobRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartJobRunResponse'
            Lude.<$> (x Lude..?> "JobRunId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartJobRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.StartJobRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartJobRun where
  toJSON StartJobRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberOfWorkers" Lude..=) Lude.<$> numberOfWorkers,
            ("NotificationProperty" Lude..=) Lude.<$> notificationProperty,
            ("Arguments" Lude..=) Lude.<$> arguments,
            ("WorkerType" Lude..=) Lude.<$> workerType,
            ("SecurityConfiguration" Lude..=) Lude.<$> securityConfiguration,
            ("AllocatedCapacity" Lude..=) Lude.<$> allocatedCapacity,
            ("MaxCapacity" Lude..=) Lude.<$> maxCapacity,
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("JobRunId" Lude..=) Lude.<$> jobRunId,
            Lude.Just ("JobName" Lude..= jobName)
          ]
      )

instance Lude.ToPath StartJobRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartJobRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartJobRunResponse' smart constructor.
data StartJobRunResponse = StartJobRunResponse'
  { jobRunId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartJobRunResponse' with the minimum fields required to make a request.
--
-- * 'jobRunId' - The ID assigned to this job run.
-- * 'responseStatus' - The response status code.
mkStartJobRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartJobRunResponse
mkStartJobRunResponse pResponseStatus_ =
  StartJobRunResponse'
    { jobRunId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID assigned to this job run.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsJobRunId :: Lens.Lens' StartJobRunResponse (Lude.Maybe Lude.Text)
sjrrsJobRunId = Lens.lens (jobRunId :: StartJobRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobRunId = a} :: StartJobRunResponse)
{-# DEPRECATED sjrrsJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsResponseStatus :: Lens.Lens' StartJobRunResponse Lude.Int
sjrrsResponseStatus = Lens.lens (responseStatus :: StartJobRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartJobRunResponse)
{-# DEPRECATED sjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
