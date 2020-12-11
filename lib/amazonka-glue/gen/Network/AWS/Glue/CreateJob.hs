{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new job definition.
module Network.AWS.Glue.CreateJob
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjNumberOfWorkers,
    cjNotificationProperty,
    cjConnections,
    cjWorkerType,
    cjSecurityConfiguration,
    cjGlueVersion,
    cjNonOverridableArguments,
    cjLogURI,
    cjMaxRetries,
    cjExecutionProperty,
    cjAllocatedCapacity,
    cjMaxCapacity,
    cjTimeout,
    cjDefaultArguments,
    cjDescription,
    cjTags,
    cjName,
    cjRole,
    cjCommand,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrsName,
    cjrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { numberOfWorkers :: Lude.Maybe Lude.Int,
    notificationProperty :: Lude.Maybe NotificationProperty,
    connections :: Lude.Maybe ConnectionsList,
    workerType :: Lude.Maybe WorkerType,
    securityConfiguration :: Lude.Maybe Lude.Text,
    glueVersion :: Lude.Maybe Lude.Text,
    nonOverridableArguments ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    logURI :: Lude.Maybe Lude.Text,
    maxRetries :: Lude.Maybe Lude.Int,
    executionProperty :: Lude.Maybe ExecutionProperty,
    allocatedCapacity :: Lude.Maybe Lude.Int,
    maxCapacity :: Lude.Maybe Lude.Double,
    timeout :: Lude.Maybe Lude.Natural,
    defaultArguments ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Text,
    role' :: Lude.Text,
    command :: JobCommand
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- * 'allocatedCapacity' - This parameter is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this Job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
-- * 'command' - The @JobCommand@ that executes this job.
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
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
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
-- * 'name' - The name you assign to this job definition. It must be unique in your account.
-- * 'nonOverridableArguments' - Non-overridable arguments for this job, specified as name-value pairs.
-- * 'notificationProperty' - Specifies configuration properties of a job notification.
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
-- * 'role'' - The name or Amazon Resource Name (ARN) of the IAM role associated with this job.
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this job.
-- * 'tags' - The tags to use with this job. You may use tags to limit access to the job. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
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
mkCreateJob ::
  -- | 'name'
  Lude.Text ->
  -- | 'role''
  Lude.Text ->
  -- | 'command'
  JobCommand ->
  CreateJob
mkCreateJob pName_ pRole_ pCommand_ =
  CreateJob'
    { numberOfWorkers = Lude.Nothing,
      notificationProperty = Lude.Nothing,
      connections = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      glueVersion = Lude.Nothing,
      nonOverridableArguments = Lude.Nothing,
      logURI = Lude.Nothing,
      maxRetries = Lude.Nothing,
      executionProperty = Lude.Nothing,
      allocatedCapacity = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      defaultArguments = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      role' = pRole_,
      command = pCommand_
    }

-- | The number of workers of a defined @workerType@ that are allocated when a job runs.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjNumberOfWorkers :: Lens.Lens' CreateJob (Lude.Maybe Lude.Int)
cjNumberOfWorkers = Lens.lens (numberOfWorkers :: CreateJob -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: CreateJob)
{-# DEPRECATED cjNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | Specifies configuration properties of a job notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjNotificationProperty :: Lens.Lens' CreateJob (Lude.Maybe NotificationProperty)
cjNotificationProperty = Lens.lens (notificationProperty :: CreateJob -> Lude.Maybe NotificationProperty) (\s a -> s {notificationProperty = a} :: CreateJob)
{-# DEPRECATED cjNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The connections used for this job.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjConnections :: Lens.Lens' CreateJob (Lude.Maybe ConnectionsList)
cjConnections = Lens.lens (connections :: CreateJob -> Lude.Maybe ConnectionsList) (\s a -> s {connections = a} :: CreateJob)
{-# DEPRECATED cjConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

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
cjWorkerType :: Lens.Lens' CreateJob (Lude.Maybe WorkerType)
cjWorkerType = Lens.lens (workerType :: CreateJob -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: CreateJob)
{-# DEPRECATED cjWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this job.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSecurityConfiguration :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjSecurityConfiguration = Lens.lens (securityConfiguration :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: CreateJob)
{-# DEPRECATED cjSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for jobs of type Spark.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Jobs that are created without specifying a Glue version default to Glue 0.9.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjGlueVersion :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjGlueVersion = Lens.lens (glueVersion :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: CreateJob)
{-# DEPRECATED cjGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | Non-overridable arguments for this job, specified as name-value pairs.
--
-- /Note:/ Consider using 'nonOverridableArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjNonOverridableArguments :: Lens.Lens' CreateJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjNonOverridableArguments = Lens.lens (nonOverridableArguments :: CreateJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {nonOverridableArguments = a} :: CreateJob)
{-# DEPRECATED cjNonOverridableArguments "Use generic-lens or generic-optics with 'nonOverridableArguments' instead." #-}

-- | This field is reserved for future use.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjLogURI :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjLogURI = Lens.lens (logURI :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: CreateJob)
{-# DEPRECATED cjLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | The maximum number of times to retry this job if it fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjMaxRetries :: Lens.Lens' CreateJob (Lude.Maybe Lude.Int)
cjMaxRetries = Lens.lens (maxRetries :: CreateJob -> Lude.Maybe Lude.Int) (\s a -> s {maxRetries = a} :: CreateJob)
{-# DEPRECATED cjMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | An @ExecutionProperty@ specifying the maximum number of concurrent runs allowed for this job.
--
-- /Note:/ Consider using 'executionProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjExecutionProperty :: Lens.Lens' CreateJob (Lude.Maybe ExecutionProperty)
cjExecutionProperty = Lens.lens (executionProperty :: CreateJob -> Lude.Maybe ExecutionProperty) (\s a -> s {executionProperty = a} :: CreateJob)
{-# DEPRECATED cjExecutionProperty "Use generic-lens or generic-optics with 'executionProperty' instead." #-}

-- | This parameter is deprecated. Use @MaxCapacity@ instead.
--
-- The number of AWS Glue data processing units (DPUs) to allocate to this Job. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- /Note:/ Consider using 'allocatedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAllocatedCapacity :: Lens.Lens' CreateJob (Lude.Maybe Lude.Int)
cjAllocatedCapacity = Lens.lens (allocatedCapacity :: CreateJob -> Lude.Maybe Lude.Int) (\s a -> s {allocatedCapacity = a} :: CreateJob)
{-# DEPRECATED cjAllocatedCapacity "Use generic-lens or generic-optics with 'allocatedCapacity' instead." #-}

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
cjMaxCapacity :: Lens.Lens' CreateJob (Lude.Maybe Lude.Double)
cjMaxCapacity = Lens.lens (maxCapacity :: CreateJob -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: CreateJob)
{-# DEPRECATED cjMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The job timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTimeout :: Lens.Lens' CreateJob (Lude.Maybe Lude.Natural)
cjTimeout = Lens.lens (timeout :: CreateJob -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: CreateJob)
{-# DEPRECATED cjTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The default arguments for this job.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'defaultArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDefaultArguments :: Lens.Lens' CreateJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjDefaultArguments = Lens.lens (defaultArguments :: CreateJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {defaultArguments = a} :: CreateJob)
{-# DEPRECATED cjDefaultArguments "Use generic-lens or generic-optics with 'defaultArguments' instead." #-}

-- | Description of the job being defined.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDescription :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjDescription = Lens.lens (description :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateJob)
{-# DEPRECATED cjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to use with this job. You may use tags to limit access to the job. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTags :: Lens.Lens' CreateJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjTags = Lens.lens (tags :: CreateJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateJob)
{-# DEPRECATED cjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name you assign to this job definition. It must be unique in your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjName :: Lens.Lens' CreateJob Lude.Text
cjName = Lens.lens (name :: CreateJob -> Lude.Text) (\s a -> s {name = a} :: CreateJob)
{-# DEPRECATED cjName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with this job.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjRole :: Lens.Lens' CreateJob Lude.Text
cjRole = Lens.lens (role' :: CreateJob -> Lude.Text) (\s a -> s {role' = a} :: CreateJob)
{-# DEPRECATED cjRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The @JobCommand@ that executes this job.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjCommand :: Lens.Lens' CreateJob JobCommand
cjCommand = Lens.lens (command :: CreateJob -> JobCommand) (\s a -> s {command = a} :: CreateJob)
{-# DEPRECATED cjCommand "Use generic-lens or generic-optics with 'command' instead." #-}

instance Lude.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.CreateJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberOfWorkers" Lude..=) Lude.<$> numberOfWorkers,
            ("NotificationProperty" Lude..=) Lude.<$> notificationProperty,
            ("Connections" Lude..=) Lude.<$> connections,
            ("WorkerType" Lude..=) Lude.<$> workerType,
            ("SecurityConfiguration" Lude..=) Lude.<$> securityConfiguration,
            ("GlueVersion" Lude..=) Lude.<$> glueVersion,
            ("NonOverridableArguments" Lude..=)
              Lude.<$> nonOverridableArguments,
            ("LogUri" Lude..=) Lude.<$> logURI,
            ("MaxRetries" Lude..=) Lude.<$> maxRetries,
            ("ExecutionProperty" Lude..=) Lude.<$> executionProperty,
            ("AllocatedCapacity" Lude..=) Lude.<$> allocatedCapacity,
            ("MaxCapacity" Lude..=) Lude.<$> maxCapacity,
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("DefaultArguments" Lude..=) Lude.<$> defaultArguments,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Role" Lude..= role'),
            Lude.Just ("Command" Lude..= command)
          ]
      )

instance Lude.ToPath CreateJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { name ::
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

-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- * 'name' - The unique name that was provided for this job definition.
-- * 'responseStatus' - The response status code.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobResponse
mkCreateJobResponse pResponseStatus_ =
  CreateJobResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique name that was provided for this job definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsName :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsName = Lens.lens (name :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateJobResponse)
{-# DEPRECATED cjrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CreateJobResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CreateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
