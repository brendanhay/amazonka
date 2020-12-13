{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.SubmitJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits an AWS Batch job from a job definition. Parameters specified during 'SubmitJob' override parameters defined in the job definition.
module Network.AWS.Batch.SubmitJob
  ( -- * Creating a request
    SubmitJob (..),
    mkSubmitJob,

    -- ** Request lenses
    sjNodeOverrides,
    sjContainerOverrides,
    sjJobName,
    sjRetryStrategy,
    sjDependsOn,
    sjJobDefinition,
    sjParameters,
    sjArrayProperties,
    sjTimeout,
    sjJobQueue,
    sjTags,

    -- * Destructuring the response
    SubmitJobResponse (..),
    mkSubmitJobResponse,

    -- ** Response lenses
    sjrsJobId,
    sjrsJobARN,
    sjrsJobName,
    sjrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSubmitJob' smart constructor.
data SubmitJob = SubmitJob'
  { -- | A list of node overrides in JSON format that specify the node range to target and the container overrides for that node range.
    nodeOverrides :: Lude.Maybe NodeOverrides,
    -- | A list of container overrides in JSON format that specify the name of a container in the specified job definition and the overrides it should receive. You can override the default command for a container (that is specified in the job definition or the Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the job definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
    containerOverrides :: Lude.Maybe ContainerOverrides,
    -- | The name of the job. The first character must be alphanumeric, and up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
    jobName :: Lude.Text,
    -- | The retry strategy to use for failed jobs from this 'SubmitJob' operation. When a retry strategy is specified here, it overrides the retry strategy defined in the job definition.
    retryStrategy :: Lude.Maybe RetryStrategy,
    -- | A list of dependencies for the job. A job can depend upon a maximum of 20 jobs. You can specify a @SEQUENTIAL@ type dependency without specifying a job ID for array jobs so that each child array job completes sequentially, starting at index 0. You can also specify an @N_TO_N@ type dependency with a job ID for array jobs. In that case, each index child of this job must wait for the corresponding index child of each dependency to complete before it can begin.
    dependsOn :: Lude.Maybe [JobDependency],
    -- | The job definition used by this job. This value can be one of @name@ , @name:revision@ , or the Amazon Resource Name (ARN) for the job definition. If @name@ is specified without a revision then the latest active revision is used.
    jobDefinition :: Lude.Text,
    -- | Additional parameters passed to the job that replace parameter substitution placeholders that are set in the job definition. Parameters are specified as a key and value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs> in the /AWS Batch User Guide/ .
    arrayProperties :: Lude.Maybe ArrayProperties,
    -- | The timeout configuration for this 'SubmitJob' operation. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. This configuration overrides any timeout configuration specified in the job definition. For array jobs, child jobs have the same timeout configuration as the parent job. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
    timeout :: Lude.Maybe JobTimeout,
    -- | The job queue into which the job is submitted. You can specify either the name or the Amazon Resource Name (ARN) of the queue.
    jobQueue :: Lude.Text,
    -- | The tags that you apply to the job request to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubmitJob' with the minimum fields required to make a request.
--
-- * 'nodeOverrides' - A list of node overrides in JSON format that specify the node range to target and the container overrides for that node range.
-- * 'containerOverrides' - A list of container overrides in JSON format that specify the name of a container in the specified job definition and the overrides it should receive. You can override the default command for a container (that is specified in the job definition or the Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the job definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
-- * 'jobName' - The name of the job. The first character must be alphanumeric, and up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
-- * 'retryStrategy' - The retry strategy to use for failed jobs from this 'SubmitJob' operation. When a retry strategy is specified here, it overrides the retry strategy defined in the job definition.
-- * 'dependsOn' - A list of dependencies for the job. A job can depend upon a maximum of 20 jobs. You can specify a @SEQUENTIAL@ type dependency without specifying a job ID for array jobs so that each child array job completes sequentially, starting at index 0. You can also specify an @N_TO_N@ type dependency with a job ID for array jobs. In that case, each index child of this job must wait for the corresponding index child of each dependency to complete before it can begin.
-- * 'jobDefinition' - The job definition used by this job. This value can be one of @name@ , @name:revision@ , or the Amazon Resource Name (ARN) for the job definition. If @name@ is specified without a revision then the latest active revision is used.
-- * 'parameters' - Additional parameters passed to the job that replace parameter substitution placeholders that are set in the job definition. Parameters are specified as a key and value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
-- * 'arrayProperties' - The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs> in the /AWS Batch User Guide/ .
-- * 'timeout' - The timeout configuration for this 'SubmitJob' operation. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. This configuration overrides any timeout configuration specified in the job definition. For array jobs, child jobs have the same timeout configuration as the parent job. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'jobQueue' - The job queue into which the job is submitted. You can specify either the name or the Amazon Resource Name (ARN) of the queue.
-- * 'tags' - The tags that you apply to the job request to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
mkSubmitJob ::
  -- | 'jobName'
  Lude.Text ->
  -- | 'jobDefinition'
  Lude.Text ->
  -- | 'jobQueue'
  Lude.Text ->
  SubmitJob
mkSubmitJob pJobName_ pJobDefinition_ pJobQueue_ =
  SubmitJob'
    { nodeOverrides = Lude.Nothing,
      containerOverrides = Lude.Nothing,
      jobName = pJobName_,
      retryStrategy = Lude.Nothing,
      dependsOn = Lude.Nothing,
      jobDefinition = pJobDefinition_,
      parameters = Lude.Nothing,
      arrayProperties = Lude.Nothing,
      timeout = Lude.Nothing,
      jobQueue = pJobQueue_,
      tags = Lude.Nothing
    }

-- | A list of node overrides in JSON format that specify the node range to target and the container overrides for that node range.
--
-- /Note:/ Consider using 'nodeOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjNodeOverrides :: Lens.Lens' SubmitJob (Lude.Maybe NodeOverrides)
sjNodeOverrides = Lens.lens (nodeOverrides :: SubmitJob -> Lude.Maybe NodeOverrides) (\s a -> s {nodeOverrides = a} :: SubmitJob)
{-# DEPRECATED sjNodeOverrides "Use generic-lens or generic-optics with 'nodeOverrides' instead." #-}

-- | A list of container overrides in JSON format that specify the name of a container in the specified job definition and the overrides it should receive. You can override the default command for a container (that is specified in the job definition or the Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the job definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- /Note:/ Consider using 'containerOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjContainerOverrides :: Lens.Lens' SubmitJob (Lude.Maybe ContainerOverrides)
sjContainerOverrides = Lens.lens (containerOverrides :: SubmitJob -> Lude.Maybe ContainerOverrides) (\s a -> s {containerOverrides = a} :: SubmitJob)
{-# DEPRECATED sjContainerOverrides "Use generic-lens or generic-optics with 'containerOverrides' instead." #-}

-- | The name of the job. The first character must be alphanumeric, and up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjJobName :: Lens.Lens' SubmitJob Lude.Text
sjJobName = Lens.lens (jobName :: SubmitJob -> Lude.Text) (\s a -> s {jobName = a} :: SubmitJob)
{-# DEPRECATED sjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The retry strategy to use for failed jobs from this 'SubmitJob' operation. When a retry strategy is specified here, it overrides the retry strategy defined in the job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjRetryStrategy :: Lens.Lens' SubmitJob (Lude.Maybe RetryStrategy)
sjRetryStrategy = Lens.lens (retryStrategy :: SubmitJob -> Lude.Maybe RetryStrategy) (\s a -> s {retryStrategy = a} :: SubmitJob)
{-# DEPRECATED sjRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | A list of dependencies for the job. A job can depend upon a maximum of 20 jobs. You can specify a @SEQUENTIAL@ type dependency without specifying a job ID for array jobs so that each child array job completes sequentially, starting at index 0. You can also specify an @N_TO_N@ type dependency with a job ID for array jobs. In that case, each index child of this job must wait for the corresponding index child of each dependency to complete before it can begin.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjDependsOn :: Lens.Lens' SubmitJob (Lude.Maybe [JobDependency])
sjDependsOn = Lens.lens (dependsOn :: SubmitJob -> Lude.Maybe [JobDependency]) (\s a -> s {dependsOn = a} :: SubmitJob)
{-# DEPRECATED sjDependsOn "Use generic-lens or generic-optics with 'dependsOn' instead." #-}

-- | The job definition used by this job. This value can be one of @name@ , @name:revision@ , or the Amazon Resource Name (ARN) for the job definition. If @name@ is specified without a revision then the latest active revision is used.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjJobDefinition :: Lens.Lens' SubmitJob Lude.Text
sjJobDefinition = Lens.lens (jobDefinition :: SubmitJob -> Lude.Text) (\s a -> s {jobDefinition = a} :: SubmitJob)
{-# DEPRECATED sjJobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead." #-}

-- | Additional parameters passed to the job that replace parameter substitution placeholders that are set in the job definition. Parameters are specified as a key and value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjParameters :: Lens.Lens' SubmitJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sjParameters = Lens.lens (parameters :: SubmitJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: SubmitJob)
{-# DEPRECATED sjParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjArrayProperties :: Lens.Lens' SubmitJob (Lude.Maybe ArrayProperties)
sjArrayProperties = Lens.lens (arrayProperties :: SubmitJob -> Lude.Maybe ArrayProperties) (\s a -> s {arrayProperties = a} :: SubmitJob)
{-# DEPRECATED sjArrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead." #-}

-- | The timeout configuration for this 'SubmitJob' operation. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. This configuration overrides any timeout configuration specified in the job definition. For array jobs, child jobs have the same timeout configuration as the parent job. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjTimeout :: Lens.Lens' SubmitJob (Lude.Maybe JobTimeout)
sjTimeout = Lens.lens (timeout :: SubmitJob -> Lude.Maybe JobTimeout) (\s a -> s {timeout = a} :: SubmitJob)
{-# DEPRECATED sjTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The job queue into which the job is submitted. You can specify either the name or the Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjJobQueue :: Lens.Lens' SubmitJob Lude.Text
sjJobQueue = Lens.lens (jobQueue :: SubmitJob -> Lude.Text) (\s a -> s {jobQueue = a} :: SubmitJob)
{-# DEPRECATED sjJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

-- | The tags that you apply to the job request to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjTags :: Lens.Lens' SubmitJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sjTags = Lens.lens (tags :: SubmitJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: SubmitJob)
{-# DEPRECATED sjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest SubmitJob where
  type Rs SubmitJob = SubmitJobResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          SubmitJobResponse'
            Lude.<$> (x Lude..:> "jobId")
            Lude.<*> (x Lude..?> "jobArn")
            Lude.<*> (x Lude..:> "jobName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SubmitJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SubmitJob where
  toJSON SubmitJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nodeOverrides" Lude..=) Lude.<$> nodeOverrides,
            ("containerOverrides" Lude..=) Lude.<$> containerOverrides,
            Lude.Just ("jobName" Lude..= jobName),
            ("retryStrategy" Lude..=) Lude.<$> retryStrategy,
            ("dependsOn" Lude..=) Lude.<$> dependsOn,
            Lude.Just ("jobDefinition" Lude..= jobDefinition),
            ("parameters" Lude..=) Lude.<$> parameters,
            ("arrayProperties" Lude..=) Lude.<$> arrayProperties,
            ("timeout" Lude..=) Lude.<$> timeout,
            Lude.Just ("jobQueue" Lude..= jobQueue),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath SubmitJob where
  toPath = Lude.const "/v1/submitjob"

instance Lude.ToQuery SubmitJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSubmitJobResponse' smart constructor.
data SubmitJobResponse = SubmitJobResponse'
  { -- | The unique identifier for the job.
    jobId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) for the job.
    jobARN :: Lude.Maybe Lude.Text,
    -- | The name of the job.
    jobName :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubmitJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique identifier for the job.
-- * 'jobARN' - The Amazon Resource Name (ARN) for the job.
-- * 'jobName' - The name of the job.
-- * 'responseStatus' - The response status code.
mkSubmitJobResponse ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'jobName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  SubmitJobResponse
mkSubmitJobResponse pJobId_ pJobName_ pResponseStatus_ =
  SubmitJobResponse'
    { jobId = pJobId_,
      jobARN = Lude.Nothing,
      jobName = pJobName_,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrsJobId :: Lens.Lens' SubmitJobResponse Lude.Text
sjrsJobId = Lens.lens (jobId :: SubmitJobResponse -> Lude.Text) (\s a -> s {jobId = a} :: SubmitJobResponse)
{-# DEPRECATED sjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The Amazon Resource Name (ARN) for the job.
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrsJobARN :: Lens.Lens' SubmitJobResponse (Lude.Maybe Lude.Text)
sjrsJobARN = Lens.lens (jobARN :: SubmitJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: SubmitJobResponse)
{-# DEPRECATED sjrsJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrsJobName :: Lens.Lens' SubmitJobResponse Lude.Text
sjrsJobName = Lens.lens (jobName :: SubmitJobResponse -> Lude.Text) (\s a -> s {jobName = a} :: SubmitJobResponse)
{-# DEPRECATED sjrsJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrsResponseStatus :: Lens.Lens' SubmitJobResponse Lude.Int
sjrsResponseStatus = Lens.lens (responseStatus :: SubmitJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SubmitJobResponse)
{-# DEPRECATED sjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
