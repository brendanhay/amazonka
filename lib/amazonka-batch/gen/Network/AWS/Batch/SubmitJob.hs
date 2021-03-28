{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SubmitJob (..)
    , mkSubmitJob
    -- ** Request lenses
    , sjJobName
    , sjJobQueue
    , sjJobDefinition
    , sjArrayProperties
    , sjContainerOverrides
    , sjDependsOn
    , sjNodeOverrides
    , sjParameters
    , sjRetryStrategy
    , sjTags
    , sjTimeout

    -- * Destructuring the response
    , SubmitJobResponse (..)
    , mkSubmitJobResponse
    -- ** Response lenses
    , sjrrsJobName
    , sjrrsJobId
    , sjrrsJobArn
    , sjrrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubmitJob' smart constructor.
data SubmitJob = SubmitJob'
  { jobName :: Core.Text
    -- ^ The name of the job. The first character must be alphanumeric, and up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
  , jobQueue :: Core.Text
    -- ^ The job queue into which the job is submitted. You can specify either the name or the Amazon Resource Name (ARN) of the queue.
  , jobDefinition :: Core.Text
    -- ^ The job definition used by this job. This value can be one of @name@ , @name:revision@ , or the Amazon Resource Name (ARN) for the job definition. If @name@ is specified without a revision then the latest active revision is used.
  , arrayProperties :: Core.Maybe Types.ArrayProperties
    -- ^ The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs> in the /AWS Batch User Guide/ .
  , containerOverrides :: Core.Maybe Types.ContainerOverrides
    -- ^ A list of container overrides in JSON format that specify the name of a container in the specified job definition and the overrides it should receive. You can override the default command for a container (that is specified in the job definition or the Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the job definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
  , dependsOn :: Core.Maybe [Types.JobDependency]
    -- ^ A list of dependencies for the job. A job can depend upon a maximum of 20 jobs. You can specify a @SEQUENTIAL@ type dependency without specifying a job ID for array jobs so that each child array job completes sequentially, starting at index 0. You can also specify an @N_TO_N@ type dependency with a job ID for array jobs. In that case, each index child of this job must wait for the corresponding index child of each dependency to complete before it can begin.
  , nodeOverrides :: Core.Maybe Types.NodeOverrides
    -- ^ A list of node overrides in JSON format that specify the node range to target and the container overrides for that node range.
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Additional parameters passed to the job that replace parameter substitution placeholders that are set in the job definition. Parameters are specified as a key and value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
  , retryStrategy :: Core.Maybe Types.RetryStrategy
    -- ^ The retry strategy to use for failed jobs from this 'SubmitJob' operation. When a retry strategy is specified here, it overrides the retry strategy defined in the job definition.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags that you apply to the job request to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
  , timeout :: Core.Maybe Types.JobTimeout
    -- ^ The timeout configuration for this 'SubmitJob' operation. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. This configuration overrides any timeout configuration specified in the job definition. For array jobs, child jobs have the same timeout configuration as the parent job. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitJob' value with any optional fields omitted.
mkSubmitJob
    :: Core.Text -- ^ 'jobName'
    -> Core.Text -- ^ 'jobQueue'
    -> Core.Text -- ^ 'jobDefinition'
    -> SubmitJob
mkSubmitJob jobName jobQueue jobDefinition
  = SubmitJob'{jobName, jobQueue, jobDefinition,
               arrayProperties = Core.Nothing, containerOverrides = Core.Nothing,
               dependsOn = Core.Nothing, nodeOverrides = Core.Nothing,
               parameters = Core.Nothing, retryStrategy = Core.Nothing,
               tags = Core.Nothing, timeout = Core.Nothing}

-- | The name of the job. The first character must be alphanumeric, and up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjJobName :: Lens.Lens' SubmitJob Core.Text
sjJobName = Lens.field @"jobName"
{-# INLINEABLE sjJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The job queue into which the job is submitted. You can specify either the name or the Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjJobQueue :: Lens.Lens' SubmitJob Core.Text
sjJobQueue = Lens.field @"jobQueue"
{-# INLINEABLE sjJobQueue #-}
{-# DEPRECATED jobQueue "Use generic-lens or generic-optics with 'jobQueue' instead"  #-}

-- | The job definition used by this job. This value can be one of @name@ , @name:revision@ , or the Amazon Resource Name (ARN) for the job definition. If @name@ is specified without a revision then the latest active revision is used.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjJobDefinition :: Lens.Lens' SubmitJob Core.Text
sjJobDefinition = Lens.field @"jobDefinition"
{-# INLINEABLE sjJobDefinition #-}
{-# DEPRECATED jobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead"  #-}

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/array_jobs.html Array Jobs> in the /AWS Batch User Guide/ .
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjArrayProperties :: Lens.Lens' SubmitJob (Core.Maybe Types.ArrayProperties)
sjArrayProperties = Lens.field @"arrayProperties"
{-# INLINEABLE sjArrayProperties #-}
{-# DEPRECATED arrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead"  #-}

-- | A list of container overrides in JSON format that specify the name of a container in the specified job definition and the overrides it should receive. You can override the default command for a container (that is specified in the job definition or the Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the job definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- /Note:/ Consider using 'containerOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjContainerOverrides :: Lens.Lens' SubmitJob (Core.Maybe Types.ContainerOverrides)
sjContainerOverrides = Lens.field @"containerOverrides"
{-# INLINEABLE sjContainerOverrides #-}
{-# DEPRECATED containerOverrides "Use generic-lens or generic-optics with 'containerOverrides' instead"  #-}

-- | A list of dependencies for the job. A job can depend upon a maximum of 20 jobs. You can specify a @SEQUENTIAL@ type dependency without specifying a job ID for array jobs so that each child array job completes sequentially, starting at index 0. You can also specify an @N_TO_N@ type dependency with a job ID for array jobs. In that case, each index child of this job must wait for the corresponding index child of each dependency to complete before it can begin.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjDependsOn :: Lens.Lens' SubmitJob (Core.Maybe [Types.JobDependency])
sjDependsOn = Lens.field @"dependsOn"
{-# INLINEABLE sjDependsOn #-}
{-# DEPRECATED dependsOn "Use generic-lens or generic-optics with 'dependsOn' instead"  #-}

-- | A list of node overrides in JSON format that specify the node range to target and the container overrides for that node range.
--
-- /Note:/ Consider using 'nodeOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjNodeOverrides :: Lens.Lens' SubmitJob (Core.Maybe Types.NodeOverrides)
sjNodeOverrides = Lens.field @"nodeOverrides"
{-# INLINEABLE sjNodeOverrides #-}
{-# DEPRECATED nodeOverrides "Use generic-lens or generic-optics with 'nodeOverrides' instead"  #-}

-- | Additional parameters passed to the job that replace parameter substitution placeholders that are set in the job definition. Parameters are specified as a key and value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjParameters :: Lens.Lens' SubmitJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
sjParameters = Lens.field @"parameters"
{-# INLINEABLE sjParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The retry strategy to use for failed jobs from this 'SubmitJob' operation. When a retry strategy is specified here, it overrides the retry strategy defined in the job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjRetryStrategy :: Lens.Lens' SubmitJob (Core.Maybe Types.RetryStrategy)
sjRetryStrategy = Lens.field @"retryStrategy"
{-# INLINEABLE sjRetryStrategy #-}
{-# DEPRECATED retryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead"  #-}

-- | The tags that you apply to the job request to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjTags :: Lens.Lens' SubmitJob (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
sjTags = Lens.field @"tags"
{-# INLINEABLE sjTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The timeout configuration for this 'SubmitJob' operation. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. This configuration overrides any timeout configuration specified in the job definition. For array jobs, child jobs have the same timeout configuration as the parent job. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjTimeout :: Lens.Lens' SubmitJob (Core.Maybe Types.JobTimeout)
sjTimeout = Lens.field @"timeout"
{-# INLINEABLE sjTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

instance Core.ToQuery SubmitJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SubmitJob where
        toHeaders SubmitJob{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SubmitJob where
        toJSON SubmitJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobName" Core..= jobName),
                  Core.Just ("jobQueue" Core..= jobQueue),
                  Core.Just ("jobDefinition" Core..= jobDefinition),
                  ("arrayProperties" Core..=) Core.<$> arrayProperties,
                  ("containerOverrides" Core..=) Core.<$> containerOverrides,
                  ("dependsOn" Core..=) Core.<$> dependsOn,
                  ("nodeOverrides" Core..=) Core.<$> nodeOverrides,
                  ("parameters" Core..=) Core.<$> parameters,
                  ("retryStrategy" Core..=) Core.<$> retryStrategy,
                  ("tags" Core..=) Core.<$> tags,
                  ("timeout" Core..=) Core.<$> timeout])

instance Core.AWSRequest SubmitJob where
        type Rs SubmitJob = SubmitJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/submitjob",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SubmitJobResponse' Core.<$>
                   (x Core..: "jobName") Core.<*> x Core..: "jobId" Core.<*>
                     x Core..:? "jobArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSubmitJobResponse' smart constructor.
data SubmitJobResponse = SubmitJobResponse'
  { jobName :: Core.Text
    -- ^ The name of the job.
  , jobId :: Core.Text
    -- ^ The unique identifier for the job.
  , jobArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitJobResponse' value with any optional fields omitted.
mkSubmitJobResponse
    :: Core.Text -- ^ 'jobName'
    -> Core.Text -- ^ 'jobId'
    -> Core.Int -- ^ 'responseStatus'
    -> SubmitJobResponse
mkSubmitJobResponse jobName jobId responseStatus
  = SubmitJobResponse'{jobName, jobId, jobArn = Core.Nothing,
                       responseStatus}

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsJobName :: Lens.Lens' SubmitJobResponse Core.Text
sjrrsJobName = Lens.field @"jobName"
{-# INLINEABLE sjrrsJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The unique identifier for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsJobId :: Lens.Lens' SubmitJobResponse Core.Text
sjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE sjrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The Amazon Resource Name (ARN) for the job.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsJobArn :: Lens.Lens' SubmitJobResponse (Core.Maybe Core.Text)
sjrrsJobArn = Lens.field @"jobArn"
{-# INLINEABLE sjrrsJobArn #-}
{-# DEPRECATED jobArn "Use generic-lens or generic-optics with 'jobArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrrsResponseStatus :: Lens.Lens' SubmitJobResponse Core.Int
sjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
