{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.RegisterJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AWS Batch job definition.
module Network.AWS.Batch.RegisterJobDefinition
    (
    -- * Creating a request
      RegisterJobDefinition (..)
    , mkRegisterJobDefinition
    -- ** Request lenses
    , rjdJobDefinitionName
    , rjdType
    , rjdContainerProperties
    , rjdNodeProperties
    , rjdParameters
    , rjdRetryStrategy
    , rjdTags
    , rjdTimeout

    -- * Destructuring the response
    , RegisterJobDefinitionResponse (..)
    , mkRegisterJobDefinitionResponse
    -- ** Response lenses
    , rjdrrsJobDefinitionName
    , rjdrrsJobDefinitionArn
    , rjdrrsRevision
    , rjdrrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterJobDefinition' smart constructor.
data RegisterJobDefinition = RegisterJobDefinition'
  { jobDefinitionName :: Core.Text
    -- ^ The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
  , type' :: Types.JobDefinitionType
    -- ^ The type of job definition.
  , containerProperties :: Core.Maybe Types.ContainerProperties
    -- ^ An object with various properties specific to single-node container-based jobs. If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
  , nodeProperties :: Core.Maybe Types.NodeProperties
    -- ^ An object with various properties specific to multi-node parallel jobs. If you specify node properties for a job, it becomes a multi-node parallel job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs> in the /AWS Batch User Guide/ . If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
  , retryStrategy :: Core.Maybe Types.RetryStrategy
    -- ^ The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags that you apply to the job definition to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
  , timeout :: Core.Maybe Types.JobTimeout
    -- ^ The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterJobDefinition' value with any optional fields omitted.
mkRegisterJobDefinition
    :: Core.Text -- ^ 'jobDefinitionName'
    -> Types.JobDefinitionType -- ^ 'type\''
    -> RegisterJobDefinition
mkRegisterJobDefinition jobDefinitionName type'
  = RegisterJobDefinition'{jobDefinitionName, type',
                           containerProperties = Core.Nothing, nodeProperties = Core.Nothing,
                           parameters = Core.Nothing, retryStrategy = Core.Nothing,
                           tags = Core.Nothing, timeout = Core.Nothing}

-- | The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdJobDefinitionName :: Lens.Lens' RegisterJobDefinition Core.Text
rjdJobDefinitionName = Lens.field @"jobDefinitionName"
{-# INLINEABLE rjdJobDefinitionName #-}
{-# DEPRECATED jobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead"  #-}

-- | The type of job definition.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdType :: Lens.Lens' RegisterJobDefinition Types.JobDefinitionType
rjdType = Lens.field @"type'"
{-# INLINEABLE rjdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | An object with various properties specific to single-node container-based jobs. If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
--
-- /Note:/ Consider using 'containerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdContainerProperties :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.ContainerProperties)
rjdContainerProperties = Lens.field @"containerProperties"
{-# INLINEABLE rjdContainerProperties #-}
{-# DEPRECATED containerProperties "Use generic-lens or generic-optics with 'containerProperties' instead"  #-}

-- | An object with various properties specific to multi-node parallel jobs. If you specify node properties for a job, it becomes a multi-node parallel job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs> in the /AWS Batch User Guide/ . If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdNodeProperties :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.NodeProperties)
rjdNodeProperties = Lens.field @"nodeProperties"
{-# INLINEABLE rjdNodeProperties #-}
{-# DEPRECATED nodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead"  #-}

-- | Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdParameters :: Lens.Lens' RegisterJobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
rjdParameters = Lens.field @"parameters"
{-# INLINEABLE rjdParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdRetryStrategy :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.RetryStrategy)
rjdRetryStrategy = Lens.field @"retryStrategy"
{-# INLINEABLE rjdRetryStrategy #-}
{-# DEPRECATED retryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead"  #-}

-- | The tags that you apply to the job definition to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdTags :: Lens.Lens' RegisterJobDefinition (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
rjdTags = Lens.field @"tags"
{-# INLINEABLE rjdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdTimeout :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.JobTimeout)
rjdTimeout = Lens.field @"timeout"
{-# INLINEABLE rjdTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

instance Core.ToQuery RegisterJobDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterJobDefinition where
        toHeaders RegisterJobDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterJobDefinition where
        toJSON RegisterJobDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobDefinitionName" Core..= jobDefinitionName),
                  Core.Just ("type" Core..= type'),
                  ("containerProperties" Core..=) Core.<$> containerProperties,
                  ("nodeProperties" Core..=) Core.<$> nodeProperties,
                  ("parameters" Core..=) Core.<$> parameters,
                  ("retryStrategy" Core..=) Core.<$> retryStrategy,
                  ("tags" Core..=) Core.<$> tags,
                  ("timeout" Core..=) Core.<$> timeout])

instance Core.AWSRequest RegisterJobDefinition where
        type Rs RegisterJobDefinition = RegisterJobDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/v1/registerjobdefinition",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterJobDefinitionResponse' Core.<$>
                   (x Core..: "jobDefinitionName") Core.<*>
                     x Core..: "jobDefinitionArn"
                     Core.<*> x Core..: "revision"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterJobDefinitionResponse' smart constructor.
data RegisterJobDefinitionResponse = RegisterJobDefinitionResponse'
  { jobDefinitionName :: Core.Text
    -- ^ The name of the job definition.
  , jobDefinitionArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the job definition.
  , revision :: Core.Int
    -- ^ The revision of the job definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterJobDefinitionResponse' value with any optional fields omitted.
mkRegisterJobDefinitionResponse
    :: Core.Text -- ^ 'jobDefinitionName'
    -> Core.Text -- ^ 'jobDefinitionArn'
    -> Core.Int -- ^ 'revision'
    -> Core.Int -- ^ 'responseStatus'
    -> RegisterJobDefinitionResponse
mkRegisterJobDefinitionResponse jobDefinitionName jobDefinitionArn
  revision responseStatus
  = RegisterJobDefinitionResponse'{jobDefinitionName,
                                   jobDefinitionArn, revision, responseStatus}

-- | The name of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsJobDefinitionName :: Lens.Lens' RegisterJobDefinitionResponse Core.Text
rjdrrsJobDefinitionName = Lens.field @"jobDefinitionName"
{-# INLINEABLE rjdrrsJobDefinitionName #-}
{-# DEPRECATED jobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsJobDefinitionArn :: Lens.Lens' RegisterJobDefinitionResponse Core.Text
rjdrrsJobDefinitionArn = Lens.field @"jobDefinitionArn"
{-# INLINEABLE rjdrrsJobDefinitionArn #-}
{-# DEPRECATED jobDefinitionArn "Use generic-lens or generic-optics with 'jobDefinitionArn' instead"  #-}

-- | The revision of the job definition.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsRevision :: Lens.Lens' RegisterJobDefinitionResponse Core.Int
rjdrrsRevision = Lens.field @"revision"
{-# INLINEABLE rjdrrsRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsResponseStatus :: Lens.Lens' RegisterJobDefinitionResponse Core.Int
rjdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rjdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
