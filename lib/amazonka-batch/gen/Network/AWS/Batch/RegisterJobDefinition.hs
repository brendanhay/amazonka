{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RegisterJobDefinition (..),
    mkRegisterJobDefinition,

    -- ** Request lenses
    rjdJobDefinitionName,
    rjdType,
    rjdContainerProperties,
    rjdNodeProperties,
    rjdParameters,
    rjdRetryStrategy,
    rjdTags,
    rjdTimeout,

    -- * Destructuring the response
    RegisterJobDefinitionResponse (..),
    mkRegisterJobDefinitionResponse,

    -- ** Response lenses
    rjdrrsJobDefinitionName,
    rjdrrsJobDefinitionArn,
    rjdrrsRevision,
    rjdrrsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterJobDefinition' smart constructor.
data RegisterJobDefinition = RegisterJobDefinition'
  { -- | The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
    jobDefinitionName :: Types.JobDefinitionName,
    -- | The type of job definition.
    type' :: Types.JobDefinitionType,
    -- | An object with various properties specific to single-node container-based jobs. If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
    containerProperties :: Core.Maybe Types.ContainerProperties,
    -- | An object with various properties specific to multi-node parallel jobs. If you specify node properties for a job, it becomes a multi-node parallel job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs> in the /AWS Batch User Guide/ . If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
    nodeProperties :: Core.Maybe Types.NodeProperties,
    -- | Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
    parameters :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
    retryStrategy :: Core.Maybe Types.RetryStrategy,
    -- | The tags that you apply to the job definition to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
    timeout :: Core.Maybe Types.JobTimeout
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterJobDefinition' value with any optional fields omitted.
mkRegisterJobDefinition ::
  -- | 'jobDefinitionName'
  Types.JobDefinitionName ->
  -- | 'type\''
  Types.JobDefinitionType ->
  RegisterJobDefinition
mkRegisterJobDefinition jobDefinitionName type' =
  RegisterJobDefinition'
    { jobDefinitionName,
      type',
      containerProperties = Core.Nothing,
      nodeProperties = Core.Nothing,
      parameters = Core.Nothing,
      retryStrategy = Core.Nothing,
      tags = Core.Nothing,
      timeout = Core.Nothing
    }

-- | The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdJobDefinitionName :: Lens.Lens' RegisterJobDefinition Types.JobDefinitionName
rjdJobDefinitionName = Lens.field @"jobDefinitionName"
{-# DEPRECATED rjdJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | The type of job definition.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdType :: Lens.Lens' RegisterJobDefinition Types.JobDefinitionType
rjdType = Lens.field @"type'"
{-# DEPRECATED rjdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An object with various properties specific to single-node container-based jobs. If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
--
-- /Note:/ Consider using 'containerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdContainerProperties :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.ContainerProperties)
rjdContainerProperties = Lens.field @"containerProperties"
{-# DEPRECATED rjdContainerProperties "Use generic-lens or generic-optics with 'containerProperties' instead." #-}

-- | An object with various properties specific to multi-node parallel jobs. If you specify node properties for a job, it becomes a multi-node parallel job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs> in the /AWS Batch User Guide/ . If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdNodeProperties :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.NodeProperties)
rjdNodeProperties = Lens.field @"nodeProperties"
{-# DEPRECATED rjdNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdParameters :: Lens.Lens' RegisterJobDefinition (Core.Maybe (Core.HashMap Types.String Types.String))
rjdParameters = Lens.field @"parameters"
{-# DEPRECATED rjdParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdRetryStrategy :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.RetryStrategy)
rjdRetryStrategy = Lens.field @"retryStrategy"
{-# DEPRECATED rjdRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | The tags that you apply to the job definition to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdTags :: Lens.Lens' RegisterJobDefinition (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
rjdTags = Lens.field @"tags"
{-# DEPRECATED rjdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdTimeout :: Lens.Lens' RegisterJobDefinition (Core.Maybe Types.JobTimeout)
rjdTimeout = Lens.field @"timeout"
{-# DEPRECATED rjdTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

instance Core.FromJSON RegisterJobDefinition where
  toJSON RegisterJobDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobDefinitionName" Core..= jobDefinitionName),
            Core.Just ("type" Core..= type'),
            ("containerProperties" Core..=) Core.<$> containerProperties,
            ("nodeProperties" Core..=) Core.<$> nodeProperties,
            ("parameters" Core..=) Core.<$> parameters,
            ("retryStrategy" Core..=) Core.<$> retryStrategy,
            ("tags" Core..=) Core.<$> tags,
            ("timeout" Core..=) Core.<$> timeout
          ]
      )

instance Core.AWSRequest RegisterJobDefinition where
  type Rs RegisterJobDefinition = RegisterJobDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/registerjobdefinition",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterJobDefinitionResponse'
            Core.<$> (x Core..: "jobDefinitionName")
            Core.<*> (x Core..: "jobDefinitionArn")
            Core.<*> (x Core..: "revision")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterJobDefinitionResponse' smart constructor.
data RegisterJobDefinitionResponse = RegisterJobDefinitionResponse'
  { -- | The name of the job definition.
    jobDefinitionName :: Types.String,
    -- | The Amazon Resource Name (ARN) of the job definition.
    jobDefinitionArn :: Types.String,
    -- | The revision of the job definition.
    revision :: Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterJobDefinitionResponse' value with any optional fields omitted.
mkRegisterJobDefinitionResponse ::
  -- | 'jobDefinitionName'
  Types.String ->
  -- | 'jobDefinitionArn'
  Types.String ->
  -- | 'revision'
  Core.Int ->
  -- | 'responseStatus'
  Core.Int ->
  RegisterJobDefinitionResponse
mkRegisterJobDefinitionResponse
  jobDefinitionName
  jobDefinitionArn
  revision
  responseStatus =
    RegisterJobDefinitionResponse'
      { jobDefinitionName,
        jobDefinitionArn,
        revision,
        responseStatus
      }

-- | The name of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsJobDefinitionName :: Lens.Lens' RegisterJobDefinitionResponse Types.String
rjdrrsJobDefinitionName = Lens.field @"jobDefinitionName"
{-# DEPRECATED rjdrrsJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | The Amazon Resource Name (ARN) of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsJobDefinitionArn :: Lens.Lens' RegisterJobDefinitionResponse Types.String
rjdrrsJobDefinitionArn = Lens.field @"jobDefinitionArn"
{-# DEPRECATED rjdrrsJobDefinitionArn "Use generic-lens or generic-optics with 'jobDefinitionArn' instead." #-}

-- | The revision of the job definition.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsRevision :: Lens.Lens' RegisterJobDefinitionResponse Core.Int
rjdrrsRevision = Lens.field @"revision"
{-# DEPRECATED rjdrrsRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrrsResponseStatus :: Lens.Lens' RegisterJobDefinitionResponse Core.Int
rjdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rjdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
