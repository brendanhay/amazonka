{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a processing job.
module Network.AWS.SageMaker.CreateProcessingJob
  ( -- * Creating a request
    CreateProcessingJob (..),
    mkCreateProcessingJob,

    -- ** Request lenses
    cpjProcessingJobName,
    cpjProcessingResources,
    cpjAppSpecification,
    cpjRoleArn,
    cpjEnvironment,
    cpjExperimentConfig,
    cpjNetworkConfig,
    cpjProcessingInputs,
    cpjProcessingOutputConfig,
    cpjStoppingCondition,
    cpjTags,

    -- * Destructuring the response
    CreateProcessingJobResponse (..),
    mkCreateProcessingJobResponse,

    -- ** Response lenses
    cpjrrsProcessingJobArn,
    cpjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateProcessingJob' smart constructor.
data CreateProcessingJob = CreateProcessingJob'
  { -- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
    processingJobName :: Types.ProcessingJobName,
    -- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
    processingResources :: Types.ProcessingResources,
    -- | Configures the processing job to run a specified Docker container image.
    appSpecification :: Types.AppSpecification,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
    roleArn :: Types.RoleArn,
    -- | Sets the environment variables in the Docker container.
    environment :: Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue),
    experimentConfig :: Core.Maybe Types.ExperimentConfig,
    -- | Networking options for a processing job.
    networkConfig :: Core.Maybe Types.NetworkConfig,
    -- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
    processingInputs :: Core.Maybe [Types.ProcessingInput],
    -- | Output configuration for the processing job.
    processingOutputConfig :: Core.Maybe Types.ProcessingOutputConfig,
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Core.Maybe Types.ProcessingStoppingCondition,
    -- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProcessingJob' value with any optional fields omitted.
mkCreateProcessingJob ::
  -- | 'processingJobName'
  Types.ProcessingJobName ->
  -- | 'processingResources'
  Types.ProcessingResources ->
  -- | 'appSpecification'
  Types.AppSpecification ->
  -- | 'roleArn'
  Types.RoleArn ->
  CreateProcessingJob
mkCreateProcessingJob
  processingJobName
  processingResources
  appSpecification
  roleArn =
    CreateProcessingJob'
      { processingJobName,
        processingResources,
        appSpecification,
        roleArn,
        environment = Core.Nothing,
        experimentConfig = Core.Nothing,
        networkConfig = Core.Nothing,
        processingInputs = Core.Nothing,
        processingOutputConfig = Core.Nothing,
        stoppingCondition = Core.Nothing,
        tags = Core.Nothing
      }

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingJobName :: Lens.Lens' CreateProcessingJob Types.ProcessingJobName
cpjProcessingJobName = Lens.field @"processingJobName"
{-# DEPRECATED cpjProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingResources :: Lens.Lens' CreateProcessingJob Types.ProcessingResources
cpjProcessingResources = Lens.field @"processingResources"
{-# DEPRECATED cpjProcessingResources "Use generic-lens or generic-optics with 'processingResources' instead." #-}

-- | Configures the processing job to run a specified Docker container image.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjAppSpecification :: Lens.Lens' CreateProcessingJob Types.AppSpecification
cpjAppSpecification = Lens.field @"appSpecification"
{-# DEPRECATED cpjAppSpecification "Use generic-lens or generic-optics with 'appSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjRoleArn :: Lens.Lens' CreateProcessingJob Types.RoleArn
cpjRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cpjRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Sets the environment variables in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjEnvironment :: Lens.Lens' CreateProcessingJob (Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue))
cpjEnvironment = Lens.field @"environment"
{-# DEPRECATED cpjEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjExperimentConfig :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.ExperimentConfig)
cpjExperimentConfig = Lens.field @"experimentConfig"
{-# DEPRECATED cpjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | Networking options for a processing job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjNetworkConfig :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.NetworkConfig)
cpjNetworkConfig = Lens.field @"networkConfig"
{-# DEPRECATED cpjNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingInputs :: Lens.Lens' CreateProcessingJob (Core.Maybe [Types.ProcessingInput])
cpjProcessingInputs = Lens.field @"processingInputs"
{-# DEPRECATED cpjProcessingInputs "Use generic-lens or generic-optics with 'processingInputs' instead." #-}

-- | Output configuration for the processing job.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingOutputConfig :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.ProcessingOutputConfig)
cpjProcessingOutputConfig = Lens.field @"processingOutputConfig"
{-# DEPRECATED cpjProcessingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead." #-}

-- | The time limit for how long the processing job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjStoppingCondition :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.ProcessingStoppingCondition)
cpjStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED cpjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjTags :: Lens.Lens' CreateProcessingJob (Core.Maybe [Types.Tag])
cpjTags = Lens.field @"tags"
{-# DEPRECATED cpjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateProcessingJob where
  toJSON CreateProcessingJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProcessingJobName" Core..= processingJobName),
            Core.Just ("ProcessingResources" Core..= processingResources),
            Core.Just ("AppSpecification" Core..= appSpecification),
            Core.Just ("RoleArn" Core..= roleArn),
            ("Environment" Core..=) Core.<$> environment,
            ("ExperimentConfig" Core..=) Core.<$> experimentConfig,
            ("NetworkConfig" Core..=) Core.<$> networkConfig,
            ("ProcessingInputs" Core..=) Core.<$> processingInputs,
            ("ProcessingOutputConfig" Core..=) Core.<$> processingOutputConfig,
            ("StoppingCondition" Core..=) Core.<$> stoppingCondition,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateProcessingJob where
  type Rs CreateProcessingJob = CreateProcessingJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateProcessingJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProcessingJobResponse'
            Core.<$> (x Core..: "ProcessingJobArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProcessingJobResponse' smart constructor.
data CreateProcessingJobResponse = CreateProcessingJobResponse'
  { -- | The Amazon Resource Name (ARN) of the processing job.
    processingJobArn :: Types.ProcessingJobArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProcessingJobResponse' value with any optional fields omitted.
mkCreateProcessingJobResponse ::
  -- | 'processingJobArn'
  Types.ProcessingJobArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateProcessingJobResponse
mkCreateProcessingJobResponse processingJobArn responseStatus =
  CreateProcessingJobResponse' {processingJobArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the processing job.
--
-- /Note:/ Consider using 'processingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjrrsProcessingJobArn :: Lens.Lens' CreateProcessingJobResponse Types.ProcessingJobArn
cpjrrsProcessingJobArn = Lens.field @"processingJobArn"
{-# DEPRECATED cpjrrsProcessingJobArn "Use generic-lens or generic-optics with 'processingJobArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjrrsResponseStatus :: Lens.Lens' CreateProcessingJobResponse Core.Int
cpjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
