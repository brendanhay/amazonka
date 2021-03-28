{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateProcessingJob (..)
    , mkCreateProcessingJob
    -- ** Request lenses
    , cpjProcessingJobName
    , cpjProcessingResources
    , cpjAppSpecification
    , cpjRoleArn
    , cpjEnvironment
    , cpjExperimentConfig
    , cpjNetworkConfig
    , cpjProcessingInputs
    , cpjProcessingOutputConfig
    , cpjStoppingCondition
    , cpjTags

    -- * Destructuring the response
    , CreateProcessingJobResponse (..)
    , mkCreateProcessingJobResponse
    -- ** Response lenses
    , cpjrrsProcessingJobArn
    , cpjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateProcessingJob' smart constructor.
data CreateProcessingJob = CreateProcessingJob'
  { processingJobName :: Types.ProcessingJobName
    -- ^ The name of the processing job. The name must be unique within an AWS Region in the AWS account.
  , processingResources :: Types.ProcessingResources
    -- ^ Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
  , appSpecification :: Types.AppSpecification
    -- ^ Configures the processing job to run a specified Docker container image.
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
  , environment :: Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue)
    -- ^ Sets the environment variables in the Docker container.
  , experimentConfig :: Core.Maybe Types.ExperimentConfig
  , networkConfig :: Core.Maybe Types.NetworkConfig
    -- ^ Networking options for a processing job.
  , processingInputs :: Core.Maybe [Types.ProcessingInput]
    -- ^ For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
  , processingOutputConfig :: Core.Maybe Types.ProcessingOutputConfig
    -- ^ Output configuration for the processing job.
  , stoppingCondition :: Core.Maybe Types.ProcessingStoppingCondition
    -- ^ The time limit for how long the processing job is allowed to run.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProcessingJob' value with any optional fields omitted.
mkCreateProcessingJob
    :: Types.ProcessingJobName -- ^ 'processingJobName'
    -> Types.ProcessingResources -- ^ 'processingResources'
    -> Types.AppSpecification -- ^ 'appSpecification'
    -> Types.RoleArn -- ^ 'roleArn'
    -> CreateProcessingJob
mkCreateProcessingJob processingJobName processingResources
  appSpecification roleArn
  = CreateProcessingJob'{processingJobName, processingResources,
                         appSpecification, roleArn, environment = Core.Nothing,
                         experimentConfig = Core.Nothing, networkConfig = Core.Nothing,
                         processingInputs = Core.Nothing,
                         processingOutputConfig = Core.Nothing,
                         stoppingCondition = Core.Nothing, tags = Core.Nothing}

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingJobName :: Lens.Lens' CreateProcessingJob Types.ProcessingJobName
cpjProcessingJobName = Lens.field @"processingJobName"
{-# INLINEABLE cpjProcessingJobName #-}
{-# DEPRECATED processingJobName "Use generic-lens or generic-optics with 'processingJobName' instead"  #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingResources :: Lens.Lens' CreateProcessingJob Types.ProcessingResources
cpjProcessingResources = Lens.field @"processingResources"
{-# INLINEABLE cpjProcessingResources #-}
{-# DEPRECATED processingResources "Use generic-lens or generic-optics with 'processingResources' instead"  #-}

-- | Configures the processing job to run a specified Docker container image.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjAppSpecification :: Lens.Lens' CreateProcessingJob Types.AppSpecification
cpjAppSpecification = Lens.field @"appSpecification"
{-# INLINEABLE cpjAppSpecification #-}
{-# DEPRECATED appSpecification "Use generic-lens or generic-optics with 'appSpecification' instead"  #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjRoleArn :: Lens.Lens' CreateProcessingJob Types.RoleArn
cpjRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cpjRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Sets the environment variables in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjEnvironment :: Lens.Lens' CreateProcessingJob (Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue))
cpjEnvironment = Lens.field @"environment"
{-# INLINEABLE cpjEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjExperimentConfig :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.ExperimentConfig)
cpjExperimentConfig = Lens.field @"experimentConfig"
{-# INLINEABLE cpjExperimentConfig #-}
{-# DEPRECATED experimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead"  #-}

-- | Networking options for a processing job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjNetworkConfig :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.NetworkConfig)
cpjNetworkConfig = Lens.field @"networkConfig"
{-# INLINEABLE cpjNetworkConfig #-}
{-# DEPRECATED networkConfig "Use generic-lens or generic-optics with 'networkConfig' instead"  #-}

-- | For each input, data is downloaded from S3 into the processing container before the processing job begins running if "S3InputMode" is set to @File@ .
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingInputs :: Lens.Lens' CreateProcessingJob (Core.Maybe [Types.ProcessingInput])
cpjProcessingInputs = Lens.field @"processingInputs"
{-# INLINEABLE cpjProcessingInputs #-}
{-# DEPRECATED processingInputs "Use generic-lens or generic-optics with 'processingInputs' instead"  #-}

-- | Output configuration for the processing job.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjProcessingOutputConfig :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.ProcessingOutputConfig)
cpjProcessingOutputConfig = Lens.field @"processingOutputConfig"
{-# INLINEABLE cpjProcessingOutputConfig #-}
{-# DEPRECATED processingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead"  #-}

-- | The time limit for how long the processing job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjStoppingCondition :: Lens.Lens' CreateProcessingJob (Core.Maybe Types.ProcessingStoppingCondition)
cpjStoppingCondition = Lens.field @"stoppingCondition"
{-# INLINEABLE cpjStoppingCondition #-}
{-# DEPRECATED stoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead"  #-}

-- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjTags :: Lens.Lens' CreateProcessingJob (Core.Maybe [Types.Tag])
cpjTags = Lens.field @"tags"
{-# INLINEABLE cpjTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateProcessingJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProcessingJob where
        toHeaders CreateProcessingJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateProcessingJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProcessingJob where
        toJSON CreateProcessingJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProcessingJobName" Core..= processingJobName),
                  Core.Just ("ProcessingResources" Core..= processingResources),
                  Core.Just ("AppSpecification" Core..= appSpecification),
                  Core.Just ("RoleArn" Core..= roleArn),
                  ("Environment" Core..=) Core.<$> environment,
                  ("ExperimentConfig" Core..=) Core.<$> experimentConfig,
                  ("NetworkConfig" Core..=) Core.<$> networkConfig,
                  ("ProcessingInputs" Core..=) Core.<$> processingInputs,
                  ("ProcessingOutputConfig" Core..=) Core.<$> processingOutputConfig,
                  ("StoppingCondition" Core..=) Core.<$> stoppingCondition,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateProcessingJob where
        type Rs CreateProcessingJob = CreateProcessingJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProcessingJobResponse' Core.<$>
                   (x Core..: "ProcessingJobArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProcessingJobResponse' smart constructor.
data CreateProcessingJobResponse = CreateProcessingJobResponse'
  { processingJobArn :: Types.ProcessingJobArn
    -- ^ The Amazon Resource Name (ARN) of the processing job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProcessingJobResponse' value with any optional fields omitted.
mkCreateProcessingJobResponse
    :: Types.ProcessingJobArn -- ^ 'processingJobArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateProcessingJobResponse
mkCreateProcessingJobResponse processingJobArn responseStatus
  = CreateProcessingJobResponse'{processingJobArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the processing job.
--
-- /Note:/ Consider using 'processingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjrrsProcessingJobArn :: Lens.Lens' CreateProcessingJobResponse Types.ProcessingJobArn
cpjrrsProcessingJobArn = Lens.field @"processingJobArn"
{-# INLINEABLE cpjrrsProcessingJobArn #-}
{-# DEPRECATED processingJobArn "Use generic-lens or generic-optics with 'processingJobArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpjrrsResponseStatus :: Lens.Lens' CreateProcessingJobResponse Core.Int
cpjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
