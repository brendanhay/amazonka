{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateCompilationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model compilation job. After the model has been compiled, Amazon SageMaker saves the resulting model artifacts to an Amazon Simple Storage Service (Amazon S3) bucket that you specify. 
--
-- If you choose to host your model using Amazon SageMaker hosting services, you can use the resulting model artifacts as part of the model. You can also use the artifacts with AWS IoT Greengrass. In that case, deploy them as an ML resource.
-- In the request body, you provide the following:
--
--     * A name for the compilation job
--
--
--     * Information about the input model artifacts 
--
--
--     * The output location for the compiled model and the device (target) that the model runs on 
--
--
--     * The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker assumes to perform the model compilation job. 
--
--
-- You can also provide a @Tag@ to track the model compilation job's resource use and costs. The response body contains the @CompilationJobArn@ for the compiled job.
-- To stop a model compilation job, use 'StopCompilationJob' . To get information about a particular model compilation job, use 'DescribeCompilationJob' . To get information about multiple model compilation jobs, use 'ListCompilationJobs' .
module Network.AWS.SageMaker.CreateCompilationJob
    (
    -- * Creating a request
      CreateCompilationJob (..)
    , mkCreateCompilationJob
    -- ** Request lenses
    , ccjCompilationJobName
    , ccjRoleArn
    , ccjInputConfig
    , ccjOutputConfig
    , ccjStoppingCondition
    , ccjTags

    -- * Destructuring the response
    , CreateCompilationJobResponse (..)
    , mkCreateCompilationJobResponse
    -- ** Response lenses
    , ccjrrsCompilationJobArn
    , ccjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateCompilationJob' smart constructor.
data CreateCompilationJob = CreateCompilationJob'
  { compilationJobName :: Types.EntityName
    -- ^ A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account. 
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf. 
--
-- During model compilation, Amazon SageMaker needs your permission to:
--
--     * Read input data from an S3 bucket
--
--
--     * Write model artifacts to an S3 bucket
--
--
--     * Write logs to Amazon CloudWatch Logs
--
--
--     * Publish metrics to Amazon CloudWatch
--
--
-- You grant permissions for all of these tasks to an IAM role. To pass this role to Amazon SageMaker, the caller of this API must have the @iam:PassRole@ permission. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.> 
  , inputConfig :: Types.InputConfig
    -- ^ Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
  , outputConfig :: Types.OutputConfig
    -- ^ Provides information about the output location for the compiled model and the target device the model runs on.
  , stoppingCondition :: Types.StoppingCondition
    -- ^ Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array of key-value pairs that you want to use to organize and track your AWS resource costs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCompilationJob' value with any optional fields omitted.
mkCreateCompilationJob
    :: Types.EntityName -- ^ 'compilationJobName'
    -> Types.RoleArn -- ^ 'roleArn'
    -> Types.InputConfig -- ^ 'inputConfig'
    -> Types.OutputConfig -- ^ 'outputConfig'
    -> Types.StoppingCondition -- ^ 'stoppingCondition'
    -> CreateCompilationJob
mkCreateCompilationJob compilationJobName roleArn inputConfig
  outputConfig stoppingCondition
  = CreateCompilationJob'{compilationJobName, roleArn, inputConfig,
                          outputConfig, stoppingCondition, tags = Core.Nothing}

-- | A name for the model compilation job. The name must be unique within the AWS Region and within your AWS account. 
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjCompilationJobName :: Lens.Lens' CreateCompilationJob Types.EntityName
ccjCompilationJobName = Lens.field @"compilationJobName"
{-# INLINEABLE ccjCompilationJobName #-}
{-# DEPRECATED compilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead"  #-}

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf. 
--
-- During model compilation, Amazon SageMaker needs your permission to:
--
--     * Read input data from an S3 bucket
--
--
--     * Write model artifacts to an S3 bucket
--
--
--     * Write logs to Amazon CloudWatch Logs
--
--
--     * Publish metrics to Amazon CloudWatch
--
--
-- You grant permissions for all of these tasks to an IAM role. To pass this role to Amazon SageMaker, the caller of this API must have the @iam:PassRole@ permission. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles.> 
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjRoleArn :: Lens.Lens' CreateCompilationJob Types.RoleArn
ccjRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ccjRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Provides information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjInputConfig :: Lens.Lens' CreateCompilationJob Types.InputConfig
ccjInputConfig = Lens.field @"inputConfig"
{-# INLINEABLE ccjInputConfig #-}
{-# DEPRECATED inputConfig "Use generic-lens or generic-optics with 'inputConfig' instead"  #-}

-- | Provides information about the output location for the compiled model and the target device the model runs on.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjOutputConfig :: Lens.Lens' CreateCompilationJob Types.OutputConfig
ccjOutputConfig = Lens.field @"outputConfig"
{-# INLINEABLE ccjOutputConfig #-}
{-# DEPRECATED outputConfig "Use generic-lens or generic-optics with 'outputConfig' instead"  #-}

-- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjStoppingCondition :: Lens.Lens' CreateCompilationJob Types.StoppingCondition
ccjStoppingCondition = Lens.field @"stoppingCondition"
{-# INLINEABLE ccjStoppingCondition #-}
{-# DEPRECATED stoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead"  #-}

-- | An array of key-value pairs that you want to use to organize and track your AWS resource costs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ . 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjTags :: Lens.Lens' CreateCompilationJob (Core.Maybe [Types.Tag])
ccjTags = Lens.field @"tags"
{-# INLINEABLE ccjTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCompilationJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCompilationJob where
        toHeaders CreateCompilationJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateCompilationJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCompilationJob where
        toJSON CreateCompilationJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CompilationJobName" Core..= compilationJobName),
                  Core.Just ("RoleArn" Core..= roleArn),
                  Core.Just ("InputConfig" Core..= inputConfig),
                  Core.Just ("OutputConfig" Core..= outputConfig),
                  Core.Just ("StoppingCondition" Core..= stoppingCondition),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCompilationJob where
        type Rs CreateCompilationJob = CreateCompilationJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCompilationJobResponse' Core.<$>
                   (x Core..: "CompilationJobArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCompilationJobResponse' smart constructor.
data CreateCompilationJobResponse = CreateCompilationJobResponse'
  { compilationJobArn :: Types.CompilationJobArn
    -- ^ If the action is successful, the service sends back an HTTP 200 response. Amazon SageMaker returns the following data in JSON format:
--
--
--     * @CompilationJobArn@ : The Amazon Resource Name (ARN) of the compiled job.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCompilationJobResponse' value with any optional fields omitted.
mkCreateCompilationJobResponse
    :: Types.CompilationJobArn -- ^ 'compilationJobArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateCompilationJobResponse
mkCreateCompilationJobResponse compilationJobArn responseStatus
  = CreateCompilationJobResponse'{compilationJobArn, responseStatus}

-- | If the action is successful, the service sends back an HTTP 200 response. Amazon SageMaker returns the following data in JSON format:
--
--
--     * @CompilationJobArn@ : The Amazon Resource Name (ARN) of the compiled job.
--
--
--
-- /Note:/ Consider using 'compilationJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjrrsCompilationJobArn :: Lens.Lens' CreateCompilationJobResponse Types.CompilationJobArn
ccjrrsCompilationJobArn = Lens.field @"compilationJobArn"
{-# INLINEABLE ccjrrsCompilationJobArn #-}
{-# DEPRECATED compilationJobArn "Use generic-lens or generic-optics with 'compilationJobArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccjrrsResponseStatus :: Lens.Lens' CreateCompilationJobResponse Core.Int
ccjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
