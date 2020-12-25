{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateTransformJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a transform job. A transform job uses a trained model to get inferences on a dataset and saves these results to an Amazon S3 location that you specify.
--
-- To perform batch transformations, you create a transform job and use the data that you have readily available.
-- In the request body, you provide the following:
--
--     * @TransformJobName@ - Identifies the transform job. The name must be unique within an AWS Region in an AWS account.
--
--
--     * @ModelName@ - Identifies the model to use. @ModelName@ must be the name of an existing Amazon SageMaker model in the same AWS Region and AWS account. For information on creating a model, see 'CreateModel' .
--
--
--     * @TransformInput@ - Describes the dataset to be transformed and the Amazon S3 location where it is stored.
--
--
--     * @TransformOutput@ - Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
--
--     * @TransformResources@ - Identifies the ML compute instances for the transform job.
--
--
-- For more information about how batch transformation works, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform.html Batch Transform> .
module Network.AWS.SageMaker.CreateTransformJob
  ( -- * Creating a request
    CreateTransformJob (..),
    mkCreateTransformJob,

    -- ** Request lenses
    ctjTransformJobName,
    ctjModelName,
    ctjTransformInput,
    ctjTransformOutput,
    ctjTransformResources,
    ctjBatchStrategy,
    ctjDataProcessing,
    ctjEnvironment,
    ctjExperimentConfig,
    ctjMaxConcurrentTransforms,
    ctjMaxPayloadInMB,
    ctjModelClientConfig,
    ctjTags,

    -- * Destructuring the response
    CreateTransformJobResponse (..),
    mkCreateTransformJobResponse,

    -- ** Response lenses
    ctjrrsTransformJobArn,
    ctjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateTransformJob' smart constructor.
data CreateTransformJob = CreateTransformJob'
  { -- | The name of the transform job. The name must be unique within an AWS Region in an AWS account.
    transformJobName :: Types.TransformJobName,
    -- | The name of the model that you want to use for the transform job. @ModelName@ must be the name of an existing Amazon SageMaker model within an AWS Region in an AWS account.
    modelName :: Types.ModelName,
    -- | Describes the input source and the way the transform job consumes it.
    transformInput :: Types.TransformInput,
    -- | Describes the results of the transform job.
    transformOutput :: Types.TransformOutput,
    -- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
    transformResources :: Types.TransformResources,
    -- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
    --
    -- To enable the batch strategy, you must set the @SplitType@ property to @Line@ , @RecordIO@ , or @TFRecord@ .
    -- To use only one record when making an HTTP invocation request to a container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to @Line@ .
    -- To fit as many records in a mini-batch as can fit within the @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and @SplitType@ to @Line@ .
    batchStrategy :: Core.Maybe Types.BatchStrategy,
    -- | The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
    dataProcessing :: Core.Maybe Types.DataProcessing,
    -- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
    environment :: Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue),
    experimentConfig :: Core.Maybe Types.ExperimentConfig,
    -- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@ or left unset, Amazon SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is @1@ . For more information on execution-parameters, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests> . For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
    maxConcurrentTransforms :: Core.Maybe Core.Natural,
    -- | The maximum allowed size of the payload, in MB. A /payload/ is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is @6@ MB.
    --
    -- For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to @0@ . This feature works only in supported algorithms. Currently, Amazon SageMaker built-in algorithms do not support HTTP chunked encoding.
    maxPayloadInMB :: Core.Maybe Core.Natural,
    -- | Configures the timeout and maximum number of retries for processing a transform job invocation.
    modelClientConfig :: Core.Maybe Types.ModelClientConfig,
    -- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransformJob' value with any optional fields omitted.
mkCreateTransformJob ::
  -- | 'transformJobName'
  Types.TransformJobName ->
  -- | 'modelName'
  Types.ModelName ->
  -- | 'transformInput'
  Types.TransformInput ->
  -- | 'transformOutput'
  Types.TransformOutput ->
  -- | 'transformResources'
  Types.TransformResources ->
  CreateTransformJob
mkCreateTransformJob
  transformJobName
  modelName
  transformInput
  transformOutput
  transformResources =
    CreateTransformJob'
      { transformJobName,
        modelName,
        transformInput,
        transformOutput,
        transformResources,
        batchStrategy = Core.Nothing,
        dataProcessing = Core.Nothing,
        environment = Core.Nothing,
        experimentConfig = Core.Nothing,
        maxConcurrentTransforms = Core.Nothing,
        maxPayloadInMB = Core.Nothing,
        modelClientConfig = Core.Nothing,
        tags = Core.Nothing
      }

-- | The name of the transform job. The name must be unique within an AWS Region in an AWS account.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformJobName :: Lens.Lens' CreateTransformJob Types.TransformJobName
ctjTransformJobName = Lens.field @"transformJobName"
{-# DEPRECATED ctjTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

-- | The name of the model that you want to use for the transform job. @ModelName@ must be the name of an existing Amazon SageMaker model within an AWS Region in an AWS account.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjModelName :: Lens.Lens' CreateTransformJob Types.ModelName
ctjModelName = Lens.field @"modelName"
{-# DEPRECATED ctjModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Describes the input source and the way the transform job consumes it.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformInput :: Lens.Lens' CreateTransformJob Types.TransformInput
ctjTransformInput = Lens.field @"transformInput"
{-# DEPRECATED ctjTransformInput "Use generic-lens or generic-optics with 'transformInput' instead." #-}

-- | Describes the results of the transform job.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformOutput :: Lens.Lens' CreateTransformJob Types.TransformOutput
ctjTransformOutput = Lens.field @"transformOutput"
{-# DEPRECATED ctjTransformOutput "Use generic-lens or generic-optics with 'transformOutput' instead." #-}

-- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformResources :: Lens.Lens' CreateTransformJob Types.TransformResources
ctjTransformResources = Lens.field @"transformResources"
{-# DEPRECATED ctjTransformResources "Use generic-lens or generic-optics with 'transformResources' instead." #-}

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
--
-- To enable the batch strategy, you must set the @SplitType@ property to @Line@ , @RecordIO@ , or @TFRecord@ .
-- To use only one record when making an HTTP invocation request to a container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to @Line@ .
-- To fit as many records in a mini-batch as can fit within the @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and @SplitType@ to @Line@ .
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjBatchStrategy :: Lens.Lens' CreateTransformJob (Core.Maybe Types.BatchStrategy)
ctjBatchStrategy = Lens.field @"batchStrategy"
{-# DEPRECATED ctjBatchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead." #-}

-- | The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
--
-- /Note:/ Consider using 'dataProcessing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjDataProcessing :: Lens.Lens' CreateTransformJob (Core.Maybe Types.DataProcessing)
ctjDataProcessing = Lens.field @"dataProcessing"
{-# DEPRECATED ctjDataProcessing "Use generic-lens or generic-optics with 'dataProcessing' instead." #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjEnvironment :: Lens.Lens' CreateTransformJob (Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue))
ctjEnvironment = Lens.field @"environment"
{-# DEPRECATED ctjEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjExperimentConfig :: Lens.Lens' CreateTransformJob (Core.Maybe Types.ExperimentConfig)
ctjExperimentConfig = Lens.field @"experimentConfig"
{-# DEPRECATED ctjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@ or left unset, Amazon SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is @1@ . For more information on execution-parameters, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests> . For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjMaxConcurrentTransforms :: Lens.Lens' CreateTransformJob (Core.Maybe Core.Natural)
ctjMaxConcurrentTransforms = Lens.field @"maxConcurrentTransforms"
{-# DEPRECATED ctjMaxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead." #-}

-- | The maximum allowed size of the payload, in MB. A /payload/ is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is @6@ MB.
--
-- For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to @0@ . This feature works only in supported algorithms. Currently, Amazon SageMaker built-in algorithms do not support HTTP chunked encoding.
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjMaxPayloadInMB :: Lens.Lens' CreateTransformJob (Core.Maybe Core.Natural)
ctjMaxPayloadInMB = Lens.field @"maxPayloadInMB"
{-# DEPRECATED ctjMaxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead." #-}

-- | Configures the timeout and maximum number of retries for processing a transform job invocation.
--
-- /Note:/ Consider using 'modelClientConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjModelClientConfig :: Lens.Lens' CreateTransformJob (Core.Maybe Types.ModelClientConfig)
ctjModelClientConfig = Lens.field @"modelClientConfig"
{-# DEPRECATED ctjModelClientConfig "Use generic-lens or generic-optics with 'modelClientConfig' instead." #-}

-- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTags :: Lens.Lens' CreateTransformJob (Core.Maybe [Types.Tag])
ctjTags = Lens.field @"tags"
{-# DEPRECATED ctjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateTransformJob where
  toJSON CreateTransformJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformJobName" Core..= transformJobName),
            Core.Just ("ModelName" Core..= modelName),
            Core.Just ("TransformInput" Core..= transformInput),
            Core.Just ("TransformOutput" Core..= transformOutput),
            Core.Just ("TransformResources" Core..= transformResources),
            ("BatchStrategy" Core..=) Core.<$> batchStrategy,
            ("DataProcessing" Core..=) Core.<$> dataProcessing,
            ("Environment" Core..=) Core.<$> environment,
            ("ExperimentConfig" Core..=) Core.<$> experimentConfig,
            ("MaxConcurrentTransforms" Core..=)
              Core.<$> maxConcurrentTransforms,
            ("MaxPayloadInMB" Core..=) Core.<$> maxPayloadInMB,
            ("ModelClientConfig" Core..=) Core.<$> modelClientConfig,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateTransformJob where
  type Rs CreateTransformJob = CreateTransformJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateTransformJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTransformJobResponse'
            Core.<$> (x Core..: "TransformJobArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTransformJobResponse' smart constructor.
data CreateTransformJobResponse = CreateTransformJobResponse'
  { -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Types.TransformJobArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransformJobResponse' value with any optional fields omitted.
mkCreateTransformJobResponse ::
  -- | 'transformJobArn'
  Types.TransformJobArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateTransformJobResponse
mkCreateTransformJobResponse transformJobArn responseStatus =
  CreateTransformJobResponse' {transformJobArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjrrsTransformJobArn :: Lens.Lens' CreateTransformJobResponse Types.TransformJobArn
ctjrrsTransformJobArn = Lens.field @"transformJobArn"
{-# DEPRECATED ctjrrsTransformJobArn "Use generic-lens or generic-optics with 'transformJobArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjrrsResponseStatus :: Lens.Lens' CreateTransformJobResponse Core.Int
ctjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
