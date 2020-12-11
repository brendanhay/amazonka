{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ctjModelClientConfig,
    ctjBatchStrategy,
    ctjMaxPayloadInMB,
    ctjEnvironment,
    ctjExperimentConfig,
    ctjMaxConcurrentTransforms,
    ctjDataProcessing,
    ctjTags,
    ctjTransformJobName,
    ctjModelName,
    ctjTransformInput,
    ctjTransformOutput,
    ctjTransformResources,

    -- * Destructuring the response
    CreateTransformJobResponse (..),
    mkCreateTransformJobResponse,

    -- ** Response lenses
    ctjrsResponseStatus,
    ctjrsTransformJobARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateTransformJob' smart constructor.
data CreateTransformJob = CreateTransformJob'
  { modelClientConfig ::
      Lude.Maybe ModelClientConfig,
    batchStrategy :: Lude.Maybe BatchStrategy,
    maxPayloadInMB :: Lude.Maybe Lude.Natural,
    environment ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    experimentConfig :: Lude.Maybe ExperimentConfig,
    maxConcurrentTransforms :: Lude.Maybe Lude.Natural,
    dataProcessing :: Lude.Maybe DataProcessing,
    tags :: Lude.Maybe [Tag],
    transformJobName :: Lude.Text,
    modelName :: Lude.Text,
    transformInput :: TransformInput,
    transformOutput :: TransformOutput,
    transformResources :: TransformResources
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransformJob' with the minimum fields required to make a request.
--
-- * 'batchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
--
-- To enable the batch strategy, you must set the @SplitType@ property to @Line@ , @RecordIO@ , or @TFRecord@ .
-- To use only one record when making an HTTP invocation request to a container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to @Line@ .
-- To fit as many records in a mini-batch as can fit within the @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and @SplitType@ to @Line@ .
-- * 'dataProcessing' - The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
-- * 'environment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
-- * 'experimentConfig' - Undocumented field.
-- * 'maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@ or left unset, Amazon SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is @1@ . For more information on execution-parameters, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests> . For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
-- * 'maxPayloadInMB' - The maximum allowed size of the payload, in MB. A /payload/ is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is @6@ MB.
--
-- For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to @0@ . This feature works only in supported algorithms. Currently, Amazon SageMaker built-in algorithms do not support HTTP chunked encoding.
-- * 'modelClientConfig' - Configures the timeout and maximum number of retries for processing a transform job invocation.
-- * 'modelName' - The name of the model that you want to use for the transform job. @ModelName@ must be the name of an existing Amazon SageMaker model within an AWS Region in an AWS account.
-- * 'tags' - (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
-- * 'transformInput' - Describes the input source and the way the transform job consumes it.
-- * 'transformJobName' - The name of the transform job. The name must be unique within an AWS Region in an AWS account.
-- * 'transformOutput' - Describes the results of the transform job.
-- * 'transformResources' - Describes the resources, including ML instance types and ML instance count, to use for the transform job.
mkCreateTransformJob ::
  -- | 'transformJobName'
  Lude.Text ->
  -- | 'modelName'
  Lude.Text ->
  -- | 'transformInput'
  TransformInput ->
  -- | 'transformOutput'
  TransformOutput ->
  -- | 'transformResources'
  TransformResources ->
  CreateTransformJob
mkCreateTransformJob
  pTransformJobName_
  pModelName_
  pTransformInput_
  pTransformOutput_
  pTransformResources_ =
    CreateTransformJob'
      { modelClientConfig = Lude.Nothing,
        batchStrategy = Lude.Nothing,
        maxPayloadInMB = Lude.Nothing,
        environment = Lude.Nothing,
        experimentConfig = Lude.Nothing,
        maxConcurrentTransforms = Lude.Nothing,
        dataProcessing = Lude.Nothing,
        tags = Lude.Nothing,
        transformJobName = pTransformJobName_,
        modelName = pModelName_,
        transformInput = pTransformInput_,
        transformOutput = pTransformOutput_,
        transformResources = pTransformResources_
      }

-- | Configures the timeout and maximum number of retries for processing a transform job invocation.
--
-- /Note:/ Consider using 'modelClientConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjModelClientConfig :: Lens.Lens' CreateTransformJob (Lude.Maybe ModelClientConfig)
ctjModelClientConfig = Lens.lens (modelClientConfig :: CreateTransformJob -> Lude.Maybe ModelClientConfig) (\s a -> s {modelClientConfig = a} :: CreateTransformJob)
{-# DEPRECATED ctjModelClientConfig "Use generic-lens or generic-optics with 'modelClientConfig' instead." #-}

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
--
-- To enable the batch strategy, you must set the @SplitType@ property to @Line@ , @RecordIO@ , or @TFRecord@ .
-- To use only one record when making an HTTP invocation request to a container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to @Line@ .
-- To fit as many records in a mini-batch as can fit within the @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and @SplitType@ to @Line@ .
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjBatchStrategy :: Lens.Lens' CreateTransformJob (Lude.Maybe BatchStrategy)
ctjBatchStrategy = Lens.lens (batchStrategy :: CreateTransformJob -> Lude.Maybe BatchStrategy) (\s a -> s {batchStrategy = a} :: CreateTransformJob)
{-# DEPRECATED ctjBatchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead." #-}

-- | The maximum allowed size of the payload, in MB. A /payload/ is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is @6@ MB.
--
-- For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to @0@ . This feature works only in supported algorithms. Currently, Amazon SageMaker built-in algorithms do not support HTTP chunked encoding.
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjMaxPayloadInMB :: Lens.Lens' CreateTransformJob (Lude.Maybe Lude.Natural)
ctjMaxPayloadInMB = Lens.lens (maxPayloadInMB :: CreateTransformJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxPayloadInMB = a} :: CreateTransformJob)
{-# DEPRECATED ctjMaxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead." #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjEnvironment :: Lens.Lens' CreateTransformJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ctjEnvironment = Lens.lens (environment :: CreateTransformJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: CreateTransformJob)
{-# DEPRECATED ctjEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjExperimentConfig :: Lens.Lens' CreateTransformJob (Lude.Maybe ExperimentConfig)
ctjExperimentConfig = Lens.lens (experimentConfig :: CreateTransformJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: CreateTransformJob)
{-# DEPRECATED ctjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@ or left unset, Amazon SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is @1@ . For more information on execution-parameters, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests> . For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjMaxConcurrentTransforms :: Lens.Lens' CreateTransformJob (Lude.Maybe Lude.Natural)
ctjMaxConcurrentTransforms = Lens.lens (maxConcurrentTransforms :: CreateTransformJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxConcurrentTransforms = a} :: CreateTransformJob)
{-# DEPRECATED ctjMaxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead." #-}

-- | The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
--
-- /Note:/ Consider using 'dataProcessing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjDataProcessing :: Lens.Lens' CreateTransformJob (Lude.Maybe DataProcessing)
ctjDataProcessing = Lens.lens (dataProcessing :: CreateTransformJob -> Lude.Maybe DataProcessing) (\s a -> s {dataProcessing = a} :: CreateTransformJob)
{-# DEPRECATED ctjDataProcessing "Use generic-lens or generic-optics with 'dataProcessing' instead." #-}

-- | (Optional) An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTags :: Lens.Lens' CreateTransformJob (Lude.Maybe [Tag])
ctjTags = Lens.lens (tags :: CreateTransformJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTransformJob)
{-# DEPRECATED ctjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the transform job. The name must be unique within an AWS Region in an AWS account.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformJobName :: Lens.Lens' CreateTransformJob Lude.Text
ctjTransformJobName = Lens.lens (transformJobName :: CreateTransformJob -> Lude.Text) (\s a -> s {transformJobName = a} :: CreateTransformJob)
{-# DEPRECATED ctjTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

-- | The name of the model that you want to use for the transform job. @ModelName@ must be the name of an existing Amazon SageMaker model within an AWS Region in an AWS account.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjModelName :: Lens.Lens' CreateTransformJob Lude.Text
ctjModelName = Lens.lens (modelName :: CreateTransformJob -> Lude.Text) (\s a -> s {modelName = a} :: CreateTransformJob)
{-# DEPRECATED ctjModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Describes the input source and the way the transform job consumes it.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformInput :: Lens.Lens' CreateTransformJob TransformInput
ctjTransformInput = Lens.lens (transformInput :: CreateTransformJob -> TransformInput) (\s a -> s {transformInput = a} :: CreateTransformJob)
{-# DEPRECATED ctjTransformInput "Use generic-lens or generic-optics with 'transformInput' instead." #-}

-- | Describes the results of the transform job.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformOutput :: Lens.Lens' CreateTransformJob TransformOutput
ctjTransformOutput = Lens.lens (transformOutput :: CreateTransformJob -> TransformOutput) (\s a -> s {transformOutput = a} :: CreateTransformJob)
{-# DEPRECATED ctjTransformOutput "Use generic-lens or generic-optics with 'transformOutput' instead." #-}

-- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjTransformResources :: Lens.Lens' CreateTransformJob TransformResources
ctjTransformResources = Lens.lens (transformResources :: CreateTransformJob -> TransformResources) (\s a -> s {transformResources = a} :: CreateTransformJob)
{-# DEPRECATED ctjTransformResources "Use generic-lens or generic-optics with 'transformResources' instead." #-}

instance Lude.AWSRequest CreateTransformJob where
  type Rs CreateTransformJob = CreateTransformJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTransformJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "TransformJobArn")
      )

instance Lude.ToHeaders CreateTransformJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateTransformJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTransformJob where
  toJSON CreateTransformJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ModelClientConfig" Lude..=) Lude.<$> modelClientConfig,
            ("BatchStrategy" Lude..=) Lude.<$> batchStrategy,
            ("MaxPayloadInMB" Lude..=) Lude.<$> maxPayloadInMB,
            ("Environment" Lude..=) Lude.<$> environment,
            ("ExperimentConfig" Lude..=) Lude.<$> experimentConfig,
            ("MaxConcurrentTransforms" Lude..=)
              Lude.<$> maxConcurrentTransforms,
            ("DataProcessing" Lude..=) Lude.<$> dataProcessing,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("TransformJobName" Lude..= transformJobName),
            Lude.Just ("ModelName" Lude..= modelName),
            Lude.Just ("TransformInput" Lude..= transformInput),
            Lude.Just ("TransformOutput" Lude..= transformOutput),
            Lude.Just ("TransformResources" Lude..= transformResources)
          ]
      )

instance Lude.ToPath CreateTransformJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransformJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTransformJobResponse' smart constructor.
data CreateTransformJobResponse = CreateTransformJobResponse'
  { responseStatus ::
      Lude.Int,
    transformJobARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransformJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transformJobARN' - The Amazon Resource Name (ARN) of the transform job.
mkCreateTransformJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'transformJobARN'
  Lude.Text ->
  CreateTransformJobResponse
mkCreateTransformJobResponse pResponseStatus_ pTransformJobARN_ =
  CreateTransformJobResponse'
    { responseStatus = pResponseStatus_,
      transformJobARN = pTransformJobARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjrsResponseStatus :: Lens.Lens' CreateTransformJobResponse Lude.Int
ctjrsResponseStatus = Lens.lens (responseStatus :: CreateTransformJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransformJobResponse)
{-# DEPRECATED ctjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctjrsTransformJobARN :: Lens.Lens' CreateTransformJobResponse Lude.Text
ctjrsTransformJobARN = Lens.lens (transformJobARN :: CreateTransformJobResponse -> Lude.Text) (\s a -> s {transformJobARN = a} :: CreateTransformJobResponse)
{-# DEPRECATED ctjrsTransformJobARN "Use generic-lens or generic-optics with 'transformJobARN' instead." #-}
