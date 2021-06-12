{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateTransformJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a transform job. A transform job uses a trained model to get
-- inferences on a dataset and saves these results to an Amazon S3 location
-- that you specify.
--
-- To perform batch transformations, you create a transform job and use the
-- data that you have readily available.
--
-- In the request body, you provide the following:
--
-- -   @TransformJobName@ - Identifies the transform job. The name must be
--     unique within an AWS Region in an AWS account.
--
-- -   @ModelName@ - Identifies the model to use. @ModelName@ must be the
--     name of an existing Amazon SageMaker model in the same AWS Region
--     and AWS account. For information on creating a model, see
--     CreateModel.
--
-- -   @TransformInput@ - Describes the dataset to be transformed and the
--     Amazon S3 location where it is stored.
--
-- -   @TransformOutput@ - Identifies the Amazon S3 location where you want
--     Amazon SageMaker to save the results from the transform job.
--
-- -   @TransformResources@ - Identifies the ML compute instances for the
--     transform job.
--
-- For more information about how batch transformation works, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform.html Batch Transform>.
module Network.AWS.SageMaker.CreateTransformJob
  ( -- * Creating a Request
    CreateTransformJob (..),
    newCreateTransformJob,

    -- * Request Lenses
    createTransformJob_experimentConfig,
    createTransformJob_maxConcurrentTransforms,
    createTransformJob_environment,
    createTransformJob_maxPayloadInMB,
    createTransformJob_batchStrategy,
    createTransformJob_modelClientConfig,
    createTransformJob_tags,
    createTransformJob_dataProcessing,
    createTransformJob_transformJobName,
    createTransformJob_modelName,
    createTransformJob_transformInput,
    createTransformJob_transformOutput,
    createTransformJob_transformResources,

    -- * Destructuring the Response
    CreateTransformJobResponse (..),
    newCreateTransformJobResponse,

    -- * Response Lenses
    createTransformJobResponse_httpStatus,
    createTransformJobResponse_transformJobArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateTransformJob' smart constructor.
data CreateTransformJob = CreateTransformJob'
  { experimentConfig :: Core.Maybe ExperimentConfig,
    -- | The maximum number of parallel requests that can be sent to each
    -- instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@
    -- or left unset, Amazon SageMaker checks the optional execution-parameters
    -- to determine the settings for your chosen algorithm. If the
    -- execution-parameters endpoint is not enabled, the default value is @1@.
    -- For more information on execution-parameters, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests>.
    -- For built-in algorithms, you don\'t need to set a value for
    -- @MaxConcurrentTransforms@.
    maxConcurrentTransforms :: Core.Maybe Core.Natural,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The maximum allowed size of the payload, in MB. A /payload/ is the data
    -- portion of a record (without metadata). The value in @MaxPayloadInMB@
    -- must be greater than, or equal to, the size of a single record. To
    -- estimate the size of a record in MB, divide the size of your dataset by
    -- the number of records. To ensure that the records fit within the maximum
    -- payload size, we recommend using a slightly larger value. The default
    -- value is @6@ MB.
    --
    -- For cases where the payload might be arbitrarily large and is
    -- transmitted using HTTP chunked encoding, set the value to @0@. This
    -- feature works only in supported algorithms. Currently, Amazon SageMaker
    -- built-in algorithms do not support HTTP chunked encoding.
    maxPayloadInMB :: Core.Maybe Core.Natural,
    -- | Specifies the number of records to include in a mini-batch for an HTTP
    -- inference request. A /record/ // is a single unit of input data that
    -- inference can be made on. For example, a single line in a CSV file is a
    -- record.
    --
    -- To enable the batch strategy, you must set the @SplitType@ property to
    -- @Line@, @RecordIO@, or @TFRecord@.
    --
    -- To use only one record when making an HTTP invocation request to a
    -- container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to
    -- @Line@.
    --
    -- To fit as many records in a mini-batch as can fit within the
    -- @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and
    -- @SplitType@ to @Line@.
    batchStrategy :: Core.Maybe BatchStrategy,
    -- | Configures the timeout and maximum number of retries for processing a
    -- transform job invocation.
    modelClientConfig :: Core.Maybe ModelClientConfig,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Core.Maybe [Tag],
    -- | The data structure used to specify the data to be used for inference in
    -- a batch transform job and to associate the data that is relevant to the
    -- prediction results in the output. The input filter provided allows you
    -- to exclude input data that is not needed for inference in a batch
    -- transform job. The output filter provided allows you to include input
    -- data relevant to interpreting the predictions in the output from the
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records>.
    dataProcessing :: Core.Maybe DataProcessing,
    -- | The name of the transform job. The name must be unique within an AWS
    -- Region in an AWS account.
    transformJobName :: Core.Text,
    -- | The name of the model that you want to use for the transform job.
    -- @ModelName@ must be the name of an existing Amazon SageMaker model
    -- within an AWS Region in an AWS account.
    modelName :: Core.Text,
    -- | Describes the input source and the way the transform job consumes it.
    transformInput :: TransformInput,
    -- | Describes the results of the transform job.
    transformOutput :: TransformOutput,
    -- | Describes the resources, including ML instance types and ML instance
    -- count, to use for the transform job.
    transformResources :: TransformResources
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTransformJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentConfig', 'createTransformJob_experimentConfig' - Undocumented member.
--
-- 'maxConcurrentTransforms', 'createTransformJob_maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each
-- instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@
-- or left unset, Amazon SageMaker checks the optional execution-parameters
-- to determine the settings for your chosen algorithm. If the
-- execution-parameters endpoint is not enabled, the default value is @1@.
-- For more information on execution-parameters, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests>.
-- For built-in algorithms, you don\'t need to set a value for
-- @MaxConcurrentTransforms@.
--
-- 'environment', 'createTransformJob_environment' - The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
--
-- 'maxPayloadInMB', 'createTransformJob_maxPayloadInMB' - The maximum allowed size of the payload, in MB. A /payload/ is the data
-- portion of a record (without metadata). The value in @MaxPayloadInMB@
-- must be greater than, or equal to, the size of a single record. To
-- estimate the size of a record in MB, divide the size of your dataset by
-- the number of records. To ensure that the records fit within the maximum
-- payload size, we recommend using a slightly larger value. The default
-- value is @6@ MB.
--
-- For cases where the payload might be arbitrarily large and is
-- transmitted using HTTP chunked encoding, set the value to @0@. This
-- feature works only in supported algorithms. Currently, Amazon SageMaker
-- built-in algorithms do not support HTTP chunked encoding.
--
-- 'batchStrategy', 'createTransformJob_batchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP
-- inference request. A /record/ // is a single unit of input data that
-- inference can be made on. For example, a single line in a CSV file is a
-- record.
--
-- To enable the batch strategy, you must set the @SplitType@ property to
-- @Line@, @RecordIO@, or @TFRecord@.
--
-- To use only one record when making an HTTP invocation request to a
-- container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to
-- @Line@.
--
-- To fit as many records in a mini-batch as can fit within the
-- @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and
-- @SplitType@ to @Line@.
--
-- 'modelClientConfig', 'createTransformJob_modelClientConfig' - Configures the timeout and maximum number of retries for processing a
-- transform job invocation.
--
-- 'tags', 'createTransformJob_tags' - (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'dataProcessing', 'createTransformJob_dataProcessing' - The data structure used to specify the data to be used for inference in
-- a batch transform job and to associate the data that is relevant to the
-- prediction results in the output. The input filter provided allows you
-- to exclude input data that is not needed for inference in a batch
-- transform job. The output filter provided allows you to include input
-- data relevant to interpreting the predictions in the output from the
-- job. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records>.
--
-- 'transformJobName', 'createTransformJob_transformJobName' - The name of the transform job. The name must be unique within an AWS
-- Region in an AWS account.
--
-- 'modelName', 'createTransformJob_modelName' - The name of the model that you want to use for the transform job.
-- @ModelName@ must be the name of an existing Amazon SageMaker model
-- within an AWS Region in an AWS account.
--
-- 'transformInput', 'createTransformJob_transformInput' - Describes the input source and the way the transform job consumes it.
--
-- 'transformOutput', 'createTransformJob_transformOutput' - Describes the results of the transform job.
--
-- 'transformResources', 'createTransformJob_transformResources' - Describes the resources, including ML instance types and ML instance
-- count, to use for the transform job.
newCreateTransformJob ::
  -- | 'transformJobName'
  Core.Text ->
  -- | 'modelName'
  Core.Text ->
  -- | 'transformInput'
  TransformInput ->
  -- | 'transformOutput'
  TransformOutput ->
  -- | 'transformResources'
  TransformResources ->
  CreateTransformJob
newCreateTransformJob
  pTransformJobName_
  pModelName_
  pTransformInput_
  pTransformOutput_
  pTransformResources_ =
    CreateTransformJob'
      { experimentConfig =
          Core.Nothing,
        maxConcurrentTransforms = Core.Nothing,
        environment = Core.Nothing,
        maxPayloadInMB = Core.Nothing,
        batchStrategy = Core.Nothing,
        modelClientConfig = Core.Nothing,
        tags = Core.Nothing,
        dataProcessing = Core.Nothing,
        transformJobName = pTransformJobName_,
        modelName = pModelName_,
        transformInput = pTransformInput_,
        transformOutput = pTransformOutput_,
        transformResources = pTransformResources_
      }

-- | Undocumented member.
createTransformJob_experimentConfig :: Lens.Lens' CreateTransformJob (Core.Maybe ExperimentConfig)
createTransformJob_experimentConfig = Lens.lens (\CreateTransformJob' {experimentConfig} -> experimentConfig) (\s@CreateTransformJob' {} a -> s {experimentConfig = a} :: CreateTransformJob)

-- | The maximum number of parallel requests that can be sent to each
-- instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@
-- or left unset, Amazon SageMaker checks the optional execution-parameters
-- to determine the settings for your chosen algorithm. If the
-- execution-parameters endpoint is not enabled, the default value is @1@.
-- For more information on execution-parameters, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests>.
-- For built-in algorithms, you don\'t need to set a value for
-- @MaxConcurrentTransforms@.
createTransformJob_maxConcurrentTransforms :: Lens.Lens' CreateTransformJob (Core.Maybe Core.Natural)
createTransformJob_maxConcurrentTransforms = Lens.lens (\CreateTransformJob' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@CreateTransformJob' {} a -> s {maxConcurrentTransforms = a} :: CreateTransformJob)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
createTransformJob_environment :: Lens.Lens' CreateTransformJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
createTransformJob_environment = Lens.lens (\CreateTransformJob' {environment} -> environment) (\s@CreateTransformJob' {} a -> s {environment = a} :: CreateTransformJob) Core.. Lens.mapping Lens._Coerce

-- | The maximum allowed size of the payload, in MB. A /payload/ is the data
-- portion of a record (without metadata). The value in @MaxPayloadInMB@
-- must be greater than, or equal to, the size of a single record. To
-- estimate the size of a record in MB, divide the size of your dataset by
-- the number of records. To ensure that the records fit within the maximum
-- payload size, we recommend using a slightly larger value. The default
-- value is @6@ MB.
--
-- For cases where the payload might be arbitrarily large and is
-- transmitted using HTTP chunked encoding, set the value to @0@. This
-- feature works only in supported algorithms. Currently, Amazon SageMaker
-- built-in algorithms do not support HTTP chunked encoding.
createTransformJob_maxPayloadInMB :: Lens.Lens' CreateTransformJob (Core.Maybe Core.Natural)
createTransformJob_maxPayloadInMB = Lens.lens (\CreateTransformJob' {maxPayloadInMB} -> maxPayloadInMB) (\s@CreateTransformJob' {} a -> s {maxPayloadInMB = a} :: CreateTransformJob)

-- | Specifies the number of records to include in a mini-batch for an HTTP
-- inference request. A /record/ // is a single unit of input data that
-- inference can be made on. For example, a single line in a CSV file is a
-- record.
--
-- To enable the batch strategy, you must set the @SplitType@ property to
-- @Line@, @RecordIO@, or @TFRecord@.
--
-- To use only one record when making an HTTP invocation request to a
-- container, set @BatchStrategy@ to @SingleRecord@ and @SplitType@ to
-- @Line@.
--
-- To fit as many records in a mini-batch as can fit within the
-- @MaxPayloadInMB@ limit, set @BatchStrategy@ to @MultiRecord@ and
-- @SplitType@ to @Line@.
createTransformJob_batchStrategy :: Lens.Lens' CreateTransformJob (Core.Maybe BatchStrategy)
createTransformJob_batchStrategy = Lens.lens (\CreateTransformJob' {batchStrategy} -> batchStrategy) (\s@CreateTransformJob' {} a -> s {batchStrategy = a} :: CreateTransformJob)

-- | Configures the timeout and maximum number of retries for processing a
-- transform job invocation.
createTransformJob_modelClientConfig :: Lens.Lens' CreateTransformJob (Core.Maybe ModelClientConfig)
createTransformJob_modelClientConfig = Lens.lens (\CreateTransformJob' {modelClientConfig} -> modelClientConfig) (\s@CreateTransformJob' {} a -> s {modelClientConfig = a} :: CreateTransformJob)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createTransformJob_tags :: Lens.Lens' CreateTransformJob (Core.Maybe [Tag])
createTransformJob_tags = Lens.lens (\CreateTransformJob' {tags} -> tags) (\s@CreateTransformJob' {} a -> s {tags = a} :: CreateTransformJob) Core.. Lens.mapping Lens._Coerce

-- | The data structure used to specify the data to be used for inference in
-- a batch transform job and to associate the data that is relevant to the
-- prediction results in the output. The input filter provided allows you
-- to exclude input data that is not needed for inference in a batch
-- transform job. The output filter provided allows you to include input
-- data relevant to interpreting the predictions in the output from the
-- job. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records>.
createTransformJob_dataProcessing :: Lens.Lens' CreateTransformJob (Core.Maybe DataProcessing)
createTransformJob_dataProcessing = Lens.lens (\CreateTransformJob' {dataProcessing} -> dataProcessing) (\s@CreateTransformJob' {} a -> s {dataProcessing = a} :: CreateTransformJob)

-- | The name of the transform job. The name must be unique within an AWS
-- Region in an AWS account.
createTransformJob_transformJobName :: Lens.Lens' CreateTransformJob Core.Text
createTransformJob_transformJobName = Lens.lens (\CreateTransformJob' {transformJobName} -> transformJobName) (\s@CreateTransformJob' {} a -> s {transformJobName = a} :: CreateTransformJob)

-- | The name of the model that you want to use for the transform job.
-- @ModelName@ must be the name of an existing Amazon SageMaker model
-- within an AWS Region in an AWS account.
createTransformJob_modelName :: Lens.Lens' CreateTransformJob Core.Text
createTransformJob_modelName = Lens.lens (\CreateTransformJob' {modelName} -> modelName) (\s@CreateTransformJob' {} a -> s {modelName = a} :: CreateTransformJob)

-- | Describes the input source and the way the transform job consumes it.
createTransformJob_transformInput :: Lens.Lens' CreateTransformJob TransformInput
createTransformJob_transformInput = Lens.lens (\CreateTransformJob' {transformInput} -> transformInput) (\s@CreateTransformJob' {} a -> s {transformInput = a} :: CreateTransformJob)

-- | Describes the results of the transform job.
createTransformJob_transformOutput :: Lens.Lens' CreateTransformJob TransformOutput
createTransformJob_transformOutput = Lens.lens (\CreateTransformJob' {transformOutput} -> transformOutput) (\s@CreateTransformJob' {} a -> s {transformOutput = a} :: CreateTransformJob)

-- | Describes the resources, including ML instance types and ML instance
-- count, to use for the transform job.
createTransformJob_transformResources :: Lens.Lens' CreateTransformJob TransformResources
createTransformJob_transformResources = Lens.lens (\CreateTransformJob' {transformResources} -> transformResources) (\s@CreateTransformJob' {} a -> s {transformResources = a} :: CreateTransformJob)

instance Core.AWSRequest CreateTransformJob where
  type
    AWSResponse CreateTransformJob =
      CreateTransformJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTransformJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "TransformJobArn")
      )

instance Core.Hashable CreateTransformJob

instance Core.NFData CreateTransformJob

instance Core.ToHeaders CreateTransformJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateTransformJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTransformJob where
  toJSON CreateTransformJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExperimentConfig" Core..=)
              Core.<$> experimentConfig,
            ("MaxConcurrentTransforms" Core..=)
              Core.<$> maxConcurrentTransforms,
            ("Environment" Core..=) Core.<$> environment,
            ("MaxPayloadInMB" Core..=) Core.<$> maxPayloadInMB,
            ("BatchStrategy" Core..=) Core.<$> batchStrategy,
            ("ModelClientConfig" Core..=)
              Core.<$> modelClientConfig,
            ("Tags" Core..=) Core.<$> tags,
            ("DataProcessing" Core..=) Core.<$> dataProcessing,
            Core.Just
              ("TransformJobName" Core..= transformJobName),
            Core.Just ("ModelName" Core..= modelName),
            Core.Just ("TransformInput" Core..= transformInput),
            Core.Just
              ("TransformOutput" Core..= transformOutput),
            Core.Just
              ("TransformResources" Core..= transformResources)
          ]
      )

instance Core.ToPath CreateTransformJob where
  toPath = Core.const "/"

instance Core.ToQuery CreateTransformJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTransformJobResponse' smart constructor.
data CreateTransformJobResponse = CreateTransformJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTransformJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTransformJobResponse_httpStatus' - The response's http status code.
--
-- 'transformJobArn', 'createTransformJobResponse_transformJobArn' - The Amazon Resource Name (ARN) of the transform job.
newCreateTransformJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'transformJobArn'
  Core.Text ->
  CreateTransformJobResponse
newCreateTransformJobResponse
  pHttpStatus_
  pTransformJobArn_ =
    CreateTransformJobResponse'
      { httpStatus =
          pHttpStatus_,
        transformJobArn = pTransformJobArn_
      }

-- | The response's http status code.
createTransformJobResponse_httpStatus :: Lens.Lens' CreateTransformJobResponse Core.Int
createTransformJobResponse_httpStatus = Lens.lens (\CreateTransformJobResponse' {httpStatus} -> httpStatus) (\s@CreateTransformJobResponse' {} a -> s {httpStatus = a} :: CreateTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the transform job.
createTransformJobResponse_transformJobArn :: Lens.Lens' CreateTransformJobResponse Core.Text
createTransformJobResponse_transformJobArn = Lens.lens (\CreateTransformJobResponse' {transformJobArn} -> transformJobArn) (\s@CreateTransformJobResponse' {} a -> s {transformJobArn = a} :: CreateTransformJobResponse)

instance Core.NFData CreateTransformJobResponse
