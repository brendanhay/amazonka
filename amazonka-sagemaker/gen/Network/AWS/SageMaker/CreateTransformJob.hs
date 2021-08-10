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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateTransformJob' smart constructor.
data CreateTransformJob = CreateTransformJob'
  { experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The maximum number of parallel requests that can be sent to each
    -- instance in a transform job. If @MaxConcurrentTransforms@ is set to @0@
    -- or left unset, Amazon SageMaker checks the optional execution-parameters
    -- to determine the settings for your chosen algorithm. If the
    -- execution-parameters endpoint is not enabled, the default value is @1@.
    -- For more information on execution-parameters, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-batch-code.html#your-algorithms-batch-code-how-containe-serves-requests How Containers Serve Requests>.
    -- For built-in algorithms, you don\'t need to set a value for
    -- @MaxConcurrentTransforms@.
    maxConcurrentTransforms :: Prelude.Maybe Prelude.Natural,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    maxPayloadInMB :: Prelude.Maybe Prelude.Natural,
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
    batchStrategy :: Prelude.Maybe BatchStrategy,
    -- | Configures the timeout and maximum number of retries for processing a
    -- transform job invocation.
    modelClientConfig :: Prelude.Maybe ModelClientConfig,
    -- | (Optional) An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The data structure used to specify the data to be used for inference in
    -- a batch transform job and to associate the data that is relevant to the
    -- prediction results in the output. The input filter provided allows you
    -- to exclude input data that is not needed for inference in a batch
    -- transform job. The output filter provided allows you to include input
    -- data relevant to interpreting the predictions in the output from the
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records>.
    dataProcessing :: Prelude.Maybe DataProcessing,
    -- | The name of the transform job. The name must be unique within an AWS
    -- Region in an AWS account.
    transformJobName :: Prelude.Text,
    -- | The name of the model that you want to use for the transform job.
    -- @ModelName@ must be the name of an existing Amazon SageMaker model
    -- within an AWS Region in an AWS account.
    modelName :: Prelude.Text,
    -- | Describes the input source and the way the transform job consumes it.
    transformInput :: TransformInput,
    -- | Describes the results of the transform job.
    transformOutput :: TransformOutput,
    -- | Describes the resources, including ML instance types and ML instance
    -- count, to use for the transform job.
    transformResources :: TransformResources
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'modelName'
  Prelude.Text ->
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
          Prelude.Nothing,
        maxConcurrentTransforms = Prelude.Nothing,
        environment = Prelude.Nothing,
        maxPayloadInMB = Prelude.Nothing,
        batchStrategy = Prelude.Nothing,
        modelClientConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        dataProcessing = Prelude.Nothing,
        transformJobName = pTransformJobName_,
        modelName = pModelName_,
        transformInput = pTransformInput_,
        transformOutput = pTransformOutput_,
        transformResources = pTransformResources_
      }

-- | Undocumented member.
createTransformJob_experimentConfig :: Lens.Lens' CreateTransformJob (Prelude.Maybe ExperimentConfig)
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
createTransformJob_maxConcurrentTransforms :: Lens.Lens' CreateTransformJob (Prelude.Maybe Prelude.Natural)
createTransformJob_maxConcurrentTransforms = Lens.lens (\CreateTransformJob' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@CreateTransformJob' {} a -> s {maxConcurrentTransforms = a} :: CreateTransformJob)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
createTransformJob_environment :: Lens.Lens' CreateTransformJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTransformJob_environment = Lens.lens (\CreateTransformJob' {environment} -> environment) (\s@CreateTransformJob' {} a -> s {environment = a} :: CreateTransformJob) Prelude.. Lens.mapping Lens._Coerce

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
createTransformJob_maxPayloadInMB :: Lens.Lens' CreateTransformJob (Prelude.Maybe Prelude.Natural)
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
createTransformJob_batchStrategy :: Lens.Lens' CreateTransformJob (Prelude.Maybe BatchStrategy)
createTransformJob_batchStrategy = Lens.lens (\CreateTransformJob' {batchStrategy} -> batchStrategy) (\s@CreateTransformJob' {} a -> s {batchStrategy = a} :: CreateTransformJob)

-- | Configures the timeout and maximum number of retries for processing a
-- transform job invocation.
createTransformJob_modelClientConfig :: Lens.Lens' CreateTransformJob (Prelude.Maybe ModelClientConfig)
createTransformJob_modelClientConfig = Lens.lens (\CreateTransformJob' {modelClientConfig} -> modelClientConfig) (\s@CreateTransformJob' {} a -> s {modelClientConfig = a} :: CreateTransformJob)

-- | (Optional) An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createTransformJob_tags :: Lens.Lens' CreateTransformJob (Prelude.Maybe [Tag])
createTransformJob_tags = Lens.lens (\CreateTransformJob' {tags} -> tags) (\s@CreateTransformJob' {} a -> s {tags = a} :: CreateTransformJob) Prelude.. Lens.mapping Lens._Coerce

-- | The data structure used to specify the data to be used for inference in
-- a batch transform job and to associate the data that is relevant to the
-- prediction results in the output. The input filter provided allows you
-- to exclude input data that is not needed for inference in a batch
-- transform job. The output filter provided allows you to include input
-- data relevant to interpreting the predictions in the output from the
-- job. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records>.
createTransformJob_dataProcessing :: Lens.Lens' CreateTransformJob (Prelude.Maybe DataProcessing)
createTransformJob_dataProcessing = Lens.lens (\CreateTransformJob' {dataProcessing} -> dataProcessing) (\s@CreateTransformJob' {} a -> s {dataProcessing = a} :: CreateTransformJob)

-- | The name of the transform job. The name must be unique within an AWS
-- Region in an AWS account.
createTransformJob_transformJobName :: Lens.Lens' CreateTransformJob Prelude.Text
createTransformJob_transformJobName = Lens.lens (\CreateTransformJob' {transformJobName} -> transformJobName) (\s@CreateTransformJob' {} a -> s {transformJobName = a} :: CreateTransformJob)

-- | The name of the model that you want to use for the transform job.
-- @ModelName@ must be the name of an existing Amazon SageMaker model
-- within an AWS Region in an AWS account.
createTransformJob_modelName :: Lens.Lens' CreateTransformJob Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "TransformJobArn")
      )

instance Prelude.Hashable CreateTransformJob

instance Prelude.NFData CreateTransformJob

instance Core.ToHeaders CreateTransformJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateTransformJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTransformJob where
  toJSON CreateTransformJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExperimentConfig" Core..=)
              Prelude.<$> experimentConfig,
            ("MaxConcurrentTransforms" Core..=)
              Prelude.<$> maxConcurrentTransforms,
            ("Environment" Core..=) Prelude.<$> environment,
            ("MaxPayloadInMB" Core..=)
              Prelude.<$> maxPayloadInMB,
            ("BatchStrategy" Core..=) Prelude.<$> batchStrategy,
            ("ModelClientConfig" Core..=)
              Prelude.<$> modelClientConfig,
            ("Tags" Core..=) Prelude.<$> tags,
            ("DataProcessing" Core..=)
              Prelude.<$> dataProcessing,
            Prelude.Just
              ("TransformJobName" Core..= transformJobName),
            Prelude.Just ("ModelName" Core..= modelName),
            Prelude.Just
              ("TransformInput" Core..= transformInput),
            Prelude.Just
              ("TransformOutput" Core..= transformOutput),
            Prelude.Just
              ("TransformResources" Core..= transformResources)
          ]
      )

instance Core.ToPath CreateTransformJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTransformJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTransformJobResponse' smart constructor.
data CreateTransformJobResponse = CreateTransformJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'transformJobArn'
  Prelude.Text ->
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
createTransformJobResponse_httpStatus :: Lens.Lens' CreateTransformJobResponse Prelude.Int
createTransformJobResponse_httpStatus = Lens.lens (\CreateTransformJobResponse' {httpStatus} -> httpStatus) (\s@CreateTransformJobResponse' {} a -> s {httpStatus = a} :: CreateTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the transform job.
createTransformJobResponse_transformJobArn :: Lens.Lens' CreateTransformJobResponse Prelude.Text
createTransformJobResponse_transformJobArn = Lens.lens (\CreateTransformJobResponse' {transformJobArn} -> transformJobArn) (\s@CreateTransformJobResponse' {} a -> s {transformJobArn = a} :: CreateTransformJobResponse)

instance Prelude.NFData CreateTransformJobResponse
