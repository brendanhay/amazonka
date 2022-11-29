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
-- Module      : Amazonka.SageMaker.DescribeTransformJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transform job.
module Amazonka.SageMaker.DescribeTransformJob
  ( -- * Creating a Request
    DescribeTransformJob (..),
    newDescribeTransformJob,

    -- * Request Lenses
    describeTransformJob_transformJobName,

    -- * Destructuring the Response
    DescribeTransformJobResponse (..),
    newDescribeTransformJobResponse,

    -- * Response Lenses
    describeTransformJobResponse_maxConcurrentTransforms,
    describeTransformJobResponse_modelClientConfig,
    describeTransformJobResponse_environment,
    describeTransformJobResponse_transformOutput,
    describeTransformJobResponse_experimentConfig,
    describeTransformJobResponse_autoMLJobArn,
    describeTransformJobResponse_transformEndTime,
    describeTransformJobResponse_maxPayloadInMB,
    describeTransformJobResponse_batchStrategy,
    describeTransformJobResponse_dataCaptureConfig,
    describeTransformJobResponse_labelingJobArn,
    describeTransformJobResponse_dataProcessing,
    describeTransformJobResponse_transformStartTime,
    describeTransformJobResponse_failureReason,
    describeTransformJobResponse_httpStatus,
    describeTransformJobResponse_transformJobName,
    describeTransformJobResponse_transformJobArn,
    describeTransformJobResponse_transformJobStatus,
    describeTransformJobResponse_modelName,
    describeTransformJobResponse_transformInput,
    describeTransformJobResponse_transformResources,
    describeTransformJobResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeTransformJob' smart constructor.
data DescribeTransformJob = DescribeTransformJob'
  { -- | The name of the transform job that you want to view details of.
    transformJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransformJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformJobName', 'describeTransformJob_transformJobName' - The name of the transform job that you want to view details of.
newDescribeTransformJob ::
  -- | 'transformJobName'
  Prelude.Text ->
  DescribeTransformJob
newDescribeTransformJob pTransformJobName_ =
  DescribeTransformJob'
    { transformJobName =
        pTransformJobName_
    }

-- | The name of the transform job that you want to view details of.
describeTransformJob_transformJobName :: Lens.Lens' DescribeTransformJob Prelude.Text
describeTransformJob_transformJobName = Lens.lens (\DescribeTransformJob' {transformJobName} -> transformJobName) (\s@DescribeTransformJob' {} a -> s {transformJobName = a} :: DescribeTransformJob)

instance Core.AWSRequest DescribeTransformJob where
  type
    AWSResponse DescribeTransformJob =
      DescribeTransformJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTransformJobResponse'
            Prelude.<$> (x Core..?> "MaxConcurrentTransforms")
            Prelude.<*> (x Core..?> "ModelClientConfig")
            Prelude.<*> (x Core..?> "Environment" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "TransformOutput")
            Prelude.<*> (x Core..?> "ExperimentConfig")
            Prelude.<*> (x Core..?> "AutoMLJobArn")
            Prelude.<*> (x Core..?> "TransformEndTime")
            Prelude.<*> (x Core..?> "MaxPayloadInMB")
            Prelude.<*> (x Core..?> "BatchStrategy")
            Prelude.<*> (x Core..?> "DataCaptureConfig")
            Prelude.<*> (x Core..?> "LabelingJobArn")
            Prelude.<*> (x Core..?> "DataProcessing")
            Prelude.<*> (x Core..?> "TransformStartTime")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "TransformJobName")
            Prelude.<*> (x Core..:> "TransformJobArn")
            Prelude.<*> (x Core..:> "TransformJobStatus")
            Prelude.<*> (x Core..:> "ModelName")
            Prelude.<*> (x Core..:> "TransformInput")
            Prelude.<*> (x Core..:> "TransformResources")
            Prelude.<*> (x Core..:> "CreationTime")
      )

instance Prelude.Hashable DescribeTransformJob where
  hashWithSalt _salt DescribeTransformJob' {..} =
    _salt `Prelude.hashWithSalt` transformJobName

instance Prelude.NFData DescribeTransformJob where
  rnf DescribeTransformJob' {..} =
    Prelude.rnf transformJobName

instance Core.ToHeaders DescribeTransformJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeTransformJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTransformJob where
  toJSON DescribeTransformJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransformJobName" Core..= transformJobName)
          ]
      )

instance Core.ToPath DescribeTransformJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTransformJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTransformJobResponse' smart constructor.
data DescribeTransformJobResponse = DescribeTransformJobResponse'
  { -- | The maximum number of parallel requests on each instance node that can
    -- be launched in a transform job. The default value is 1.
    maxConcurrentTransforms :: Prelude.Maybe Prelude.Natural,
    -- | The timeout and maximum number of retries for processing a transform job
    -- invocation.
    modelClientConfig :: Prelude.Maybe ModelClientConfig,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to
    -- save the results from the transform job.
    transformOutput :: Prelude.Maybe TransformOutput,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The Amazon Resource Name (ARN) of the AutoML transform job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the transform job has been completed, or has stopped or
    -- failed. You are billed for the time interval between this time and the
    -- value of @TransformStartTime@.
    transformEndTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum payload size, in MB, used in the transform job.
    maxPayloadInMB :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the number of records to include in a mini-batch for an HTTP
    -- inference request. A /record/ // is a single unit of input data that
    -- inference can be made on. For example, a single line in a CSV file is a
    -- record.
    --
    -- To enable the batch strategy, you must set @SplitType@ to @Line@,
    -- @RecordIO@, or @TFRecord@.
    batchStrategy :: Prelude.Maybe BatchStrategy,
    -- | Configuration to control how SageMaker captures inference data.
    dataCaptureConfig :: Prelude.Maybe BatchDataCaptureConfig,
    -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
    -- labeling job that created the transform or training job.
    labelingJobArn :: Prelude.Maybe Prelude.Text,
    dataProcessing :: Prelude.Maybe DataProcessing,
    -- | Indicates when the transform job starts on ML instances. You are billed
    -- for the time interval between this time and the value of
    -- @TransformEndTime@.
    transformStartTime :: Prelude.Maybe Core.POSIX,
    -- | If the transform job failed, @FailureReason@ describes why it failed. A
    -- transform job creates a log file, which includes error messages, and
    -- stores it as an Amazon S3 object. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the transform job.
    transformJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Prelude.Text,
    -- | The status of the transform job. If the transform job failed, the reason
    -- is returned in the @FailureReason@ field.
    transformJobStatus :: TransformJobStatus,
    -- | The name of the model used in the transform job.
    modelName :: Prelude.Text,
    -- | Describes the dataset to be transformed and the Amazon S3 location where
    -- it is stored.
    transformInput :: TransformInput,
    -- | Describes the resources, including ML instance types and ML instance
    -- count, to use for the transform job.
    transformResources :: TransformResources,
    -- | A timestamp that shows when the transform Job was created.
    creationTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransformJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrentTransforms', 'describeTransformJobResponse_maxConcurrentTransforms' - The maximum number of parallel requests on each instance node that can
-- be launched in a transform job. The default value is 1.
--
-- 'modelClientConfig', 'describeTransformJobResponse_modelClientConfig' - The timeout and maximum number of retries for processing a transform job
-- invocation.
--
-- 'environment', 'describeTransformJobResponse_environment' - The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
--
-- 'transformOutput', 'describeTransformJobResponse_transformOutput' - Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
--
-- 'experimentConfig', 'describeTransformJobResponse_experimentConfig' - Undocumented member.
--
-- 'autoMLJobArn', 'describeTransformJobResponse_autoMLJobArn' - The Amazon Resource Name (ARN) of the AutoML transform job.
--
-- 'transformEndTime', 'describeTransformJobResponse_transformEndTime' - Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
--
-- 'maxPayloadInMB', 'describeTransformJobResponse_maxPayloadInMB' - The maximum payload size, in MB, used in the transform job.
--
-- 'batchStrategy', 'describeTransformJobResponse_batchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP
-- inference request. A /record/ // is a single unit of input data that
-- inference can be made on. For example, a single line in a CSV file is a
-- record.
--
-- To enable the batch strategy, you must set @SplitType@ to @Line@,
-- @RecordIO@, or @TFRecord@.
--
-- 'dataCaptureConfig', 'describeTransformJobResponse_dataCaptureConfig' - Configuration to control how SageMaker captures inference data.
--
-- 'labelingJobArn', 'describeTransformJobResponse_labelingJobArn' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
--
-- 'dataProcessing', 'describeTransformJobResponse_dataProcessing' - Undocumented member.
--
-- 'transformStartTime', 'describeTransformJobResponse_transformStartTime' - Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
--
-- 'failureReason', 'describeTransformJobResponse_failureReason' - If the transform job failed, @FailureReason@ describes why it failed. A
-- transform job creates a log file, which includes error messages, and
-- stores it as an Amazon S3 object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
--
-- 'httpStatus', 'describeTransformJobResponse_httpStatus' - The response's http status code.
--
-- 'transformJobName', 'describeTransformJobResponse_transformJobName' - The name of the transform job.
--
-- 'transformJobArn', 'describeTransformJobResponse_transformJobArn' - The Amazon Resource Name (ARN) of the transform job.
--
-- 'transformJobStatus', 'describeTransformJobResponse_transformJobStatus' - The status of the transform job. If the transform job failed, the reason
-- is returned in the @FailureReason@ field.
--
-- 'modelName', 'describeTransformJobResponse_modelName' - The name of the model used in the transform job.
--
-- 'transformInput', 'describeTransformJobResponse_transformInput' - Describes the dataset to be transformed and the Amazon S3 location where
-- it is stored.
--
-- 'transformResources', 'describeTransformJobResponse_transformResources' - Describes the resources, including ML instance types and ML instance
-- count, to use for the transform job.
--
-- 'creationTime', 'describeTransformJobResponse_creationTime' - A timestamp that shows when the transform Job was created.
newDescribeTransformJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'transformJobName'
  Prelude.Text ->
  -- | 'transformJobArn'
  Prelude.Text ->
  -- | 'transformJobStatus'
  TransformJobStatus ->
  -- | 'modelName'
  Prelude.Text ->
  -- | 'transformInput'
  TransformInput ->
  -- | 'transformResources'
  TransformResources ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  DescribeTransformJobResponse
newDescribeTransformJobResponse
  pHttpStatus_
  pTransformJobName_
  pTransformJobArn_
  pTransformJobStatus_
  pModelName_
  pTransformInput_
  pTransformResources_
  pCreationTime_ =
    DescribeTransformJobResponse'
      { maxConcurrentTransforms =
          Prelude.Nothing,
        modelClientConfig = Prelude.Nothing,
        environment = Prelude.Nothing,
        transformOutput = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        autoMLJobArn = Prelude.Nothing,
        transformEndTime = Prelude.Nothing,
        maxPayloadInMB = Prelude.Nothing,
        batchStrategy = Prelude.Nothing,
        dataCaptureConfig = Prelude.Nothing,
        labelingJobArn = Prelude.Nothing,
        dataProcessing = Prelude.Nothing,
        transformStartTime = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        transformJobName = pTransformJobName_,
        transformJobArn = pTransformJobArn_,
        transformJobStatus = pTransformJobStatus_,
        modelName = pModelName_,
        transformInput = pTransformInput_,
        transformResources = pTransformResources_,
        creationTime =
          Core._Time Lens.# pCreationTime_
      }

-- | The maximum number of parallel requests on each instance node that can
-- be launched in a transform job. The default value is 1.
describeTransformJobResponse_maxConcurrentTransforms :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Natural)
describeTransformJobResponse_maxConcurrentTransforms = Lens.lens (\DescribeTransformJobResponse' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@DescribeTransformJobResponse' {} a -> s {maxConcurrentTransforms = a} :: DescribeTransformJobResponse)

-- | The timeout and maximum number of retries for processing a transform job
-- invocation.
describeTransformJobResponse_modelClientConfig :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe ModelClientConfig)
describeTransformJobResponse_modelClientConfig = Lens.lens (\DescribeTransformJobResponse' {modelClientConfig} -> modelClientConfig) (\s@DescribeTransformJobResponse' {} a -> s {modelClientConfig = a} :: DescribeTransformJobResponse)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
describeTransformJobResponse_environment :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeTransformJobResponse_environment = Lens.lens (\DescribeTransformJobResponse' {environment} -> environment) (\s@DescribeTransformJobResponse' {} a -> s {environment = a} :: DescribeTransformJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
describeTransformJobResponse_transformOutput :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe TransformOutput)
describeTransformJobResponse_transformOutput = Lens.lens (\DescribeTransformJobResponse' {transformOutput} -> transformOutput) (\s@DescribeTransformJobResponse' {} a -> s {transformOutput = a} :: DescribeTransformJobResponse)

-- | Undocumented member.
describeTransformJobResponse_experimentConfig :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe ExperimentConfig)
describeTransformJobResponse_experimentConfig = Lens.lens (\DescribeTransformJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeTransformJobResponse' {} a -> s {experimentConfig = a} :: DescribeTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the AutoML transform job.
describeTransformJobResponse_autoMLJobArn :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Text)
describeTransformJobResponse_autoMLJobArn = Lens.lens (\DescribeTransformJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeTransformJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeTransformJobResponse)

-- | Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
describeTransformJobResponse_transformEndTime :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.UTCTime)
describeTransformJobResponse_transformEndTime = Lens.lens (\DescribeTransformJobResponse' {transformEndTime} -> transformEndTime) (\s@DescribeTransformJobResponse' {} a -> s {transformEndTime = a} :: DescribeTransformJobResponse) Prelude.. Lens.mapping Core._Time

-- | The maximum payload size, in MB, used in the transform job.
describeTransformJobResponse_maxPayloadInMB :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Natural)
describeTransformJobResponse_maxPayloadInMB = Lens.lens (\DescribeTransformJobResponse' {maxPayloadInMB} -> maxPayloadInMB) (\s@DescribeTransformJobResponse' {} a -> s {maxPayloadInMB = a} :: DescribeTransformJobResponse)

-- | Specifies the number of records to include in a mini-batch for an HTTP
-- inference request. A /record/ // is a single unit of input data that
-- inference can be made on. For example, a single line in a CSV file is a
-- record.
--
-- To enable the batch strategy, you must set @SplitType@ to @Line@,
-- @RecordIO@, or @TFRecord@.
describeTransformJobResponse_batchStrategy :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe BatchStrategy)
describeTransformJobResponse_batchStrategy = Lens.lens (\DescribeTransformJobResponse' {batchStrategy} -> batchStrategy) (\s@DescribeTransformJobResponse' {} a -> s {batchStrategy = a} :: DescribeTransformJobResponse)

-- | Configuration to control how SageMaker captures inference data.
describeTransformJobResponse_dataCaptureConfig :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe BatchDataCaptureConfig)
describeTransformJobResponse_dataCaptureConfig = Lens.lens (\DescribeTransformJobResponse' {dataCaptureConfig} -> dataCaptureConfig) (\s@DescribeTransformJobResponse' {} a -> s {dataCaptureConfig = a} :: DescribeTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
describeTransformJobResponse_labelingJobArn :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Text)
describeTransformJobResponse_labelingJobArn = Lens.lens (\DescribeTransformJobResponse' {labelingJobArn} -> labelingJobArn) (\s@DescribeTransformJobResponse' {} a -> s {labelingJobArn = a} :: DescribeTransformJobResponse)

-- | Undocumented member.
describeTransformJobResponse_dataProcessing :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe DataProcessing)
describeTransformJobResponse_dataProcessing = Lens.lens (\DescribeTransformJobResponse' {dataProcessing} -> dataProcessing) (\s@DescribeTransformJobResponse' {} a -> s {dataProcessing = a} :: DescribeTransformJobResponse)

-- | Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
describeTransformJobResponse_transformStartTime :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.UTCTime)
describeTransformJobResponse_transformStartTime = Lens.lens (\DescribeTransformJobResponse' {transformStartTime} -> transformStartTime) (\s@DescribeTransformJobResponse' {} a -> s {transformStartTime = a} :: DescribeTransformJobResponse) Prelude.. Lens.mapping Core._Time

-- | If the transform job failed, @FailureReason@ describes why it failed. A
-- transform job creates a log file, which includes error messages, and
-- stores it as an Amazon S3 object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
describeTransformJobResponse_failureReason :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Text)
describeTransformJobResponse_failureReason = Lens.lens (\DescribeTransformJobResponse' {failureReason} -> failureReason) (\s@DescribeTransformJobResponse' {} a -> s {failureReason = a} :: DescribeTransformJobResponse)

-- | The response's http status code.
describeTransformJobResponse_httpStatus :: Lens.Lens' DescribeTransformJobResponse Prelude.Int
describeTransformJobResponse_httpStatus = Lens.lens (\DescribeTransformJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTransformJobResponse' {} a -> s {httpStatus = a} :: DescribeTransformJobResponse)

-- | The name of the transform job.
describeTransformJobResponse_transformJobName :: Lens.Lens' DescribeTransformJobResponse Prelude.Text
describeTransformJobResponse_transformJobName = Lens.lens (\DescribeTransformJobResponse' {transformJobName} -> transformJobName) (\s@DescribeTransformJobResponse' {} a -> s {transformJobName = a} :: DescribeTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the transform job.
describeTransformJobResponse_transformJobArn :: Lens.Lens' DescribeTransformJobResponse Prelude.Text
describeTransformJobResponse_transformJobArn = Lens.lens (\DescribeTransformJobResponse' {transformJobArn} -> transformJobArn) (\s@DescribeTransformJobResponse' {} a -> s {transformJobArn = a} :: DescribeTransformJobResponse)

-- | The status of the transform job. If the transform job failed, the reason
-- is returned in the @FailureReason@ field.
describeTransformJobResponse_transformJobStatus :: Lens.Lens' DescribeTransformJobResponse TransformJobStatus
describeTransformJobResponse_transformJobStatus = Lens.lens (\DescribeTransformJobResponse' {transformJobStatus} -> transformJobStatus) (\s@DescribeTransformJobResponse' {} a -> s {transformJobStatus = a} :: DescribeTransformJobResponse)

-- | The name of the model used in the transform job.
describeTransformJobResponse_modelName :: Lens.Lens' DescribeTransformJobResponse Prelude.Text
describeTransformJobResponse_modelName = Lens.lens (\DescribeTransformJobResponse' {modelName} -> modelName) (\s@DescribeTransformJobResponse' {} a -> s {modelName = a} :: DescribeTransformJobResponse)

-- | Describes the dataset to be transformed and the Amazon S3 location where
-- it is stored.
describeTransformJobResponse_transformInput :: Lens.Lens' DescribeTransformJobResponse TransformInput
describeTransformJobResponse_transformInput = Lens.lens (\DescribeTransformJobResponse' {transformInput} -> transformInput) (\s@DescribeTransformJobResponse' {} a -> s {transformInput = a} :: DescribeTransformJobResponse)

-- | Describes the resources, including ML instance types and ML instance
-- count, to use for the transform job.
describeTransformJobResponse_transformResources :: Lens.Lens' DescribeTransformJobResponse TransformResources
describeTransformJobResponse_transformResources = Lens.lens (\DescribeTransformJobResponse' {transformResources} -> transformResources) (\s@DescribeTransformJobResponse' {} a -> s {transformResources = a} :: DescribeTransformJobResponse)

-- | A timestamp that shows when the transform Job was created.
describeTransformJobResponse_creationTime :: Lens.Lens' DescribeTransformJobResponse Prelude.UTCTime
describeTransformJobResponse_creationTime = Lens.lens (\DescribeTransformJobResponse' {creationTime} -> creationTime) (\s@DescribeTransformJobResponse' {} a -> s {creationTime = a} :: DescribeTransformJobResponse) Prelude.. Core._Time

instance Prelude.NFData DescribeTransformJobResponse where
  rnf DescribeTransformJobResponse' {..} =
    Prelude.rnf maxConcurrentTransforms
      `Prelude.seq` Prelude.rnf modelClientConfig
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf transformOutput
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf transformEndTime
      `Prelude.seq` Prelude.rnf maxPayloadInMB
      `Prelude.seq` Prelude.rnf batchStrategy
      `Prelude.seq` Prelude.rnf dataCaptureConfig
      `Prelude.seq` Prelude.rnf labelingJobArn
      `Prelude.seq` Prelude.rnf dataProcessing
      `Prelude.seq` Prelude.rnf transformStartTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf transformJobName
      `Prelude.seq` Prelude.rnf transformJobArn
      `Prelude.seq` Prelude.rnf transformJobStatus
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf transformInput
      `Prelude.seq` Prelude.rnf
        transformResources
      `Prelude.seq` Prelude.rnf creationTime
