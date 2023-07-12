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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeTransformJobResponse_autoMLJobArn,
    describeTransformJobResponse_batchStrategy,
    describeTransformJobResponse_dataCaptureConfig,
    describeTransformJobResponse_dataProcessing,
    describeTransformJobResponse_environment,
    describeTransformJobResponse_experimentConfig,
    describeTransformJobResponse_failureReason,
    describeTransformJobResponse_labelingJobArn,
    describeTransformJobResponse_maxConcurrentTransforms,
    describeTransformJobResponse_maxPayloadInMB,
    describeTransformJobResponse_modelClientConfig,
    describeTransformJobResponse_transformEndTime,
    describeTransformJobResponse_transformOutput,
    describeTransformJobResponse_transformStartTime,
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
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "AutoMLJobArn")
            Prelude.<*> (x Data..?> "BatchStrategy")
            Prelude.<*> (x Data..?> "DataCaptureConfig")
            Prelude.<*> (x Data..?> "DataProcessing")
            Prelude.<*> (x Data..?> "Environment" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ExperimentConfig")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "LabelingJobArn")
            Prelude.<*> (x Data..?> "MaxConcurrentTransforms")
            Prelude.<*> (x Data..?> "MaxPayloadInMB")
            Prelude.<*> (x Data..?> "ModelClientConfig")
            Prelude.<*> (x Data..?> "TransformEndTime")
            Prelude.<*> (x Data..?> "TransformOutput")
            Prelude.<*> (x Data..?> "TransformStartTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TransformJobName")
            Prelude.<*> (x Data..:> "TransformJobArn")
            Prelude.<*> (x Data..:> "TransformJobStatus")
            Prelude.<*> (x Data..:> "ModelName")
            Prelude.<*> (x Data..:> "TransformInput")
            Prelude.<*> (x Data..:> "TransformResources")
            Prelude.<*> (x Data..:> "CreationTime")
      )

instance Prelude.Hashable DescribeTransformJob where
  hashWithSalt _salt DescribeTransformJob' {..} =
    _salt `Prelude.hashWithSalt` transformJobName

instance Prelude.NFData DescribeTransformJob where
  rnf DescribeTransformJob' {..} =
    Prelude.rnf transformJobName

instance Data.ToHeaders DescribeTransformJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeTransformJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTransformJob where
  toJSON DescribeTransformJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransformJobName" Data..= transformJobName)
          ]
      )

instance Data.ToPath DescribeTransformJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTransformJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTransformJobResponse' smart constructor.
data DescribeTransformJobResponse = DescribeTransformJobResponse'
  { -- | The Amazon Resource Name (ARN) of the AutoML transform job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
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
    dataProcessing :: Prelude.Maybe DataProcessing,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | If the transform job failed, @FailureReason@ describes why it failed. A
    -- transform job creates a log file, which includes error messages, and
    -- stores it as an Amazon S3 object. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
    -- labeling job that created the transform or training job.
    labelingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of parallel requests on each instance node that can
    -- be launched in a transform job. The default value is 1.
    maxConcurrentTransforms :: Prelude.Maybe Prelude.Natural,
    -- | The maximum payload size, in MB, used in the transform job.
    maxPayloadInMB :: Prelude.Maybe Prelude.Natural,
    -- | The timeout and maximum number of retries for processing a transform job
    -- invocation.
    modelClientConfig :: Prelude.Maybe ModelClientConfig,
    -- | Indicates when the transform job has been completed, or has stopped or
    -- failed. You are billed for the time interval between this time and the
    -- value of @TransformStartTime@.
    transformEndTime :: Prelude.Maybe Data.POSIX,
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to
    -- save the results from the transform job.
    transformOutput :: Prelude.Maybe TransformOutput,
    -- | Indicates when the transform job starts on ML instances. You are billed
    -- for the time interval between this time and the value of
    -- @TransformEndTime@.
    transformStartTime :: Prelude.Maybe Data.POSIX,
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
    creationTime :: Data.POSIX
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
-- 'autoMLJobArn', 'describeTransformJobResponse_autoMLJobArn' - The Amazon Resource Name (ARN) of the AutoML transform job.
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
-- 'dataProcessing', 'describeTransformJobResponse_dataProcessing' - Undocumented member.
--
-- 'environment', 'describeTransformJobResponse_environment' - The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
--
-- 'experimentConfig', 'describeTransformJobResponse_experimentConfig' - Undocumented member.
--
-- 'failureReason', 'describeTransformJobResponse_failureReason' - If the transform job failed, @FailureReason@ describes why it failed. A
-- transform job creates a log file, which includes error messages, and
-- stores it as an Amazon S3 object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
--
-- 'labelingJobArn', 'describeTransformJobResponse_labelingJobArn' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
--
-- 'maxConcurrentTransforms', 'describeTransformJobResponse_maxConcurrentTransforms' - The maximum number of parallel requests on each instance node that can
-- be launched in a transform job. The default value is 1.
--
-- 'maxPayloadInMB', 'describeTransformJobResponse_maxPayloadInMB' - The maximum payload size, in MB, used in the transform job.
--
-- 'modelClientConfig', 'describeTransformJobResponse_modelClientConfig' - The timeout and maximum number of retries for processing a transform job
-- invocation.
--
-- 'transformEndTime', 'describeTransformJobResponse_transformEndTime' - Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
--
-- 'transformOutput', 'describeTransformJobResponse_transformOutput' - Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
--
-- 'transformStartTime', 'describeTransformJobResponse_transformStartTime' - Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
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
      { autoMLJobArn =
          Prelude.Nothing,
        batchStrategy = Prelude.Nothing,
        dataCaptureConfig = Prelude.Nothing,
        dataProcessing = Prelude.Nothing,
        environment = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        labelingJobArn = Prelude.Nothing,
        maxConcurrentTransforms = Prelude.Nothing,
        maxPayloadInMB = Prelude.Nothing,
        modelClientConfig = Prelude.Nothing,
        transformEndTime = Prelude.Nothing,
        transformOutput = Prelude.Nothing,
        transformStartTime = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        transformJobName = pTransformJobName_,
        transformJobArn = pTransformJobArn_,
        transformJobStatus = pTransformJobStatus_,
        modelName = pModelName_,
        transformInput = pTransformInput_,
        transformResources = pTransformResources_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | The Amazon Resource Name (ARN) of the AutoML transform job.
describeTransformJobResponse_autoMLJobArn :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Text)
describeTransformJobResponse_autoMLJobArn = Lens.lens (\DescribeTransformJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeTransformJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeTransformJobResponse)

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

-- | Undocumented member.
describeTransformJobResponse_dataProcessing :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe DataProcessing)
describeTransformJobResponse_dataProcessing = Lens.lens (\DescribeTransformJobResponse' {dataProcessing} -> dataProcessing) (\s@DescribeTransformJobResponse' {} a -> s {dataProcessing = a} :: DescribeTransformJobResponse)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
describeTransformJobResponse_environment :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeTransformJobResponse_environment = Lens.lens (\DescribeTransformJobResponse' {environment} -> environment) (\s@DescribeTransformJobResponse' {} a -> s {environment = a} :: DescribeTransformJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeTransformJobResponse_experimentConfig :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe ExperimentConfig)
describeTransformJobResponse_experimentConfig = Lens.lens (\DescribeTransformJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeTransformJobResponse' {} a -> s {experimentConfig = a} :: DescribeTransformJobResponse)

-- | If the transform job failed, @FailureReason@ describes why it failed. A
-- transform job creates a log file, which includes error messages, and
-- stores it as an Amazon S3 object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
describeTransformJobResponse_failureReason :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Text)
describeTransformJobResponse_failureReason = Lens.lens (\DescribeTransformJobResponse' {failureReason} -> failureReason) (\s@DescribeTransformJobResponse' {} a -> s {failureReason = a} :: DescribeTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
describeTransformJobResponse_labelingJobArn :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Text)
describeTransformJobResponse_labelingJobArn = Lens.lens (\DescribeTransformJobResponse' {labelingJobArn} -> labelingJobArn) (\s@DescribeTransformJobResponse' {} a -> s {labelingJobArn = a} :: DescribeTransformJobResponse)

-- | The maximum number of parallel requests on each instance node that can
-- be launched in a transform job. The default value is 1.
describeTransformJobResponse_maxConcurrentTransforms :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Natural)
describeTransformJobResponse_maxConcurrentTransforms = Lens.lens (\DescribeTransformJobResponse' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@DescribeTransformJobResponse' {} a -> s {maxConcurrentTransforms = a} :: DescribeTransformJobResponse)

-- | The maximum payload size, in MB, used in the transform job.
describeTransformJobResponse_maxPayloadInMB :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.Natural)
describeTransformJobResponse_maxPayloadInMB = Lens.lens (\DescribeTransformJobResponse' {maxPayloadInMB} -> maxPayloadInMB) (\s@DescribeTransformJobResponse' {} a -> s {maxPayloadInMB = a} :: DescribeTransformJobResponse)

-- | The timeout and maximum number of retries for processing a transform job
-- invocation.
describeTransformJobResponse_modelClientConfig :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe ModelClientConfig)
describeTransformJobResponse_modelClientConfig = Lens.lens (\DescribeTransformJobResponse' {modelClientConfig} -> modelClientConfig) (\s@DescribeTransformJobResponse' {} a -> s {modelClientConfig = a} :: DescribeTransformJobResponse)

-- | Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
describeTransformJobResponse_transformEndTime :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.UTCTime)
describeTransformJobResponse_transformEndTime = Lens.lens (\DescribeTransformJobResponse' {transformEndTime} -> transformEndTime) (\s@DescribeTransformJobResponse' {} a -> s {transformEndTime = a} :: DescribeTransformJobResponse) Prelude.. Lens.mapping Data._Time

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
describeTransformJobResponse_transformOutput :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe TransformOutput)
describeTransformJobResponse_transformOutput = Lens.lens (\DescribeTransformJobResponse' {transformOutput} -> transformOutput) (\s@DescribeTransformJobResponse' {} a -> s {transformOutput = a} :: DescribeTransformJobResponse)

-- | Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
describeTransformJobResponse_transformStartTime :: Lens.Lens' DescribeTransformJobResponse (Prelude.Maybe Prelude.UTCTime)
describeTransformJobResponse_transformStartTime = Lens.lens (\DescribeTransformJobResponse' {transformStartTime} -> transformStartTime) (\s@DescribeTransformJobResponse' {} a -> s {transformStartTime = a} :: DescribeTransformJobResponse) Prelude.. Lens.mapping Data._Time

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
describeTransformJobResponse_creationTime = Lens.lens (\DescribeTransformJobResponse' {creationTime} -> creationTime) (\s@DescribeTransformJobResponse' {} a -> s {creationTime = a} :: DescribeTransformJobResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeTransformJobResponse where
  rnf DescribeTransformJobResponse' {..} =
    Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf batchStrategy
      `Prelude.seq` Prelude.rnf dataCaptureConfig
      `Prelude.seq` Prelude.rnf dataProcessing
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf labelingJobArn
      `Prelude.seq` Prelude.rnf maxConcurrentTransforms
      `Prelude.seq` Prelude.rnf maxPayloadInMB
      `Prelude.seq` Prelude.rnf modelClientConfig
      `Prelude.seq` Prelude.rnf transformEndTime
      `Prelude.seq` Prelude.rnf transformOutput
      `Prelude.seq` Prelude.rnf transformStartTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf transformJobName
      `Prelude.seq` Prelude.rnf transformJobArn
      `Prelude.seq` Prelude.rnf transformJobStatus
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf transformInput
      `Prelude.seq` Prelude.rnf
        transformResources
      `Prelude.seq` Prelude.rnf creationTime
