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
-- Module      : Network.AWS.SageMaker.DescribeTransformJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transform job.
module Network.AWS.SageMaker.DescribeTransformJob
  ( -- * Creating a Request
    DescribeTransformJob (..),
    newDescribeTransformJob,

    -- * Request Lenses
    describeTransformJob_transformJobName,

    -- * Destructuring the Response
    DescribeTransformJobResponse (..),
    newDescribeTransformJobResponse,

    -- * Response Lenses
    describeTransformJobResponse_labelingJobArn,
    describeTransformJobResponse_transformStartTime,
    describeTransformJobResponse_transformOutput,
    describeTransformJobResponse_experimentConfig,
    describeTransformJobResponse_maxConcurrentTransforms,
    describeTransformJobResponse_environment,
    describeTransformJobResponse_maxPayloadInMB,
    describeTransformJobResponse_batchStrategy,
    describeTransformJobResponse_autoMLJobArn,
    describeTransformJobResponse_failureReason,
    describeTransformJobResponse_modelClientConfig,
    describeTransformJobResponse_transformEndTime,
    describeTransformJobResponse_dataProcessing,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeTransformJob' smart constructor.
data DescribeTransformJob = DescribeTransformJob'
  { -- | The name of the transform job that you want to view details of.
    transformJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeTransformJob
newDescribeTransformJob pTransformJobName_ =
  DescribeTransformJob'
    { transformJobName =
        pTransformJobName_
    }

-- | The name of the transform job that you want to view details of.
describeTransformJob_transformJobName :: Lens.Lens' DescribeTransformJob Core.Text
describeTransformJob_transformJobName = Lens.lens (\DescribeTransformJob' {transformJobName} -> transformJobName) (\s@DescribeTransformJob' {} a -> s {transformJobName = a} :: DescribeTransformJob)

instance Core.AWSRequest DescribeTransformJob where
  type
    AWSResponse DescribeTransformJob =
      DescribeTransformJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTransformJobResponse'
            Core.<$> (x Core..?> "LabelingJobArn")
            Core.<*> (x Core..?> "TransformStartTime")
            Core.<*> (x Core..?> "TransformOutput")
            Core.<*> (x Core..?> "ExperimentConfig")
            Core.<*> (x Core..?> "MaxConcurrentTransforms")
            Core.<*> (x Core..?> "Environment" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "MaxPayloadInMB")
            Core.<*> (x Core..?> "BatchStrategy")
            Core.<*> (x Core..?> "AutoMLJobArn")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "ModelClientConfig")
            Core.<*> (x Core..?> "TransformEndTime")
            Core.<*> (x Core..?> "DataProcessing")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "TransformJobName")
            Core.<*> (x Core..:> "TransformJobArn")
            Core.<*> (x Core..:> "TransformJobStatus")
            Core.<*> (x Core..:> "ModelName")
            Core.<*> (x Core..:> "TransformInput")
            Core.<*> (x Core..:> "TransformResources")
            Core.<*> (x Core..:> "CreationTime")
      )

instance Core.Hashable DescribeTransformJob

instance Core.NFData DescribeTransformJob

instance Core.ToHeaders DescribeTransformJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeTransformJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTransformJob where
  toJSON DescribeTransformJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("TransformJobName" Core..= transformJobName)
          ]
      )

instance Core.ToPath DescribeTransformJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTransformJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTransformJobResponse' smart constructor.
data DescribeTransformJobResponse = DescribeTransformJobResponse'
  { -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
    -- labeling job that created the transform or training job.
    labelingJobArn :: Core.Maybe Core.Text,
    -- | Indicates when the transform job starts on ML instances. You are billed
    -- for the time interval between this time and the value of
    -- @TransformEndTime@.
    transformStartTime :: Core.Maybe Core.POSIX,
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to
    -- save the results from the transform job.
    transformOutput :: Core.Maybe TransformOutput,
    experimentConfig :: Core.Maybe ExperimentConfig,
    -- | The maximum number of parallel requests on each instance node that can
    -- be launched in a transform job. The default value is 1.
    maxConcurrentTransforms :: Core.Maybe Core.Natural,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The maximum payload size, in MB, used in the transform job.
    maxPayloadInMB :: Core.Maybe Core.Natural,
    -- | Specifies the number of records to include in a mini-batch for an HTTP
    -- inference request. A /record/ // is a single unit of input data that
    -- inference can be made on. For example, a single line in a CSV file is a
    -- record.
    --
    -- To enable the batch strategy, you must set @SplitType@ to @Line@,
    -- @RecordIO@, or @TFRecord@.
    batchStrategy :: Core.Maybe BatchStrategy,
    -- | The Amazon Resource Name (ARN) of the AutoML transform job.
    autoMLJobArn :: Core.Maybe Core.Text,
    -- | If the transform job failed, @FailureReason@ describes why it failed. A
    -- transform job creates a log file, which includes error messages, and
    -- stores it as an Amazon S3 object. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
    failureReason :: Core.Maybe Core.Text,
    -- | The timeout and maximum number of retries for processing a transform job
    -- invocation.
    modelClientConfig :: Core.Maybe ModelClientConfig,
    -- | Indicates when the transform job has been completed, or has stopped or
    -- failed. You are billed for the time interval between this time and the
    -- value of @TransformStartTime@.
    transformEndTime :: Core.Maybe Core.POSIX,
    dataProcessing :: Core.Maybe DataProcessing,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the transform job.
    transformJobName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Core.Text,
    -- | The status of the transform job. If the transform job failed, the reason
    -- is returned in the @FailureReason@ field.
    transformJobStatus :: TransformJobStatus,
    -- | The name of the model used in the transform job.
    modelName :: Core.Text,
    -- | Describes the dataset to be transformed and the Amazon S3 location where
    -- it is stored.
    transformInput :: TransformInput,
    -- | Describes the resources, including ML instance types and ML instance
    -- count, to use for the transform job.
    transformResources :: TransformResources,
    -- | A timestamp that shows when the transform Job was created.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTransformJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelingJobArn', 'describeTransformJobResponse_labelingJobArn' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
--
-- 'transformStartTime', 'describeTransformJobResponse_transformStartTime' - Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
--
-- 'transformOutput', 'describeTransformJobResponse_transformOutput' - Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
--
-- 'experimentConfig', 'describeTransformJobResponse_experimentConfig' - Undocumented member.
--
-- 'maxConcurrentTransforms', 'describeTransformJobResponse_maxConcurrentTransforms' - The maximum number of parallel requests on each instance node that can
-- be launched in a transform job. The default value is 1.
--
-- 'environment', 'describeTransformJobResponse_environment' - The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
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
-- 'autoMLJobArn', 'describeTransformJobResponse_autoMLJobArn' - The Amazon Resource Name (ARN) of the AutoML transform job.
--
-- 'failureReason', 'describeTransformJobResponse_failureReason' - If the transform job failed, @FailureReason@ describes why it failed. A
-- transform job creates a log file, which includes error messages, and
-- stores it as an Amazon S3 object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
--
-- 'modelClientConfig', 'describeTransformJobResponse_modelClientConfig' - The timeout and maximum number of retries for processing a transform job
-- invocation.
--
-- 'transformEndTime', 'describeTransformJobResponse_transformEndTime' - Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
--
-- 'dataProcessing', 'describeTransformJobResponse_dataProcessing' - Undocumented member.
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
  Core.Int ->
  -- | 'transformJobName'
  Core.Text ->
  -- | 'transformJobArn'
  Core.Text ->
  -- | 'transformJobStatus'
  TransformJobStatus ->
  -- | 'modelName'
  Core.Text ->
  -- | 'transformInput'
  TransformInput ->
  -- | 'transformResources'
  TransformResources ->
  -- | 'creationTime'
  Core.UTCTime ->
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
      { labelingJobArn =
          Core.Nothing,
        transformStartTime = Core.Nothing,
        transformOutput = Core.Nothing,
        experimentConfig = Core.Nothing,
        maxConcurrentTransforms = Core.Nothing,
        environment = Core.Nothing,
        maxPayloadInMB = Core.Nothing,
        batchStrategy = Core.Nothing,
        autoMLJobArn = Core.Nothing,
        failureReason = Core.Nothing,
        modelClientConfig = Core.Nothing,
        transformEndTime = Core.Nothing,
        dataProcessing = Core.Nothing,
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

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
describeTransformJobResponse_labelingJobArn :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Text)
describeTransformJobResponse_labelingJobArn = Lens.lens (\DescribeTransformJobResponse' {labelingJobArn} -> labelingJobArn) (\s@DescribeTransformJobResponse' {} a -> s {labelingJobArn = a} :: DescribeTransformJobResponse)

-- | Indicates when the transform job starts on ML instances. You are billed
-- for the time interval between this time and the value of
-- @TransformEndTime@.
describeTransformJobResponse_transformStartTime :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.UTCTime)
describeTransformJobResponse_transformStartTime = Lens.lens (\DescribeTransformJobResponse' {transformStartTime} -> transformStartTime) (\s@DescribeTransformJobResponse' {} a -> s {transformStartTime = a} :: DescribeTransformJobResponse) Core.. Lens.mapping Core._Time

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
describeTransformJobResponse_transformOutput :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe TransformOutput)
describeTransformJobResponse_transformOutput = Lens.lens (\DescribeTransformJobResponse' {transformOutput} -> transformOutput) (\s@DescribeTransformJobResponse' {} a -> s {transformOutput = a} :: DescribeTransformJobResponse)

-- | Undocumented member.
describeTransformJobResponse_experimentConfig :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe ExperimentConfig)
describeTransformJobResponse_experimentConfig = Lens.lens (\DescribeTransformJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeTransformJobResponse' {} a -> s {experimentConfig = a} :: DescribeTransformJobResponse)

-- | The maximum number of parallel requests on each instance node that can
-- be launched in a transform job. The default value is 1.
describeTransformJobResponse_maxConcurrentTransforms :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Natural)
describeTransformJobResponse_maxConcurrentTransforms = Lens.lens (\DescribeTransformJobResponse' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@DescribeTransformJobResponse' {} a -> s {maxConcurrentTransforms = a} :: DescribeTransformJobResponse)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
describeTransformJobResponse_environment :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeTransformJobResponse_environment = Lens.lens (\DescribeTransformJobResponse' {environment} -> environment) (\s@DescribeTransformJobResponse' {} a -> s {environment = a} :: DescribeTransformJobResponse) Core.. Lens.mapping Lens._Coerce

-- | The maximum payload size, in MB, used in the transform job.
describeTransformJobResponse_maxPayloadInMB :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Natural)
describeTransformJobResponse_maxPayloadInMB = Lens.lens (\DescribeTransformJobResponse' {maxPayloadInMB} -> maxPayloadInMB) (\s@DescribeTransformJobResponse' {} a -> s {maxPayloadInMB = a} :: DescribeTransformJobResponse)

-- | Specifies the number of records to include in a mini-batch for an HTTP
-- inference request. A /record/ // is a single unit of input data that
-- inference can be made on. For example, a single line in a CSV file is a
-- record.
--
-- To enable the batch strategy, you must set @SplitType@ to @Line@,
-- @RecordIO@, or @TFRecord@.
describeTransformJobResponse_batchStrategy :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe BatchStrategy)
describeTransformJobResponse_batchStrategy = Lens.lens (\DescribeTransformJobResponse' {batchStrategy} -> batchStrategy) (\s@DescribeTransformJobResponse' {} a -> s {batchStrategy = a} :: DescribeTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the AutoML transform job.
describeTransformJobResponse_autoMLJobArn :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Text)
describeTransformJobResponse_autoMLJobArn = Lens.lens (\DescribeTransformJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeTransformJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeTransformJobResponse)

-- | If the transform job failed, @FailureReason@ describes why it failed. A
-- transform job creates a log file, which includes error messages, and
-- stores it as an Amazon S3 object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch>.
describeTransformJobResponse_failureReason :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Text)
describeTransformJobResponse_failureReason = Lens.lens (\DescribeTransformJobResponse' {failureReason} -> failureReason) (\s@DescribeTransformJobResponse' {} a -> s {failureReason = a} :: DescribeTransformJobResponse)

-- | The timeout and maximum number of retries for processing a transform job
-- invocation.
describeTransformJobResponse_modelClientConfig :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe ModelClientConfig)
describeTransformJobResponse_modelClientConfig = Lens.lens (\DescribeTransformJobResponse' {modelClientConfig} -> modelClientConfig) (\s@DescribeTransformJobResponse' {} a -> s {modelClientConfig = a} :: DescribeTransformJobResponse)

-- | Indicates when the transform job has been completed, or has stopped or
-- failed. You are billed for the time interval between this time and the
-- value of @TransformStartTime@.
describeTransformJobResponse_transformEndTime :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.UTCTime)
describeTransformJobResponse_transformEndTime = Lens.lens (\DescribeTransformJobResponse' {transformEndTime} -> transformEndTime) (\s@DescribeTransformJobResponse' {} a -> s {transformEndTime = a} :: DescribeTransformJobResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
describeTransformJobResponse_dataProcessing :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe DataProcessing)
describeTransformJobResponse_dataProcessing = Lens.lens (\DescribeTransformJobResponse' {dataProcessing} -> dataProcessing) (\s@DescribeTransformJobResponse' {} a -> s {dataProcessing = a} :: DescribeTransformJobResponse)

-- | The response's http status code.
describeTransformJobResponse_httpStatus :: Lens.Lens' DescribeTransformJobResponse Core.Int
describeTransformJobResponse_httpStatus = Lens.lens (\DescribeTransformJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTransformJobResponse' {} a -> s {httpStatus = a} :: DescribeTransformJobResponse)

-- | The name of the transform job.
describeTransformJobResponse_transformJobName :: Lens.Lens' DescribeTransformJobResponse Core.Text
describeTransformJobResponse_transformJobName = Lens.lens (\DescribeTransformJobResponse' {transformJobName} -> transformJobName) (\s@DescribeTransformJobResponse' {} a -> s {transformJobName = a} :: DescribeTransformJobResponse)

-- | The Amazon Resource Name (ARN) of the transform job.
describeTransformJobResponse_transformJobArn :: Lens.Lens' DescribeTransformJobResponse Core.Text
describeTransformJobResponse_transformJobArn = Lens.lens (\DescribeTransformJobResponse' {transformJobArn} -> transformJobArn) (\s@DescribeTransformJobResponse' {} a -> s {transformJobArn = a} :: DescribeTransformJobResponse)

-- | The status of the transform job. If the transform job failed, the reason
-- is returned in the @FailureReason@ field.
describeTransformJobResponse_transformJobStatus :: Lens.Lens' DescribeTransformJobResponse TransformJobStatus
describeTransformJobResponse_transformJobStatus = Lens.lens (\DescribeTransformJobResponse' {transformJobStatus} -> transformJobStatus) (\s@DescribeTransformJobResponse' {} a -> s {transformJobStatus = a} :: DescribeTransformJobResponse)

-- | The name of the model used in the transform job.
describeTransformJobResponse_modelName :: Lens.Lens' DescribeTransformJobResponse Core.Text
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
describeTransformJobResponse_creationTime :: Lens.Lens' DescribeTransformJobResponse Core.UTCTime
describeTransformJobResponse_creationTime = Lens.lens (\DescribeTransformJobResponse' {creationTime} -> creationTime) (\s@DescribeTransformJobResponse' {} a -> s {creationTime = a} :: DescribeTransformJobResponse) Core.. Core._Time

instance Core.NFData DescribeTransformJobResponse
