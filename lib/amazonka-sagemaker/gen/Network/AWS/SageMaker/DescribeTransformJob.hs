{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeTransformJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transform job.
module Network.AWS.SageMaker.DescribeTransformJob
  ( -- * Creating a request
    DescribeTransformJob (..),
    mkDescribeTransformJob,

    -- ** Request lenses
    dtjTransformJobName,

    -- * Destructuring the response
    DescribeTransformJobResponse (..),
    mkDescribeTransformJobResponse,

    -- ** Response lenses
    dtjrrsTransformJobName,
    dtjrrsTransformJobArn,
    dtjrrsTransformJobStatus,
    dtjrrsModelName,
    dtjrrsTransformInput,
    dtjrrsTransformResources,
    dtjrrsCreationTime,
    dtjrrsAutoMLJobArn,
    dtjrrsBatchStrategy,
    dtjrrsDataProcessing,
    dtjrrsEnvironment,
    dtjrrsExperimentConfig,
    dtjrrsFailureReason,
    dtjrrsLabelingJobArn,
    dtjrrsMaxConcurrentTransforms,
    dtjrrsMaxPayloadInMB,
    dtjrrsModelClientConfig,
    dtjrrsTransformEndTime,
    dtjrrsTransformOutput,
    dtjrrsTransformStartTime,
    dtjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeTransformJob' smart constructor.
newtype DescribeTransformJob = DescribeTransformJob'
  { -- | The name of the transform job that you want to view details of.
    transformJobName :: Types.TransformJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTransformJob' value with any optional fields omitted.
mkDescribeTransformJob ::
  -- | 'transformJobName'
  Types.TransformJobName ->
  DescribeTransformJob
mkDescribeTransformJob transformJobName =
  DescribeTransformJob' {transformJobName}

-- | The name of the transform job that you want to view details of.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjTransformJobName :: Lens.Lens' DescribeTransformJob Types.TransformJobName
dtjTransformJobName = Lens.field @"transformJobName"
{-# DEPRECATED dtjTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

instance Core.FromJSON DescribeTransformJob where
  toJSON DescribeTransformJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TransformJobName" Core..= transformJobName)]
      )

instance Core.AWSRequest DescribeTransformJob where
  type Rs DescribeTransformJob = DescribeTransformJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeTransformJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTransformJobResponse'
            Core.<$> (x Core..: "TransformJobName")
            Core.<*> (x Core..: "TransformJobArn")
            Core.<*> (x Core..: "TransformJobStatus")
            Core.<*> (x Core..: "ModelName")
            Core.<*> (x Core..: "TransformInput")
            Core.<*> (x Core..: "TransformResources")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..:? "AutoMLJobArn")
            Core.<*> (x Core..:? "BatchStrategy")
            Core.<*> (x Core..:? "DataProcessing")
            Core.<*> (x Core..:? "Environment")
            Core.<*> (x Core..:? "ExperimentConfig")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "LabelingJobArn")
            Core.<*> (x Core..:? "MaxConcurrentTransforms")
            Core.<*> (x Core..:? "MaxPayloadInMB")
            Core.<*> (x Core..:? "ModelClientConfig")
            Core.<*> (x Core..:? "TransformEndTime")
            Core.<*> (x Core..:? "TransformOutput")
            Core.<*> (x Core..:? "TransformStartTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTransformJobResponse' smart constructor.
data DescribeTransformJobResponse = DescribeTransformJobResponse'
  { -- | The name of the transform job.
    transformJobName :: Types.TransformJobName,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Types.TransformJobArn,
    -- | The status of the transform job. If the transform job failed, the reason is returned in the @FailureReason@ field.
    transformJobStatus :: Types.TransformJobStatus,
    -- | The name of the model used in the transform job.
    modelName :: Types.ModelName,
    -- | Describes the dataset to be transformed and the Amazon S3 location where it is stored.
    transformInput :: Types.TransformInput,
    -- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
    transformResources :: Types.TransformResources,
    -- | A timestamp that shows when the transform Job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the AutoML transform job.
    autoMLJobArn :: Core.Maybe Types.AutoMLJobArn,
    -- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
    --
    -- To enable the batch strategy, you must set @SplitType@ to @Line@ , @RecordIO@ , or @TFRecord@ .
    batchStrategy :: Core.Maybe Types.BatchStrategy,
    dataProcessing :: Core.Maybe Types.DataProcessing,
    -- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
    environment :: Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue),
    experimentConfig :: Core.Maybe Types.ExperimentConfig,
    -- | If the transform job failed, @FailureReason@ describes why it failed. A transform job creates a log file, which includes error messages, and stores it as an Amazon S3 object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch> .
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
    labelingJobArn :: Core.Maybe Types.LabelingJobArn,
    -- | The maximum number of parallel requests on each instance node that can be launched in a transform job. The default value is 1.
    maxConcurrentTransforms :: Core.Maybe Core.Natural,
    -- | The maximum payload size, in MB, used in the transform job.
    maxPayloadInMB :: Core.Maybe Core.Natural,
    -- | The timeout and maximum number of retries for processing a transform job invocation.
    modelClientConfig :: Core.Maybe Types.ModelClientConfig,
    -- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
    transformEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
    transformOutput :: Core.Maybe Types.TransformOutput,
    -- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
    transformStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTransformJobResponse' value with any optional fields omitted.
mkDescribeTransformJobResponse ::
  -- | 'transformJobName'
  Types.TransformJobName ->
  -- | 'transformJobArn'
  Types.TransformJobArn ->
  -- | 'transformJobStatus'
  Types.TransformJobStatus ->
  -- | 'modelName'
  Types.ModelName ->
  -- | 'transformInput'
  Types.TransformInput ->
  -- | 'transformResources'
  Types.TransformResources ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeTransformJobResponse
mkDescribeTransformJobResponse
  transformJobName
  transformJobArn
  transformJobStatus
  modelName
  transformInput
  transformResources
  creationTime
  responseStatus =
    DescribeTransformJobResponse'
      { transformJobName,
        transformJobArn,
        transformJobStatus,
        modelName,
        transformInput,
        transformResources,
        creationTime,
        autoMLJobArn = Core.Nothing,
        batchStrategy = Core.Nothing,
        dataProcessing = Core.Nothing,
        environment = Core.Nothing,
        experimentConfig = Core.Nothing,
        failureReason = Core.Nothing,
        labelingJobArn = Core.Nothing,
        maxConcurrentTransforms = Core.Nothing,
        maxPayloadInMB = Core.Nothing,
        modelClientConfig = Core.Nothing,
        transformEndTime = Core.Nothing,
        transformOutput = Core.Nothing,
        transformStartTime = Core.Nothing,
        responseStatus
      }

-- | The name of the transform job.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformJobName :: Lens.Lens' DescribeTransformJobResponse Types.TransformJobName
dtjrrsTransformJobName = Lens.field @"transformJobName"
{-# DEPRECATED dtjrrsTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformJobArn :: Lens.Lens' DescribeTransformJobResponse Types.TransformJobArn
dtjrrsTransformJobArn = Lens.field @"transformJobArn"
{-# DEPRECATED dtjrrsTransformJobArn "Use generic-lens or generic-optics with 'transformJobArn' instead." #-}

-- | The status of the transform job. If the transform job failed, the reason is returned in the @FailureReason@ field.
--
-- /Note:/ Consider using 'transformJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformJobStatus :: Lens.Lens' DescribeTransformJobResponse Types.TransformJobStatus
dtjrrsTransformJobStatus = Lens.field @"transformJobStatus"
{-# DEPRECATED dtjrrsTransformJobStatus "Use generic-lens or generic-optics with 'transformJobStatus' instead." #-}

-- | The name of the model used in the transform job.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsModelName :: Lens.Lens' DescribeTransformJobResponse Types.ModelName
dtjrrsModelName = Lens.field @"modelName"
{-# DEPRECATED dtjrrsModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Describes the dataset to be transformed and the Amazon S3 location where it is stored.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformInput :: Lens.Lens' DescribeTransformJobResponse Types.TransformInput
dtjrrsTransformInput = Lens.field @"transformInput"
{-# DEPRECATED dtjrrsTransformInput "Use generic-lens or generic-optics with 'transformInput' instead." #-}

-- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformResources :: Lens.Lens' DescribeTransformJobResponse Types.TransformResources
dtjrrsTransformResources = Lens.field @"transformResources"
{-# DEPRECATED dtjrrsTransformResources "Use generic-lens or generic-optics with 'transformResources' instead." #-}

-- | A timestamp that shows when the transform Job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsCreationTime :: Lens.Lens' DescribeTransformJobResponse Core.NominalDiffTime
dtjrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dtjrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the AutoML transform job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsAutoMLJobArn :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.AutoMLJobArn)
dtjrrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# DEPRECATED dtjrrsAutoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead." #-}

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
--
-- To enable the batch strategy, you must set @SplitType@ to @Line@ , @RecordIO@ , or @TFRecord@ .
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsBatchStrategy :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.BatchStrategy)
dtjrrsBatchStrategy = Lens.field @"batchStrategy"
{-# DEPRECATED dtjrrsBatchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataProcessing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsDataProcessing :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.DataProcessing)
dtjrrsDataProcessing = Lens.field @"dataProcessing"
{-# DEPRECATED dtjrrsDataProcessing "Use generic-lens or generic-optics with 'dataProcessing' instead." #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsEnvironment :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue))
dtjrrsEnvironment = Lens.field @"environment"
{-# DEPRECATED dtjrrsEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsExperimentConfig :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.ExperimentConfig)
dtjrrsExperimentConfig = Lens.field @"experimentConfig"
{-# DEPRECATED dtjrrsExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | If the transform job failed, @FailureReason@ describes why it failed. A transform job creates a log file, which includes error messages, and stores it as an Amazon S3 object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch> .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsFailureReason :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.FailureReason)
dtjrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dtjrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- /Note:/ Consider using 'labelingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsLabelingJobArn :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.LabelingJobArn)
dtjrrsLabelingJobArn = Lens.field @"labelingJobArn"
{-# DEPRECATED dtjrrsLabelingJobArn "Use generic-lens or generic-optics with 'labelingJobArn' instead." #-}

-- | The maximum number of parallel requests on each instance node that can be launched in a transform job. The default value is 1.
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsMaxConcurrentTransforms :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Natural)
dtjrrsMaxConcurrentTransforms = Lens.field @"maxConcurrentTransforms"
{-# DEPRECATED dtjrrsMaxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead." #-}

-- | The maximum payload size, in MB, used in the transform job.
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsMaxPayloadInMB :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Natural)
dtjrrsMaxPayloadInMB = Lens.field @"maxPayloadInMB"
{-# DEPRECATED dtjrrsMaxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead." #-}

-- | The timeout and maximum number of retries for processing a transform job invocation.
--
-- /Note:/ Consider using 'modelClientConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsModelClientConfig :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.ModelClientConfig)
dtjrrsModelClientConfig = Lens.field @"modelClientConfig"
{-# DEPRECATED dtjrrsModelClientConfig "Use generic-lens or generic-optics with 'modelClientConfig' instead." #-}

-- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
--
-- /Note:/ Consider using 'transformEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformEndTime :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.NominalDiffTime)
dtjrrsTransformEndTime = Lens.field @"transformEndTime"
{-# DEPRECATED dtjrrsTransformEndTime "Use generic-lens or generic-optics with 'transformEndTime' instead." #-}

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformOutput :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.TransformOutput)
dtjrrsTransformOutput = Lens.field @"transformOutput"
{-# DEPRECATED dtjrrsTransformOutput "Use generic-lens or generic-optics with 'transformOutput' instead." #-}

-- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
--
-- /Note:/ Consider using 'transformStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformStartTime :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.NominalDiffTime)
dtjrrsTransformStartTime = Lens.field @"transformStartTime"
{-# DEPRECATED dtjrrsTransformStartTime "Use generic-lens or generic-optics with 'transformStartTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsResponseStatus :: Lens.Lens' DescribeTransformJobResponse Core.Int
dtjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
