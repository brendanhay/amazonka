{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeTransformJob (..)
    , mkDescribeTransformJob
    -- ** Request lenses
    , dtjTransformJobName

    -- * Destructuring the response
    , DescribeTransformJobResponse (..)
    , mkDescribeTransformJobResponse
    -- ** Response lenses
    , dtjrrsTransformJobName
    , dtjrrsTransformJobArn
    , dtjrrsTransformJobStatus
    , dtjrrsModelName
    , dtjrrsTransformInput
    , dtjrrsTransformResources
    , dtjrrsCreationTime
    , dtjrrsAutoMLJobArn
    , dtjrrsBatchStrategy
    , dtjrrsDataProcessing
    , dtjrrsEnvironment
    , dtjrrsExperimentConfig
    , dtjrrsFailureReason
    , dtjrrsLabelingJobArn
    , dtjrrsMaxConcurrentTransforms
    , dtjrrsMaxPayloadInMB
    , dtjrrsModelClientConfig
    , dtjrrsTransformEndTime
    , dtjrrsTransformOutput
    , dtjrrsTransformStartTime
    , dtjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeTransformJob' smart constructor.
newtype DescribeTransformJob = DescribeTransformJob'
  { transformJobName :: Types.TransformJobName
    -- ^ The name of the transform job that you want to view details of.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTransformJob' value with any optional fields omitted.
mkDescribeTransformJob
    :: Types.TransformJobName -- ^ 'transformJobName'
    -> DescribeTransformJob
mkDescribeTransformJob transformJobName
  = DescribeTransformJob'{transformJobName}

-- | The name of the transform job that you want to view details of.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjTransformJobName :: Lens.Lens' DescribeTransformJob Types.TransformJobName
dtjTransformJobName = Lens.field @"transformJobName"
{-# INLINEABLE dtjTransformJobName #-}
{-# DEPRECATED transformJobName "Use generic-lens or generic-optics with 'transformJobName' instead"  #-}

instance Core.ToQuery DescribeTransformJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTransformJob where
        toHeaders DescribeTransformJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeTransformJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTransformJob where
        toJSON DescribeTransformJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TransformJobName" Core..= transformJobName)])

instance Core.AWSRequest DescribeTransformJob where
        type Rs DescribeTransformJob = DescribeTransformJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTransformJobResponse' Core.<$>
                   (x Core..: "TransformJobName") Core.<*> x Core..: "TransformJobArn"
                     Core.<*> x Core..: "TransformJobStatus"
                     Core.<*> x Core..: "ModelName"
                     Core.<*> x Core..: "TransformInput"
                     Core.<*> x Core..: "TransformResources"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..:? "AutoMLJobArn"
                     Core.<*> x Core..:? "BatchStrategy"
                     Core.<*> x Core..:? "DataProcessing"
                     Core.<*> x Core..:? "Environment"
                     Core.<*> x Core..:? "ExperimentConfig"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "LabelingJobArn"
                     Core.<*> x Core..:? "MaxConcurrentTransforms"
                     Core.<*> x Core..:? "MaxPayloadInMB"
                     Core.<*> x Core..:? "ModelClientConfig"
                     Core.<*> x Core..:? "TransformEndTime"
                     Core.<*> x Core..:? "TransformOutput"
                     Core.<*> x Core..:? "TransformStartTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTransformJobResponse' smart constructor.
data DescribeTransformJobResponse = DescribeTransformJobResponse'
  { transformJobName :: Types.TransformJobName
    -- ^ The name of the transform job.
  , transformJobArn :: Types.TransformJobArn
    -- ^ The Amazon Resource Name (ARN) of the transform job.
  , transformJobStatus :: Types.TransformJobStatus
    -- ^ The status of the transform job. If the transform job failed, the reason is returned in the @FailureReason@ field.
  , modelName :: Types.ModelName
    -- ^ The name of the model used in the transform job.
  , transformInput :: Types.TransformInput
    -- ^ Describes the dataset to be transformed and the Amazon S3 location where it is stored.
  , transformResources :: Types.TransformResources
    -- ^ Describes the resources, including ML instance types and ML instance count, to use for the transform job.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the transform Job was created.
  , autoMLJobArn :: Core.Maybe Types.AutoMLJobArn
    -- ^ The Amazon Resource Name (ARN) of the AutoML transform job.
  , batchStrategy :: Core.Maybe Types.BatchStrategy
    -- ^ Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record. 
--
-- To enable the batch strategy, you must set @SplitType@ to @Line@ , @RecordIO@ , or @TFRecord@ .
  , dataProcessing :: Core.Maybe Types.DataProcessing
  , environment :: Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue)
    -- ^ The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
  , experimentConfig :: Core.Maybe Types.ExperimentConfig
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ If the transform job failed, @FailureReason@ describes why it failed. A transform job creates a log file, which includes error messages, and stores it as an Amazon S3 object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch> .
  , labelingJobArn :: Core.Maybe Types.LabelingJobArn
    -- ^ The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
  , maxConcurrentTransforms :: Core.Maybe Core.Natural
    -- ^ The maximum number of parallel requests on each instance node that can be launched in a transform job. The default value is 1.
  , maxPayloadInMB :: Core.Maybe Core.Natural
    -- ^ The maximum payload size, in MB, used in the transform job.
  , modelClientConfig :: Core.Maybe Types.ModelClientConfig
    -- ^ The timeout and maximum number of retries for processing a transform job invocation.
  , transformEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
  , transformOutput :: Core.Maybe Types.TransformOutput
    -- ^ Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
  , transformStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTransformJobResponse' value with any optional fields omitted.
mkDescribeTransformJobResponse
    :: Types.TransformJobName -- ^ 'transformJobName'
    -> Types.TransformJobArn -- ^ 'transformJobArn'
    -> Types.TransformJobStatus -- ^ 'transformJobStatus'
    -> Types.ModelName -- ^ 'modelName'
    -> Types.TransformInput -- ^ 'transformInput'
    -> Types.TransformResources -- ^ 'transformResources'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeTransformJobResponse
mkDescribeTransformJobResponse transformJobName transformJobArn
  transformJobStatus modelName transformInput transformResources
  creationTime responseStatus
  = DescribeTransformJobResponse'{transformJobName, transformJobArn,
                                  transformJobStatus, modelName, transformInput, transformResources,
                                  creationTime, autoMLJobArn = Core.Nothing,
                                  batchStrategy = Core.Nothing, dataProcessing = Core.Nothing,
                                  environment = Core.Nothing, experimentConfig = Core.Nothing,
                                  failureReason = Core.Nothing, labelingJobArn = Core.Nothing,
                                  maxConcurrentTransforms = Core.Nothing,
                                  maxPayloadInMB = Core.Nothing, modelClientConfig = Core.Nothing,
                                  transformEndTime = Core.Nothing, transformOutput = Core.Nothing,
                                  transformStartTime = Core.Nothing, responseStatus}

-- | The name of the transform job.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformJobName :: Lens.Lens' DescribeTransformJobResponse Types.TransformJobName
dtjrrsTransformJobName = Lens.field @"transformJobName"
{-# INLINEABLE dtjrrsTransformJobName #-}
{-# DEPRECATED transformJobName "Use generic-lens or generic-optics with 'transformJobName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformJobArn :: Lens.Lens' DescribeTransformJobResponse Types.TransformJobArn
dtjrrsTransformJobArn = Lens.field @"transformJobArn"
{-# INLINEABLE dtjrrsTransformJobArn #-}
{-# DEPRECATED transformJobArn "Use generic-lens or generic-optics with 'transformJobArn' instead"  #-}

-- | The status of the transform job. If the transform job failed, the reason is returned in the @FailureReason@ field.
--
-- /Note:/ Consider using 'transformJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformJobStatus :: Lens.Lens' DescribeTransformJobResponse Types.TransformJobStatus
dtjrrsTransformJobStatus = Lens.field @"transformJobStatus"
{-# INLINEABLE dtjrrsTransformJobStatus #-}
{-# DEPRECATED transformJobStatus "Use generic-lens or generic-optics with 'transformJobStatus' instead"  #-}

-- | The name of the model used in the transform job.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsModelName :: Lens.Lens' DescribeTransformJobResponse Types.ModelName
dtjrrsModelName = Lens.field @"modelName"
{-# INLINEABLE dtjrrsModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | Describes the dataset to be transformed and the Amazon S3 location where it is stored.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformInput :: Lens.Lens' DescribeTransformJobResponse Types.TransformInput
dtjrrsTransformInput = Lens.field @"transformInput"
{-# INLINEABLE dtjrrsTransformInput #-}
{-# DEPRECATED transformInput "Use generic-lens or generic-optics with 'transformInput' instead"  #-}

-- | Describes the resources, including ML instance types and ML instance count, to use for the transform job.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformResources :: Lens.Lens' DescribeTransformJobResponse Types.TransformResources
dtjrrsTransformResources = Lens.field @"transformResources"
{-# INLINEABLE dtjrrsTransformResources #-}
{-# DEPRECATED transformResources "Use generic-lens or generic-optics with 'transformResources' instead"  #-}

-- | A timestamp that shows when the transform Job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsCreationTime :: Lens.Lens' DescribeTransformJobResponse Core.NominalDiffTime
dtjrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dtjrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AutoML transform job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsAutoMLJobArn :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.AutoMLJobArn)
dtjrrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# INLINEABLE dtjrrsAutoMLJobArn #-}
{-# DEPRECATED autoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead"  #-}

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A /record/ // is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record. 
--
-- To enable the batch strategy, you must set @SplitType@ to @Line@ , @RecordIO@ , or @TFRecord@ .
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsBatchStrategy :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.BatchStrategy)
dtjrrsBatchStrategy = Lens.field @"batchStrategy"
{-# INLINEABLE dtjrrsBatchStrategy #-}
{-# DEPRECATED batchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataProcessing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsDataProcessing :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.DataProcessing)
dtjrrsDataProcessing = Lens.field @"dataProcessing"
{-# INLINEABLE dtjrrsDataProcessing #-}
{-# DEPRECATED dataProcessing "Use generic-lens or generic-optics with 'dataProcessing' instead"  #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsEnvironment :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue))
dtjrrsEnvironment = Lens.field @"environment"
{-# INLINEABLE dtjrrsEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsExperimentConfig :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.ExperimentConfig)
dtjrrsExperimentConfig = Lens.field @"experimentConfig"
{-# INLINEABLE dtjrrsExperimentConfig #-}
{-# DEPRECATED experimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead"  #-}

-- | If the transform job failed, @FailureReason@ describes why it failed. A transform job creates a log file, which includes error messages, and stores it as an Amazon S3 object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/logging-cloudwatch.html Log Amazon SageMaker Events with Amazon CloudWatch> .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsFailureReason :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.FailureReason)
dtjrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE dtjrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- /Note:/ Consider using 'labelingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsLabelingJobArn :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.LabelingJobArn)
dtjrrsLabelingJobArn = Lens.field @"labelingJobArn"
{-# INLINEABLE dtjrrsLabelingJobArn #-}
{-# DEPRECATED labelingJobArn "Use generic-lens or generic-optics with 'labelingJobArn' instead"  #-}

-- | The maximum number of parallel requests on each instance node that can be launched in a transform job. The default value is 1.
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsMaxConcurrentTransforms :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Natural)
dtjrrsMaxConcurrentTransforms = Lens.field @"maxConcurrentTransforms"
{-# INLINEABLE dtjrrsMaxConcurrentTransforms #-}
{-# DEPRECATED maxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead"  #-}

-- | The maximum payload size, in MB, used in the transform job.
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsMaxPayloadInMB :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.Natural)
dtjrrsMaxPayloadInMB = Lens.field @"maxPayloadInMB"
{-# INLINEABLE dtjrrsMaxPayloadInMB #-}
{-# DEPRECATED maxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead"  #-}

-- | The timeout and maximum number of retries for processing a transform job invocation.
--
-- /Note:/ Consider using 'modelClientConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsModelClientConfig :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.ModelClientConfig)
dtjrrsModelClientConfig = Lens.field @"modelClientConfig"
{-# INLINEABLE dtjrrsModelClientConfig #-}
{-# DEPRECATED modelClientConfig "Use generic-lens or generic-optics with 'modelClientConfig' instead"  #-}

-- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
--
-- /Note:/ Consider using 'transformEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformEndTime :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.NominalDiffTime)
dtjrrsTransformEndTime = Lens.field @"transformEndTime"
{-# INLINEABLE dtjrrsTransformEndTime #-}
{-# DEPRECATED transformEndTime "Use generic-lens or generic-optics with 'transformEndTime' instead"  #-}

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformOutput :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Types.TransformOutput)
dtjrrsTransformOutput = Lens.field @"transformOutput"
{-# INLINEABLE dtjrrsTransformOutput #-}
{-# DEPRECATED transformOutput "Use generic-lens or generic-optics with 'transformOutput' instead"  #-}

-- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
--
-- /Note:/ Consider using 'transformStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsTransformStartTime :: Lens.Lens' DescribeTransformJobResponse (Core.Maybe Core.NominalDiffTime)
dtjrrsTransformStartTime = Lens.field @"transformStartTime"
{-# INLINEABLE dtjrrsTransformStartTime #-}
{-# DEPRECATED transformStartTime "Use generic-lens or generic-optics with 'transformStartTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrrsResponseStatus :: Lens.Lens' DescribeTransformJobResponse Core.Int
dtjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
