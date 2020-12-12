{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJob
  ( TransformJob (..),

    -- * Smart constructor
    mkTransformJob,

    -- * Lenses
    traCreationTime,
    traLabelingJobARN,
    traTransformJobName,
    traFailureReason,
    traModelClientConfig,
    traBatchStrategy,
    traMaxPayloadInMB,
    traEnvironment,
    traTransformResources,
    traModelName,
    traExperimentConfig,
    traTransformEndTime,
    traTransformStartTime,
    traAutoMLJobARN,
    traTransformJobStatus,
    traTransformInput,
    traMaxConcurrentTransforms,
    traTransformOutput,
    traDataProcessing,
    traTransformJobARN,
    traTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.BatchStrategy
import Network.AWS.SageMaker.Types.DataProcessing
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.ModelClientConfig
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformJobStatus
import Network.AWS.SageMaker.Types.TransformOutput
import Network.AWS.SageMaker.Types.TransformResources

-- | A batch transform job. For information about SageMaker batch transform, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform.html Use Batch Transform> .
--
-- /See:/ 'mkTransformJob' smart constructor.
data TransformJob = TransformJob'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    labelingJobARN :: Lude.Maybe Lude.Text,
    transformJobName :: Lude.Maybe Lude.Text,
    failureReason :: Lude.Maybe Lude.Text,
    modelClientConfig :: Lude.Maybe ModelClientConfig,
    batchStrategy :: Lude.Maybe BatchStrategy,
    maxPayloadInMB :: Lude.Maybe Lude.Natural,
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    transformResources :: Lude.Maybe TransformResources,
    modelName :: Lude.Maybe Lude.Text,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    transformEndTime :: Lude.Maybe Lude.Timestamp,
    transformStartTime :: Lude.Maybe Lude.Timestamp,
    autoMLJobARN :: Lude.Maybe Lude.Text,
    transformJobStatus :: Lude.Maybe TransformJobStatus,
    transformInput :: Lude.Maybe TransformInput,
    maxConcurrentTransforms :: Lude.Maybe Lude.Natural,
    transformOutput :: Lude.Maybe TransformOutput,
    dataProcessing :: Lude.Maybe DataProcessing,
    transformJobARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformJob' with the minimum fields required to make a request.
--
-- * 'autoMLJobARN' - The Amazon Resource Name (ARN) of the AutoML job that created the transform job.
-- * 'batchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP inference request. A record is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
-- * 'creationTime' - A timestamp that shows when the transform Job was created.
-- * 'dataProcessing' - Undocumented field.
-- * 'environment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
-- * 'experimentConfig' - Undocumented field.
-- * 'failureReason' - If the transform job failed, the reason it failed.
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) of the labeling job that created the transform job.
-- * 'maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or left unset, SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is 1. For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
-- * 'maxPayloadInMB' - The maximum allowed size of the payload, in MB. A payload is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is 6 MB. For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to 0. This feature works only in supported algorithms. Currently, SageMaker built-in algorithms do not support HTTP chunked encoding.
-- * 'modelClientConfig' - Undocumented field.
-- * 'modelName' - The name of the model associated with the transform job.
-- * 'tags' - A list of tags associated with the transform job.
-- * 'transformEndTime' - Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
-- * 'transformInput' - Undocumented field.
-- * 'transformJobARN' - The Amazon Resource Name (ARN) of the transform job.
-- * 'transformJobName' - The name of the transform job.
-- * 'transformJobStatus' - The status of the transform job.
--
-- Transform job statuses are:
--
--     * @InProgress@ - The job is in progress.
--
--
--     * @Completed@ - The job has completed.
--
--
--     * @Failed@ - The transform job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTransformJob@ call.
--
--
--     * @Stopping@ - The transform job is stopping.
--
--
--     * @Stopped@ - The transform job has stopped.
--
--
-- * 'transformOutput' - Undocumented field.
-- * 'transformResources' - Undocumented field.
-- * 'transformStartTime' - Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
mkTransformJob ::
  TransformJob
mkTransformJob =
  TransformJob'
    { creationTime = Lude.Nothing,
      labelingJobARN = Lude.Nothing,
      transformJobName = Lude.Nothing,
      failureReason = Lude.Nothing,
      modelClientConfig = Lude.Nothing,
      batchStrategy = Lude.Nothing,
      maxPayloadInMB = Lude.Nothing,
      environment = Lude.Nothing,
      transformResources = Lude.Nothing,
      modelName = Lude.Nothing,
      experimentConfig = Lude.Nothing,
      transformEndTime = Lude.Nothing,
      transformStartTime = Lude.Nothing,
      autoMLJobARN = Lude.Nothing,
      transformJobStatus = Lude.Nothing,
      transformInput = Lude.Nothing,
      maxConcurrentTransforms = Lude.Nothing,
      transformOutput = Lude.Nothing,
      dataProcessing = Lude.Nothing,
      transformJobARN = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A timestamp that shows when the transform Job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traCreationTime :: Lens.Lens' TransformJob (Lude.Maybe Lude.Timestamp)
traCreationTime = Lens.lens (creationTime :: TransformJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TransformJob)
{-# DEPRECATED traCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the labeling job that created the transform job.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traLabelingJobARN :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
traLabelingJobARN = Lens.lens (labelingJobARN :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {labelingJobARN = a} :: TransformJob)
{-# DEPRECATED traLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}

-- | The name of the transform job.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformJobName :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
traTransformJobName = Lens.lens (transformJobName :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {transformJobName = a} :: TransformJob)
{-# DEPRECATED traTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

-- | If the transform job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traFailureReason :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
traFailureReason = Lens.lens (failureReason :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: TransformJob)
{-# DEPRECATED traFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'modelClientConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traModelClientConfig :: Lens.Lens' TransformJob (Lude.Maybe ModelClientConfig)
traModelClientConfig = Lens.lens (modelClientConfig :: TransformJob -> Lude.Maybe ModelClientConfig) (\s a -> s {modelClientConfig = a} :: TransformJob)
{-# DEPRECATED traModelClientConfig "Use generic-lens or generic-optics with 'modelClientConfig' instead." #-}

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A record is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traBatchStrategy :: Lens.Lens' TransformJob (Lude.Maybe BatchStrategy)
traBatchStrategy = Lens.lens (batchStrategy :: TransformJob -> Lude.Maybe BatchStrategy) (\s a -> s {batchStrategy = a} :: TransformJob)
{-# DEPRECATED traBatchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead." #-}

-- | The maximum allowed size of the payload, in MB. A payload is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is 6 MB. For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to 0. This feature works only in supported algorithms. Currently, SageMaker built-in algorithms do not support HTTP chunked encoding.
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traMaxPayloadInMB :: Lens.Lens' TransformJob (Lude.Maybe Lude.Natural)
traMaxPayloadInMB = Lens.lens (maxPayloadInMB :: TransformJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxPayloadInMB = a} :: TransformJob)
{-# DEPRECATED traMaxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead." #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traEnvironment :: Lens.Lens' TransformJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
traEnvironment = Lens.lens (environment :: TransformJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: TransformJob)
{-# DEPRECATED traEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformResources :: Lens.Lens' TransformJob (Lude.Maybe TransformResources)
traTransformResources = Lens.lens (transformResources :: TransformJob -> Lude.Maybe TransformResources) (\s a -> s {transformResources = a} :: TransformJob)
{-# DEPRECATED traTransformResources "Use generic-lens or generic-optics with 'transformResources' instead." #-}

-- | The name of the model associated with the transform job.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traModelName :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
traModelName = Lens.lens (modelName :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {modelName = a} :: TransformJob)
{-# DEPRECATED traModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traExperimentConfig :: Lens.Lens' TransformJob (Lude.Maybe ExperimentConfig)
traExperimentConfig = Lens.lens (experimentConfig :: TransformJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: TransformJob)
{-# DEPRECATED traExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
--
-- /Note:/ Consider using 'transformEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformEndTime :: Lens.Lens' TransformJob (Lude.Maybe Lude.Timestamp)
traTransformEndTime = Lens.lens (transformEndTime :: TransformJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {transformEndTime = a} :: TransformJob)
{-# DEPRECATED traTransformEndTime "Use generic-lens or generic-optics with 'transformEndTime' instead." #-}

-- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
--
-- /Note:/ Consider using 'transformStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformStartTime :: Lens.Lens' TransformJob (Lude.Maybe Lude.Timestamp)
traTransformStartTime = Lens.lens (transformStartTime :: TransformJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {transformStartTime = a} :: TransformJob)
{-# DEPRECATED traTransformStartTime "Use generic-lens or generic-optics with 'transformStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the AutoML job that created the transform job.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traAutoMLJobARN :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
traAutoMLJobARN = Lens.lens (autoMLJobARN :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {autoMLJobARN = a} :: TransformJob)
{-# DEPRECATED traAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

-- | The status of the transform job.
--
-- Transform job statuses are:
--
--     * @InProgress@ - The job is in progress.
--
--
--     * @Completed@ - The job has completed.
--
--
--     * @Failed@ - The transform job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTransformJob@ call.
--
--
--     * @Stopping@ - The transform job is stopping.
--
--
--     * @Stopped@ - The transform job has stopped.
--
--
--
-- /Note:/ Consider using 'transformJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformJobStatus :: Lens.Lens' TransformJob (Lude.Maybe TransformJobStatus)
traTransformJobStatus = Lens.lens (transformJobStatus :: TransformJob -> Lude.Maybe TransformJobStatus) (\s a -> s {transformJobStatus = a} :: TransformJob)
{-# DEPRECATED traTransformJobStatus "Use generic-lens or generic-optics with 'transformJobStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformInput :: Lens.Lens' TransformJob (Lude.Maybe TransformInput)
traTransformInput = Lens.lens (transformInput :: TransformJob -> Lude.Maybe TransformInput) (\s a -> s {transformInput = a} :: TransformJob)
{-# DEPRECATED traTransformInput "Use generic-lens or generic-optics with 'transformInput' instead." #-}

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or left unset, SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is 1. For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traMaxConcurrentTransforms :: Lens.Lens' TransformJob (Lude.Maybe Lude.Natural)
traMaxConcurrentTransforms = Lens.lens (maxConcurrentTransforms :: TransformJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxConcurrentTransforms = a} :: TransformJob)
{-# DEPRECATED traMaxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformOutput :: Lens.Lens' TransformJob (Lude.Maybe TransformOutput)
traTransformOutput = Lens.lens (transformOutput :: TransformJob -> Lude.Maybe TransformOutput) (\s a -> s {transformOutput = a} :: TransformJob)
{-# DEPRECATED traTransformOutput "Use generic-lens or generic-optics with 'transformOutput' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataProcessing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traDataProcessing :: Lens.Lens' TransformJob (Lude.Maybe DataProcessing)
traDataProcessing = Lens.lens (dataProcessing :: TransformJob -> Lude.Maybe DataProcessing) (\s a -> s {dataProcessing = a} :: TransformJob)
{-# DEPRECATED traDataProcessing "Use generic-lens or generic-optics with 'dataProcessing' instead." #-}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTransformJobARN :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
traTransformJobARN = Lens.lens (transformJobARN :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {transformJobARN = a} :: TransformJob)
{-# DEPRECATED traTransformJobARN "Use generic-lens or generic-optics with 'transformJobARN' instead." #-}

-- | A list of tags associated with the transform job.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
traTags :: Lens.Lens' TransformJob (Lude.Maybe [Tag])
traTags = Lens.lens (tags :: TransformJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransformJob)
{-# DEPRECATED traTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON TransformJob where
  parseJSON =
    Lude.withObject
      "TransformJob"
      ( \x ->
          TransformJob'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LabelingJobArn")
            Lude.<*> (x Lude..:? "TransformJobName")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "ModelClientConfig")
            Lude.<*> (x Lude..:? "BatchStrategy")
            Lude.<*> (x Lude..:? "MaxPayloadInMB")
            Lude.<*> (x Lude..:? "Environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TransformResources")
            Lude.<*> (x Lude..:? "ModelName")
            Lude.<*> (x Lude..:? "ExperimentConfig")
            Lude.<*> (x Lude..:? "TransformEndTime")
            Lude.<*> (x Lude..:? "TransformStartTime")
            Lude.<*> (x Lude..:? "AutoMLJobArn")
            Lude.<*> (x Lude..:? "TransformJobStatus")
            Lude.<*> (x Lude..:? "TransformInput")
            Lude.<*> (x Lude..:? "MaxConcurrentTransforms")
            Lude.<*> (x Lude..:? "TransformOutput")
            Lude.<*> (x Lude..:? "DataProcessing")
            Lude.<*> (x Lude..:? "TransformJobArn")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
