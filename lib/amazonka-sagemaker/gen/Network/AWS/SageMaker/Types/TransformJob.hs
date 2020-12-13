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
    tjfCreationTime,
    tjfLabelingJobARN,
    tjfTransformJobName,
    tjfFailureReason,
    tjfModelClientConfig,
    tjfBatchStrategy,
    tjfMaxPayloadInMB,
    tjfEnvironment,
    tjfTransformResources,
    tjfModelName,
    tjfExperimentConfig,
    tjfTransformEndTime,
    tjfTransformStartTime,
    tjfAutoMLJobARN,
    tjfTransformJobStatus,
    tjfTransformInput,
    tjfMaxConcurrentTransforms,
    tjfTransformOutput,
    tjfDataProcessing,
    tjfTransformJobARN,
    tjfTags,
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
  { -- | A timestamp that shows when the transform Job was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the labeling job that created the transform job.
    labelingJobARN :: Lude.Maybe Lude.Text,
    -- | The name of the transform job.
    transformJobName :: Lude.Maybe Lude.Text,
    -- | If the transform job failed, the reason it failed.
    failureReason :: Lude.Maybe Lude.Text,
    modelClientConfig :: Lude.Maybe ModelClientConfig,
    -- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A record is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
    batchStrategy :: Lude.Maybe BatchStrategy,
    -- | The maximum allowed size of the payload, in MB. A payload is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is 6 MB. For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to 0. This feature works only in supported algorithms. Currently, SageMaker built-in algorithms do not support HTTP chunked encoding.
    maxPayloadInMB :: Lude.Maybe Lude.Natural,
    -- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    transformResources :: Lude.Maybe TransformResources,
    -- | The name of the model associated with the transform job.
    modelName :: Lude.Maybe Lude.Text,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    -- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
    transformEndTime :: Lude.Maybe Lude.Timestamp,
    -- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
    transformStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the AutoML job that created the transform job.
    autoMLJobARN :: Lude.Maybe Lude.Text,
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
    transformJobStatus :: Lude.Maybe TransformJobStatus,
    transformInput :: Lude.Maybe TransformInput,
    -- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or left unset, SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is 1. For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
    maxConcurrentTransforms :: Lude.Maybe Lude.Natural,
    transformOutput :: Lude.Maybe TransformOutput,
    dataProcessing :: Lude.Maybe DataProcessing,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobARN :: Lude.Maybe Lude.Text,
    -- | A list of tags associated with the transform job.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformJob' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the transform Job was created.
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) of the labeling job that created the transform job.
-- * 'transformJobName' - The name of the transform job.
-- * 'failureReason' - If the transform job failed, the reason it failed.
-- * 'modelClientConfig' -
-- * 'batchStrategy' - Specifies the number of records to include in a mini-batch for an HTTP inference request. A record is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
-- * 'maxPayloadInMB' - The maximum allowed size of the payload, in MB. A payload is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is 6 MB. For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to 0. This feature works only in supported algorithms. Currently, SageMaker built-in algorithms do not support HTTP chunked encoding.
-- * 'environment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
-- * 'transformResources' -
-- * 'modelName' - The name of the model associated with the transform job.
-- * 'experimentConfig' -
-- * 'transformEndTime' - Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
-- * 'transformStartTime' - Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
-- * 'autoMLJobARN' - The Amazon Resource Name (ARN) of the AutoML job that created the transform job.
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
-- * 'transformInput' -
-- * 'maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or left unset, SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is 1. For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
-- * 'transformOutput' -
-- * 'dataProcessing' -
-- * 'transformJobARN' - The Amazon Resource Name (ARN) of the transform job.
-- * 'tags' - A list of tags associated with the transform job.
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
tjfCreationTime :: Lens.Lens' TransformJob (Lude.Maybe Lude.Timestamp)
tjfCreationTime = Lens.lens (creationTime :: TransformJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TransformJob)
{-# DEPRECATED tjfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the labeling job that created the transform job.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfLabelingJobARN :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
tjfLabelingJobARN = Lens.lens (labelingJobARN :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {labelingJobARN = a} :: TransformJob)
{-# DEPRECATED tjfLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}

-- | The name of the transform job.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTransformJobName :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
tjfTransformJobName = Lens.lens (transformJobName :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {transformJobName = a} :: TransformJob)
{-# DEPRECATED tjfTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

-- | If the transform job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfFailureReason :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
tjfFailureReason = Lens.lens (failureReason :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: TransformJob)
{-# DEPRECATED tjfFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'modelClientConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfModelClientConfig :: Lens.Lens' TransformJob (Lude.Maybe ModelClientConfig)
tjfModelClientConfig = Lens.lens (modelClientConfig :: TransformJob -> Lude.Maybe ModelClientConfig) (\s a -> s {modelClientConfig = a} :: TransformJob)
{-# DEPRECATED tjfModelClientConfig "Use generic-lens or generic-optics with 'modelClientConfig' instead." #-}

-- | Specifies the number of records to include in a mini-batch for an HTTP inference request. A record is a single unit of input data that inference can be made on. For example, a single line in a CSV file is a record.
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfBatchStrategy :: Lens.Lens' TransformJob (Lude.Maybe BatchStrategy)
tjfBatchStrategy = Lens.lens (batchStrategy :: TransformJob -> Lude.Maybe BatchStrategy) (\s a -> s {batchStrategy = a} :: TransformJob)
{-# DEPRECATED tjfBatchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead." #-}

-- | The maximum allowed size of the payload, in MB. A payload is the data portion of a record (without metadata). The value in @MaxPayloadInMB@ must be greater than, or equal to, the size of a single record. To estimate the size of a record in MB, divide the size of your dataset by the number of records. To ensure that the records fit within the maximum payload size, we recommend using a slightly larger value. The default value is 6 MB. For cases where the payload might be arbitrarily large and is transmitted using HTTP chunked encoding, set the value to 0. This feature works only in supported algorithms. Currently, SageMaker built-in algorithms do not support HTTP chunked encoding.
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfMaxPayloadInMB :: Lens.Lens' TransformJob (Lude.Maybe Lude.Natural)
tjfMaxPayloadInMB = Lens.lens (maxPayloadInMB :: TransformJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxPayloadInMB = a} :: TransformJob)
{-# DEPRECATED tjfMaxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead." #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfEnvironment :: Lens.Lens' TransformJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tjfEnvironment = Lens.lens (environment :: TransformJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: TransformJob)
{-# DEPRECATED tjfEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTransformResources :: Lens.Lens' TransformJob (Lude.Maybe TransformResources)
tjfTransformResources = Lens.lens (transformResources :: TransformJob -> Lude.Maybe TransformResources) (\s a -> s {transformResources = a} :: TransformJob)
{-# DEPRECATED tjfTransformResources "Use generic-lens or generic-optics with 'transformResources' instead." #-}

-- | The name of the model associated with the transform job.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfModelName :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
tjfModelName = Lens.lens (modelName :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {modelName = a} :: TransformJob)
{-# DEPRECATED tjfModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfExperimentConfig :: Lens.Lens' TransformJob (Lude.Maybe ExperimentConfig)
tjfExperimentConfig = Lens.lens (experimentConfig :: TransformJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: TransformJob)
{-# DEPRECATED tjfExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | Indicates when the transform job has been completed, or has stopped or failed. You are billed for the time interval between this time and the value of @TransformStartTime@ .
--
-- /Note:/ Consider using 'transformEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTransformEndTime :: Lens.Lens' TransformJob (Lude.Maybe Lude.Timestamp)
tjfTransformEndTime = Lens.lens (transformEndTime :: TransformJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {transformEndTime = a} :: TransformJob)
{-# DEPRECATED tjfTransformEndTime "Use generic-lens or generic-optics with 'transformEndTime' instead." #-}

-- | Indicates when the transform job starts on ML instances. You are billed for the time interval between this time and the value of @TransformEndTime@ .
--
-- /Note:/ Consider using 'transformStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTransformStartTime :: Lens.Lens' TransformJob (Lude.Maybe Lude.Timestamp)
tjfTransformStartTime = Lens.lens (transformStartTime :: TransformJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {transformStartTime = a} :: TransformJob)
{-# DEPRECATED tjfTransformStartTime "Use generic-lens or generic-optics with 'transformStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the AutoML job that created the transform job.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfAutoMLJobARN :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
tjfAutoMLJobARN = Lens.lens (autoMLJobARN :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {autoMLJobARN = a} :: TransformJob)
{-# DEPRECATED tjfAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

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
tjfTransformJobStatus :: Lens.Lens' TransformJob (Lude.Maybe TransformJobStatus)
tjfTransformJobStatus = Lens.lens (transformJobStatus :: TransformJob -> Lude.Maybe TransformJobStatus) (\s a -> s {transformJobStatus = a} :: TransformJob)
{-# DEPRECATED tjfTransformJobStatus "Use generic-lens or generic-optics with 'transformJobStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTransformInput :: Lens.Lens' TransformJob (Lude.Maybe TransformInput)
tjfTransformInput = Lens.lens (transformInput :: TransformJob -> Lude.Maybe TransformInput) (\s a -> s {transformInput = a} :: TransformJob)
{-# DEPRECATED tjfTransformInput "Use generic-lens or generic-optics with 'transformInput' instead." #-}

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. If @MaxConcurrentTransforms@ is set to 0 or left unset, SageMaker checks the optional execution-parameters to determine the settings for your chosen algorithm. If the execution-parameters endpoint is not enabled, the default value is 1. For built-in algorithms, you don't need to set a value for @MaxConcurrentTransforms@ .
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfMaxConcurrentTransforms :: Lens.Lens' TransformJob (Lude.Maybe Lude.Natural)
tjfMaxConcurrentTransforms = Lens.lens (maxConcurrentTransforms :: TransformJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxConcurrentTransforms = a} :: TransformJob)
{-# DEPRECATED tjfMaxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTransformOutput :: Lens.Lens' TransformJob (Lude.Maybe TransformOutput)
tjfTransformOutput = Lens.lens (transformOutput :: TransformJob -> Lude.Maybe TransformOutput) (\s a -> s {transformOutput = a} :: TransformJob)
{-# DEPRECATED tjfTransformOutput "Use generic-lens or generic-optics with 'transformOutput' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataProcessing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfDataProcessing :: Lens.Lens' TransformJob (Lude.Maybe DataProcessing)
tjfDataProcessing = Lens.lens (dataProcessing :: TransformJob -> Lude.Maybe DataProcessing) (\s a -> s {dataProcessing = a} :: TransformJob)
{-# DEPRECATED tjfDataProcessing "Use generic-lens or generic-optics with 'dataProcessing' instead." #-}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTransformJobARN :: Lens.Lens' TransformJob (Lude.Maybe Lude.Text)
tjfTransformJobARN = Lens.lens (transformJobARN :: TransformJob -> Lude.Maybe Lude.Text) (\s a -> s {transformJobARN = a} :: TransformJob)
{-# DEPRECATED tjfTransformJobARN "Use generic-lens or generic-optics with 'transformJobARN' instead." #-}

-- | A list of tags associated with the transform job.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjfTags :: Lens.Lens' TransformJob (Lude.Maybe [Tag])
tjfTags = Lens.lens (tags :: TransformJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransformJob)
{-# DEPRECATED tjfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
