-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobSummary
  ( LabelingJobSummary (..),

    -- * Smart constructor
    mkLabelingJobSummary,

    -- * Lenses
    ljsFailureReason,
    ljsAnnotationConsolidationLambdaARN,
    ljsInputConfig,
    ljsLabelingJobOutput,
    ljsLabelingJobName,
    ljsLabelingJobARN,
    ljsCreationTime,
    ljsLastModifiedTime,
    ljsLabelingJobStatus,
    ljsLabelCounters,
    ljsWorkteamARN,
    ljsPreHumanTaskLambdaARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.LabelCounters
import Network.AWS.SageMaker.Types.LabelingJobInputConfig
import Network.AWS.SageMaker.Types.LabelingJobOutput
import Network.AWS.SageMaker.Types.LabelingJobStatus

-- | Provides summary information about a labeling job.
--
-- /See:/ 'mkLabelingJobSummary' smart constructor.
data LabelingJobSummary = LabelingJobSummary'
  { failureReason ::
      Lude.Maybe Lude.Text,
    annotationConsolidationLambdaARN ::
      Lude.Maybe Lude.Text,
    inputConfig :: Lude.Maybe LabelingJobInputConfig,
    labelingJobOutput :: Lude.Maybe LabelingJobOutput,
    labelingJobName :: Lude.Text,
    labelingJobARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    lastModifiedTime :: Lude.Timestamp,
    labelingJobStatus :: LabelingJobStatus,
    labelCounters :: LabelCounters,
    workteamARN :: Lude.Text,
    preHumanTaskLambdaARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobSummary' with the minimum fields required to make a request.
--
-- * 'annotationConsolidationLambdaARN' - The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
-- * 'creationTime' - The date and time that the job was created (timestamp).
-- * 'failureReason' - If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
-- * 'inputConfig' - Input configuration for the labeling job.
-- * 'labelCounters' - Counts showing the progress of the labeling job.
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
-- * 'labelingJobName' - The name of the labeling job.
-- * 'labelingJobOutput' - The location of the output produced by the labeling job.
-- * 'labelingJobStatus' - The current status of the labeling job.
-- * 'lastModifiedTime' - The date and time that the job was last modified (timestamp).
-- * 'preHumanTaskLambdaARN' - The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
-- * 'workteamARN' - The Amazon Resource Name (ARN) of the work team assigned to the job.
mkLabelingJobSummary ::
  -- | 'labelingJobName'
  Lude.Text ->
  -- | 'labelingJobARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'labelingJobStatus'
  LabelingJobStatus ->
  -- | 'labelCounters'
  LabelCounters ->
  -- | 'workteamARN'
  Lude.Text ->
  -- | 'preHumanTaskLambdaARN'
  Lude.Text ->
  LabelingJobSummary
mkLabelingJobSummary
  pLabelingJobName_
  pLabelingJobARN_
  pCreationTime_
  pLastModifiedTime_
  pLabelingJobStatus_
  pLabelCounters_
  pWorkteamARN_
  pPreHumanTaskLambdaARN_ =
    LabelingJobSummary'
      { failureReason = Lude.Nothing,
        annotationConsolidationLambdaARN = Lude.Nothing,
        inputConfig = Lude.Nothing,
        labelingJobOutput = Lude.Nothing,
        labelingJobName = pLabelingJobName_,
        labelingJobARN = pLabelingJobARN_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_,
        labelingJobStatus = pLabelingJobStatus_,
        labelCounters = pLabelCounters_,
        workteamARN = pWorkteamARN_,
        preHumanTaskLambdaARN = pPreHumanTaskLambdaARN_
      }

-- | If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsFailureReason :: Lens.Lens' LabelingJobSummary (Lude.Maybe Lude.Text)
ljsFailureReason = Lens.lens (failureReason :: LabelingJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: LabelingJobSummary)
{-# DEPRECATED ljsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
--
-- /Note:/ Consider using 'annotationConsolidationLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsAnnotationConsolidationLambdaARN :: Lens.Lens' LabelingJobSummary (Lude.Maybe Lude.Text)
ljsAnnotationConsolidationLambdaARN = Lens.lens (annotationConsolidationLambdaARN :: LabelingJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {annotationConsolidationLambdaARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsAnnotationConsolidationLambdaARN "Use generic-lens or generic-optics with 'annotationConsolidationLambdaARN' instead." #-}

-- | Input configuration for the labeling job.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsInputConfig :: Lens.Lens' LabelingJobSummary (Lude.Maybe LabelingJobInputConfig)
ljsInputConfig = Lens.lens (inputConfig :: LabelingJobSummary -> Lude.Maybe LabelingJobInputConfig) (\s a -> s {inputConfig = a} :: LabelingJobSummary)
{-# DEPRECATED ljsInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | The location of the output produced by the labeling job.
--
-- /Note:/ Consider using 'labelingJobOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobOutput :: Lens.Lens' LabelingJobSummary (Lude.Maybe LabelingJobOutput)
ljsLabelingJobOutput = Lens.lens (labelingJobOutput :: LabelingJobSummary -> Lude.Maybe LabelingJobOutput) (\s a -> s {labelingJobOutput = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobOutput "Use generic-lens or generic-optics with 'labelingJobOutput' instead." #-}

-- | The name of the labeling job.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobName :: Lens.Lens' LabelingJobSummary Lude.Text
ljsLabelingJobName = Lens.lens (labelingJobName :: LabelingJobSummary -> Lude.Text) (\s a -> s {labelingJobName = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobARN :: Lens.Lens' LabelingJobSummary Lude.Text
ljsLabelingJobARN = Lens.lens (labelingJobARN :: LabelingJobSummary -> Lude.Text) (\s a -> s {labelingJobARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}

-- | The date and time that the job was created (timestamp).
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsCreationTime :: Lens.Lens' LabelingJobSummary Lude.Timestamp
ljsCreationTime = Lens.lens (creationTime :: LabelingJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: LabelingJobSummary)
{-# DEPRECATED ljsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the job was last modified (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLastModifiedTime :: Lens.Lens' LabelingJobSummary Lude.Timestamp
ljsLastModifiedTime = Lens.lens (lastModifiedTime :: LabelingJobSummary -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The current status of the labeling job.
--
-- /Note:/ Consider using 'labelingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobStatus :: Lens.Lens' LabelingJobSummary LabelingJobStatus
ljsLabelingJobStatus = Lens.lens (labelingJobStatus :: LabelingJobSummary -> LabelingJobStatus) (\s a -> s {labelingJobStatus = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobStatus "Use generic-lens or generic-optics with 'labelingJobStatus' instead." #-}

-- | Counts showing the progress of the labeling job.
--
-- /Note:/ Consider using 'labelCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelCounters :: Lens.Lens' LabelingJobSummary LabelCounters
ljsLabelCounters = Lens.lens (labelCounters :: LabelingJobSummary -> LabelCounters) (\s a -> s {labelCounters = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelCounters "Use generic-lens or generic-optics with 'labelCounters' instead." #-}

-- | The Amazon Resource Name (ARN) of the work team assigned to the job.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsWorkteamARN :: Lens.Lens' LabelingJobSummary Lude.Text
ljsWorkteamARN = Lens.lens (workteamARN :: LabelingJobSummary -> Lude.Text) (\s a -> s {workteamARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

-- | The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
--
-- /Note:/ Consider using 'preHumanTaskLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsPreHumanTaskLambdaARN :: Lens.Lens' LabelingJobSummary Lude.Text
ljsPreHumanTaskLambdaARN = Lens.lens (preHumanTaskLambdaARN :: LabelingJobSummary -> Lude.Text) (\s a -> s {preHumanTaskLambdaARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsPreHumanTaskLambdaARN "Use generic-lens or generic-optics with 'preHumanTaskLambdaARN' instead." #-}

instance Lude.FromJSON LabelingJobSummary where
  parseJSON =
    Lude.withObject
      "LabelingJobSummary"
      ( \x ->
          LabelingJobSummary'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "AnnotationConsolidationLambdaArn")
            Lude.<*> (x Lude..:? "InputConfig")
            Lude.<*> (x Lude..:? "LabelingJobOutput")
            Lude.<*> (x Lude..: "LabelingJobName")
            Lude.<*> (x Lude..: "LabelingJobArn")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LastModifiedTime")
            Lude.<*> (x Lude..: "LabelingJobStatus")
            Lude.<*> (x Lude..: "LabelCounters")
            Lude.<*> (x Lude..: "WorkteamArn")
            Lude.<*> (x Lude..: "PreHumanTaskLambdaArn")
      )
