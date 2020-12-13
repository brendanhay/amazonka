{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ljsCreationTime,
    ljsLabelingJobARN,
    ljsFailureReason,
    ljsPreHumanTaskLambdaARN,
    ljsAnnotationConsolidationLambdaARN,
    ljsLastModifiedTime,
    ljsWorkteamARN,
    ljsLabelCounters,
    ljsInputConfig,
    ljsLabelingJobStatus,
    ljsLabelingJobName,
    ljsLabelingJobOutput,
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
  { -- | The date and time that the job was created (timestamp).
    creationTime :: Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
    labelingJobARN :: Lude.Text,
    -- | If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
    preHumanTaskLambdaARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
    annotationConsolidationLambdaARN :: Lude.Maybe Lude.Text,
    -- | The date and time that the job was last modified (timestamp).
    lastModifiedTime :: Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the work team assigned to the job.
    workteamARN :: Lude.Text,
    -- | Counts showing the progress of the labeling job.
    labelCounters :: LabelCounters,
    -- | Input configuration for the labeling job.
    inputConfig :: Lude.Maybe LabelingJobInputConfig,
    -- | The current status of the labeling job.
    labelingJobStatus :: LabelingJobStatus,
    -- | The name of the labeling job.
    labelingJobName :: Lude.Text,
    -- | The location of the output produced by the labeling job.
    labelingJobOutput :: Lude.Maybe LabelingJobOutput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time that the job was created (timestamp).
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
-- * 'failureReason' - If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
-- * 'preHumanTaskLambdaARN' - The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
-- * 'annotationConsolidationLambdaARN' - The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
-- * 'lastModifiedTime' - The date and time that the job was last modified (timestamp).
-- * 'workteamARN' - The Amazon Resource Name (ARN) of the work team assigned to the job.
-- * 'labelCounters' - Counts showing the progress of the labeling job.
-- * 'inputConfig' - Input configuration for the labeling job.
-- * 'labelingJobStatus' - The current status of the labeling job.
-- * 'labelingJobName' - The name of the labeling job.
-- * 'labelingJobOutput' - The location of the output produced by the labeling job.
mkLabelingJobSummary ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'labelingJobARN'
  Lude.Text ->
  -- | 'preHumanTaskLambdaARN'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'workteamARN'
  Lude.Text ->
  -- | 'labelCounters'
  LabelCounters ->
  -- | 'labelingJobStatus'
  LabelingJobStatus ->
  -- | 'labelingJobName'
  Lude.Text ->
  LabelingJobSummary
mkLabelingJobSummary
  pCreationTime_
  pLabelingJobARN_
  pPreHumanTaskLambdaARN_
  pLastModifiedTime_
  pWorkteamARN_
  pLabelCounters_
  pLabelingJobStatus_
  pLabelingJobName_ =
    LabelingJobSummary'
      { creationTime = pCreationTime_,
        labelingJobARN = pLabelingJobARN_,
        failureReason = Lude.Nothing,
        preHumanTaskLambdaARN = pPreHumanTaskLambdaARN_,
        annotationConsolidationLambdaARN = Lude.Nothing,
        lastModifiedTime = pLastModifiedTime_,
        workteamARN = pWorkteamARN_,
        labelCounters = pLabelCounters_,
        inputConfig = Lude.Nothing,
        labelingJobStatus = pLabelingJobStatus_,
        labelingJobName = pLabelingJobName_,
        labelingJobOutput = Lude.Nothing
      }

-- | The date and time that the job was created (timestamp).
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsCreationTime :: Lens.Lens' LabelingJobSummary Lude.Timestamp
ljsCreationTime = Lens.lens (creationTime :: LabelingJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: LabelingJobSummary)
{-# DEPRECATED ljsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobARN :: Lens.Lens' LabelingJobSummary Lude.Text
ljsLabelingJobARN = Lens.lens (labelingJobARN :: LabelingJobSummary -> Lude.Text) (\s a -> s {labelingJobARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}

-- | If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsFailureReason :: Lens.Lens' LabelingJobSummary (Lude.Maybe Lude.Text)
ljsFailureReason = Lens.lens (failureReason :: LabelingJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: LabelingJobSummary)
{-# DEPRECATED ljsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
--
-- /Note:/ Consider using 'preHumanTaskLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsPreHumanTaskLambdaARN :: Lens.Lens' LabelingJobSummary Lude.Text
ljsPreHumanTaskLambdaARN = Lens.lens (preHumanTaskLambdaARN :: LabelingJobSummary -> Lude.Text) (\s a -> s {preHumanTaskLambdaARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsPreHumanTaskLambdaARN "Use generic-lens or generic-optics with 'preHumanTaskLambdaARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
--
-- /Note:/ Consider using 'annotationConsolidationLambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsAnnotationConsolidationLambdaARN :: Lens.Lens' LabelingJobSummary (Lude.Maybe Lude.Text)
ljsAnnotationConsolidationLambdaARN = Lens.lens (annotationConsolidationLambdaARN :: LabelingJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {annotationConsolidationLambdaARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsAnnotationConsolidationLambdaARN "Use generic-lens or generic-optics with 'annotationConsolidationLambdaARN' instead." #-}

-- | The date and time that the job was last modified (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLastModifiedTime :: Lens.Lens' LabelingJobSummary Lude.Timestamp
ljsLastModifiedTime = Lens.lens (lastModifiedTime :: LabelingJobSummary -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the work team assigned to the job.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsWorkteamARN :: Lens.Lens' LabelingJobSummary Lude.Text
ljsWorkteamARN = Lens.lens (workteamARN :: LabelingJobSummary -> Lude.Text) (\s a -> s {workteamARN = a} :: LabelingJobSummary)
{-# DEPRECATED ljsWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

-- | Counts showing the progress of the labeling job.
--
-- /Note:/ Consider using 'labelCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelCounters :: Lens.Lens' LabelingJobSummary LabelCounters
ljsLabelCounters = Lens.lens (labelCounters :: LabelingJobSummary -> LabelCounters) (\s a -> s {labelCounters = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelCounters "Use generic-lens or generic-optics with 'labelCounters' instead." #-}

-- | Input configuration for the labeling job.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsInputConfig :: Lens.Lens' LabelingJobSummary (Lude.Maybe LabelingJobInputConfig)
ljsInputConfig = Lens.lens (inputConfig :: LabelingJobSummary -> Lude.Maybe LabelingJobInputConfig) (\s a -> s {inputConfig = a} :: LabelingJobSummary)
{-# DEPRECATED ljsInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | The current status of the labeling job.
--
-- /Note:/ Consider using 'labelingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobStatus :: Lens.Lens' LabelingJobSummary LabelingJobStatus
ljsLabelingJobStatus = Lens.lens (labelingJobStatus :: LabelingJobSummary -> LabelingJobStatus) (\s a -> s {labelingJobStatus = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobStatus "Use generic-lens or generic-optics with 'labelingJobStatus' instead." #-}

-- | The name of the labeling job.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobName :: Lens.Lens' LabelingJobSummary Lude.Text
ljsLabelingJobName = Lens.lens (labelingJobName :: LabelingJobSummary -> Lude.Text) (\s a -> s {labelingJobName = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | The location of the output produced by the labeling job.
--
-- /Note:/ Consider using 'labelingJobOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobOutput :: Lens.Lens' LabelingJobSummary (Lude.Maybe LabelingJobOutput)
ljsLabelingJobOutput = Lens.lens (labelingJobOutput :: LabelingJobSummary -> Lude.Maybe LabelingJobOutput) (\s a -> s {labelingJobOutput = a} :: LabelingJobSummary)
{-# DEPRECATED ljsLabelingJobOutput "Use generic-lens or generic-optics with 'labelingJobOutput' instead." #-}

instance Lude.FromJSON LabelingJobSummary where
  parseJSON =
    Lude.withObject
      "LabelingJobSummary"
      ( \x ->
          LabelingJobSummary'
            Lude.<$> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LabelingJobArn")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..: "PreHumanTaskLambdaArn")
            Lude.<*> (x Lude..:? "AnnotationConsolidationLambdaArn")
            Lude.<*> (x Lude..: "LastModifiedTime")
            Lude.<*> (x Lude..: "WorkteamArn")
            Lude.<*> (x Lude..: "LabelCounters")
            Lude.<*> (x Lude..:? "InputConfig")
            Lude.<*> (x Lude..: "LabelingJobStatus")
            Lude.<*> (x Lude..: "LabelingJobName")
            Lude.<*> (x Lude..:? "LabelingJobOutput")
      )
