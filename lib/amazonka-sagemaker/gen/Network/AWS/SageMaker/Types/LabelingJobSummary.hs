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
    ljsLabelingJobName,
    ljsLabelingJobArn,
    ljsCreationTime,
    ljsLastModifiedTime,
    ljsLabelingJobStatus,
    ljsLabelCounters,
    ljsWorkteamArn,
    ljsPreHumanTaskLambdaArn,
    ljsAnnotationConsolidationLambdaArn,
    ljsFailureReason,
    ljsInputConfig,
    ljsLabelingJobOutput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AnnotationConsolidationLambdaArn as Types
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.LabelCounters as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobArn as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobInputConfig as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobName as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobOutput as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobStatus as Types
import qualified Network.AWS.SageMaker.Types.PreHumanTaskLambdaArn as Types
import qualified Network.AWS.SageMaker.Types.WorkteamArn as Types

-- | Provides summary information about a labeling job.
--
-- /See:/ 'mkLabelingJobSummary' smart constructor.
data LabelingJobSummary = LabelingJobSummary'
  { -- | The name of the labeling job.
    labelingJobName :: Types.LabelingJobName,
    -- | The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
    labelingJobArn :: Types.LabelingJobArn,
    -- | The date and time that the job was created (timestamp).
    creationTime :: Core.NominalDiffTime,
    -- | The date and time that the job was last modified (timestamp).
    lastModifiedTime :: Core.NominalDiffTime,
    -- | The current status of the labeling job.
    labelingJobStatus :: Types.LabelingJobStatus,
    -- | Counts showing the progress of the labeling job.
    labelCounters :: Types.LabelCounters,
    -- | The Amazon Resource Name (ARN) of the work team assigned to the job.
    workteamArn :: Types.WorkteamArn,
    -- | The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
    preHumanTaskLambdaArn :: Types.PreHumanTaskLambdaArn,
    -- | The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
    annotationConsolidationLambdaArn :: Core.Maybe Types.AnnotationConsolidationLambdaArn,
    -- | If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | Input configuration for the labeling job.
    inputConfig :: Core.Maybe Types.LabelingJobInputConfig,
    -- | The location of the output produced by the labeling job.
    labelingJobOutput :: Core.Maybe Types.LabelingJobOutput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LabelingJobSummary' value with any optional fields omitted.
mkLabelingJobSummary ::
  -- | 'labelingJobName'
  Types.LabelingJobName ->
  -- | 'labelingJobArn'
  Types.LabelingJobArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  -- | 'labelingJobStatus'
  Types.LabelingJobStatus ->
  -- | 'labelCounters'
  Types.LabelCounters ->
  -- | 'workteamArn'
  Types.WorkteamArn ->
  -- | 'preHumanTaskLambdaArn'
  Types.PreHumanTaskLambdaArn ->
  LabelingJobSummary
mkLabelingJobSummary
  labelingJobName
  labelingJobArn
  creationTime
  lastModifiedTime
  labelingJobStatus
  labelCounters
  workteamArn
  preHumanTaskLambdaArn =
    LabelingJobSummary'
      { labelingJobName,
        labelingJobArn,
        creationTime,
        lastModifiedTime,
        labelingJobStatus,
        labelCounters,
        workteamArn,
        preHumanTaskLambdaArn,
        annotationConsolidationLambdaArn = Core.Nothing,
        failureReason = Core.Nothing,
        inputConfig = Core.Nothing,
        labelingJobOutput = Core.Nothing
      }

-- | The name of the labeling job.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobName :: Lens.Lens' LabelingJobSummary Types.LabelingJobName
ljsLabelingJobName = Lens.field @"labelingJobName"
{-# DEPRECATED ljsLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
--
-- /Note:/ Consider using 'labelingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobArn :: Lens.Lens' LabelingJobSummary Types.LabelingJobArn
ljsLabelingJobArn = Lens.field @"labelingJobArn"
{-# DEPRECATED ljsLabelingJobArn "Use generic-lens or generic-optics with 'labelingJobArn' instead." #-}

-- | The date and time that the job was created (timestamp).
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsCreationTime :: Lens.Lens' LabelingJobSummary Core.NominalDiffTime
ljsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED ljsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the job was last modified (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLastModifiedTime :: Lens.Lens' LabelingJobSummary Core.NominalDiffTime
ljsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED ljsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The current status of the labeling job.
--
-- /Note:/ Consider using 'labelingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobStatus :: Lens.Lens' LabelingJobSummary Types.LabelingJobStatus
ljsLabelingJobStatus = Lens.field @"labelingJobStatus"
{-# DEPRECATED ljsLabelingJobStatus "Use generic-lens or generic-optics with 'labelingJobStatus' instead." #-}

-- | Counts showing the progress of the labeling job.
--
-- /Note:/ Consider using 'labelCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelCounters :: Lens.Lens' LabelingJobSummary Types.LabelCounters
ljsLabelCounters = Lens.field @"labelCounters"
{-# DEPRECATED ljsLabelCounters "Use generic-lens or generic-optics with 'labelCounters' instead." #-}

-- | The Amazon Resource Name (ARN) of the work team assigned to the job.
--
-- /Note:/ Consider using 'workteamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsWorkteamArn :: Lens.Lens' LabelingJobSummary Types.WorkteamArn
ljsWorkteamArn = Lens.field @"workteamArn"
{-# DEPRECATED ljsWorkteamArn "Use generic-lens or generic-optics with 'workteamArn' instead." #-}

-- | The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
--
-- /Note:/ Consider using 'preHumanTaskLambdaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsPreHumanTaskLambdaArn :: Lens.Lens' LabelingJobSummary Types.PreHumanTaskLambdaArn
ljsPreHumanTaskLambdaArn = Lens.field @"preHumanTaskLambdaArn"
{-# DEPRECATED ljsPreHumanTaskLambdaArn "Use generic-lens or generic-optics with 'preHumanTaskLambdaArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
--
-- /Note:/ Consider using 'annotationConsolidationLambdaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsAnnotationConsolidationLambdaArn :: Lens.Lens' LabelingJobSummary (Core.Maybe Types.AnnotationConsolidationLambdaArn)
ljsAnnotationConsolidationLambdaArn = Lens.field @"annotationConsolidationLambdaArn"
{-# DEPRECATED ljsAnnotationConsolidationLambdaArn "Use generic-lens or generic-optics with 'annotationConsolidationLambdaArn' instead." #-}

-- | If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsFailureReason :: Lens.Lens' LabelingJobSummary (Core.Maybe Types.FailureReason)
ljsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED ljsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Input configuration for the labeling job.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsInputConfig :: Lens.Lens' LabelingJobSummary (Core.Maybe Types.LabelingJobInputConfig)
ljsInputConfig = Lens.field @"inputConfig"
{-# DEPRECATED ljsInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | The location of the output produced by the labeling job.
--
-- /Note:/ Consider using 'labelingJobOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsLabelingJobOutput :: Lens.Lens' LabelingJobSummary (Core.Maybe Types.LabelingJobOutput)
ljsLabelingJobOutput = Lens.field @"labelingJobOutput"
{-# DEPRECATED ljsLabelingJobOutput "Use generic-lens or generic-optics with 'labelingJobOutput' instead." #-}

instance Core.FromJSON LabelingJobSummary where
  parseJSON =
    Core.withObject "LabelingJobSummary" Core.$
      \x ->
        LabelingJobSummary'
          Core.<$> (x Core..: "LabelingJobName")
          Core.<*> (x Core..: "LabelingJobArn")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "LastModifiedTime")
          Core.<*> (x Core..: "LabelingJobStatus")
          Core.<*> (x Core..: "LabelCounters")
          Core.<*> (x Core..: "WorkteamArn")
          Core.<*> (x Core..: "PreHumanTaskLambdaArn")
          Core.<*> (x Core..:? "AnnotationConsolidationLambdaArn")
          Core.<*> (x Core..:? "FailureReason")
          Core.<*> (x Core..:? "InputConfig")
          Core.<*> (x Core..:? "LabelingJobOutput")
