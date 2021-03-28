{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ProjectVersionDescription
  ( ProjectVersionDescription (..)
  -- * Smart constructor
  , mkProjectVersionDescription
  -- * Lenses
  , pvdBillableTrainingTimeInSeconds
  , pvdCreationTimestamp
  , pvdEvaluationResult
  , pvdManifestSummary
  , pvdMinInferenceUnits
  , pvdOutputConfig
  , pvdProjectVersionArn
  , pvdStatus
  , pvdStatusMessage
  , pvdTestingDataResult
  , pvdTrainingDataResult
  , pvdTrainingEndTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.EvaluationResult as Types
import qualified Network.AWS.Rekognition.Types.GroundTruthManifest as Types
import qualified Network.AWS.Rekognition.Types.OutputConfig as Types
import qualified Network.AWS.Rekognition.Types.ProjectVersionArn as Types
import qualified Network.AWS.Rekognition.Types.ProjectVersionStatus as Types
import qualified Network.AWS.Rekognition.Types.StatusMessage as Types
import qualified Network.AWS.Rekognition.Types.TestingDataResult as Types
import qualified Network.AWS.Rekognition.Types.TrainingDataResult as Types

-- | The description of a version of a model.
--
-- /See:/ 'mkProjectVersionDescription' smart constructor.
data ProjectVersionDescription = ProjectVersionDescription'
  { billableTrainingTimeInSeconds :: Core.Maybe Core.Natural
    -- ^ The duration, in seconds, that the model version has been billed for training. This value is only returned if the model version has been successfully trained.
  , creationTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The Unix datetime for the date and time that training started.
  , evaluationResult :: Core.Maybe Types.EvaluationResult
    -- ^ The training results. @EvaluationResult@ is only returned if training is successful.
  , manifestSummary :: Core.Maybe Types.GroundTruthManifest
    -- ^ The location of the summary manifest. The summary manifest provides aggregate data validation results for the training and test datasets.
  , minInferenceUnits :: Core.Maybe Core.Natural
    -- ^ The minimum number of inference units used by the model. For more information, see 'StartProjectVersion' .
  , outputConfig :: Core.Maybe Types.OutputConfig
    -- ^ The location where training results are saved.
  , projectVersionArn :: Core.Maybe Types.ProjectVersionArn
    -- ^ The Amazon Resource Name (ARN) of the model version. 
  , status :: Core.Maybe Types.ProjectVersionStatus
    -- ^ The current status of the model version.
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ A descriptive message for an error or warning that occurred.
  , testingDataResult :: Core.Maybe Types.TestingDataResult
    -- ^ Contains information about the testing results.
  , trainingDataResult :: Core.Maybe Types.TrainingDataResult
    -- ^ Contains information about the training results.
  , trainingEndTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The Unix date and time that training of the model ended.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProjectVersionDescription' value with any optional fields omitted.
mkProjectVersionDescription
    :: ProjectVersionDescription
mkProjectVersionDescription
  = ProjectVersionDescription'{billableTrainingTimeInSeconds =
                                 Core.Nothing,
                               creationTimestamp = Core.Nothing, evaluationResult = Core.Nothing,
                               manifestSummary = Core.Nothing, minInferenceUnits = Core.Nothing,
                               outputConfig = Core.Nothing, projectVersionArn = Core.Nothing,
                               status = Core.Nothing, statusMessage = Core.Nothing,
                               testingDataResult = Core.Nothing,
                               trainingDataResult = Core.Nothing,
                               trainingEndTimestamp = Core.Nothing}

-- | The duration, in seconds, that the model version has been billed for training. This value is only returned if the model version has been successfully trained.
--
-- /Note:/ Consider using 'billableTrainingTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdBillableTrainingTimeInSeconds :: Lens.Lens' ProjectVersionDescription (Core.Maybe Core.Natural)
pvdBillableTrainingTimeInSeconds = Lens.field @"billableTrainingTimeInSeconds"
{-# INLINEABLE pvdBillableTrainingTimeInSeconds #-}
{-# DEPRECATED billableTrainingTimeInSeconds "Use generic-lens or generic-optics with 'billableTrainingTimeInSeconds' instead"  #-}

-- | The Unix datetime for the date and time that training started.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdCreationTimestamp :: Lens.Lens' ProjectVersionDescription (Core.Maybe Core.NominalDiffTime)
pvdCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE pvdCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The training results. @EvaluationResult@ is only returned if training is successful.
--
-- /Note:/ Consider using 'evaluationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdEvaluationResult :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.EvaluationResult)
pvdEvaluationResult = Lens.field @"evaluationResult"
{-# INLINEABLE pvdEvaluationResult #-}
{-# DEPRECATED evaluationResult "Use generic-lens or generic-optics with 'evaluationResult' instead"  #-}

-- | The location of the summary manifest. The summary manifest provides aggregate data validation results for the training and test datasets.
--
-- /Note:/ Consider using 'manifestSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdManifestSummary :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.GroundTruthManifest)
pvdManifestSummary = Lens.field @"manifestSummary"
{-# INLINEABLE pvdManifestSummary #-}
{-# DEPRECATED manifestSummary "Use generic-lens or generic-optics with 'manifestSummary' instead"  #-}

-- | The minimum number of inference units used by the model. For more information, see 'StartProjectVersion' .
--
-- /Note:/ Consider using 'minInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdMinInferenceUnits :: Lens.Lens' ProjectVersionDescription (Core.Maybe Core.Natural)
pvdMinInferenceUnits = Lens.field @"minInferenceUnits"
{-# INLINEABLE pvdMinInferenceUnits #-}
{-# DEPRECATED minInferenceUnits "Use generic-lens or generic-optics with 'minInferenceUnits' instead"  #-}

-- | The location where training results are saved.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdOutputConfig :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.OutputConfig)
pvdOutputConfig = Lens.field @"outputConfig"
{-# INLINEABLE pvdOutputConfig #-}
{-# DEPRECATED outputConfig "Use generic-lens or generic-optics with 'outputConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the model version. 
--
-- /Note:/ Consider using 'projectVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdProjectVersionArn :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.ProjectVersionArn)
pvdProjectVersionArn = Lens.field @"projectVersionArn"
{-# INLINEABLE pvdProjectVersionArn #-}
{-# DEPRECATED projectVersionArn "Use generic-lens or generic-optics with 'projectVersionArn' instead"  #-}

-- | The current status of the model version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdStatus :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.ProjectVersionStatus)
pvdStatus = Lens.field @"status"
{-# INLINEABLE pvdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A descriptive message for an error or warning that occurred.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdStatusMessage :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.StatusMessage)
pvdStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE pvdStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Contains information about the testing results.
--
-- /Note:/ Consider using 'testingDataResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdTestingDataResult :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.TestingDataResult)
pvdTestingDataResult = Lens.field @"testingDataResult"
{-# INLINEABLE pvdTestingDataResult #-}
{-# DEPRECATED testingDataResult "Use generic-lens or generic-optics with 'testingDataResult' instead"  #-}

-- | Contains information about the training results.
--
-- /Note:/ Consider using 'trainingDataResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdTrainingDataResult :: Lens.Lens' ProjectVersionDescription (Core.Maybe Types.TrainingDataResult)
pvdTrainingDataResult = Lens.field @"trainingDataResult"
{-# INLINEABLE pvdTrainingDataResult #-}
{-# DEPRECATED trainingDataResult "Use generic-lens or generic-optics with 'trainingDataResult' instead"  #-}

-- | The Unix date and time that training of the model ended.
--
-- /Note:/ Consider using 'trainingEndTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdTrainingEndTimestamp :: Lens.Lens' ProjectVersionDescription (Core.Maybe Core.NominalDiffTime)
pvdTrainingEndTimestamp = Lens.field @"trainingEndTimestamp"
{-# INLINEABLE pvdTrainingEndTimestamp #-}
{-# DEPRECATED trainingEndTimestamp "Use generic-lens or generic-optics with 'trainingEndTimestamp' instead"  #-}

instance Core.FromJSON ProjectVersionDescription where
        parseJSON
          = Core.withObject "ProjectVersionDescription" Core.$
              \ x ->
                ProjectVersionDescription' Core.<$>
                  (x Core..:? "BillableTrainingTimeInSeconds") Core.<*>
                    x Core..:? "CreationTimestamp"
                    Core.<*> x Core..:? "EvaluationResult"
                    Core.<*> x Core..:? "ManifestSummary"
                    Core.<*> x Core..:? "MinInferenceUnits"
                    Core.<*> x Core..:? "OutputConfig"
                    Core.<*> x Core..:? "ProjectVersionArn"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusMessage"
                    Core.<*> x Core..:? "TestingDataResult"
                    Core.<*> x Core..:? "TrainingDataResult"
                    Core.<*> x Core..:? "TrainingEndTimestamp"
