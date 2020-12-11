-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectVersionDescription
  ( ProjectVersionDescription (..),

    -- * Smart constructor
    mkProjectVersionDescription,

    -- * Lenses
    pvdMinInferenceUnits,
    pvdStatus,
    pvdEvaluationResult,
    pvdManifestSummary,
    pvdTestingDataResult,
    pvdStatusMessage,
    pvdCreationTimestamp,
    pvdProjectVersionARN,
    pvdOutputConfig,
    pvdBillableTrainingTimeInSeconds,
    pvdTrainingEndTimestamp,
    pvdTrainingDataResult,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.EvaluationResult
import Network.AWS.Rekognition.Types.GroundTruthManifest
import Network.AWS.Rekognition.Types.OutputConfig
import Network.AWS.Rekognition.Types.ProjectVersionStatus
import Network.AWS.Rekognition.Types.TestingDataResult
import Network.AWS.Rekognition.Types.TrainingDataResult

-- | The description of a version of a model.
--
-- /See:/ 'mkProjectVersionDescription' smart constructor.
data ProjectVersionDescription = ProjectVersionDescription'
  { minInferenceUnits ::
      Lude.Maybe Lude.Natural,
    status ::
      Lude.Maybe ProjectVersionStatus,
    evaluationResult ::
      Lude.Maybe EvaluationResult,
    manifestSummary ::
      Lude.Maybe GroundTruthManifest,
    testingDataResult ::
      Lude.Maybe TestingDataResult,
    statusMessage :: Lude.Maybe Lude.Text,
    creationTimestamp ::
      Lude.Maybe Lude.Timestamp,
    projectVersionARN ::
      Lude.Maybe Lude.Text,
    outputConfig :: Lude.Maybe OutputConfig,
    billableTrainingTimeInSeconds ::
      Lude.Maybe Lude.Natural,
    trainingEndTimestamp ::
      Lude.Maybe Lude.Timestamp,
    trainingDataResult ::
      Lude.Maybe TrainingDataResult
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectVersionDescription' with the minimum fields required to make a request.
--
-- * 'billableTrainingTimeInSeconds' - The duration, in seconds, that the model version has been billed for training. This value is only returned if the model version has been successfully trained.
-- * 'creationTimestamp' - The Unix datetime for the date and time that training started.
-- * 'evaluationResult' - The training results. @EvaluationResult@ is only returned if training is successful.
-- * 'manifestSummary' - The location of the summary manifest. The summary manifest provides aggregate data validation results for the training and test datasets.
-- * 'minInferenceUnits' - The minimum number of inference units used by the model. For more information, see 'StartProjectVersion' .
-- * 'outputConfig' - The location where training results are saved.
-- * 'projectVersionARN' - The Amazon Resource Name (ARN) of the model version.
-- * 'status' - The current status of the model version.
-- * 'statusMessage' - A descriptive message for an error or warning that occurred.
-- * 'testingDataResult' - Contains information about the testing results.
-- * 'trainingDataResult' - Contains information about the training results.
-- * 'trainingEndTimestamp' - The Unix date and time that training of the model ended.
mkProjectVersionDescription ::
  ProjectVersionDescription
mkProjectVersionDescription =
  ProjectVersionDescription'
    { minInferenceUnits = Lude.Nothing,
      status = Lude.Nothing,
      evaluationResult = Lude.Nothing,
      manifestSummary = Lude.Nothing,
      testingDataResult = Lude.Nothing,
      statusMessage = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      projectVersionARN = Lude.Nothing,
      outputConfig = Lude.Nothing,
      billableTrainingTimeInSeconds = Lude.Nothing,
      trainingEndTimestamp = Lude.Nothing,
      trainingDataResult = Lude.Nothing
    }

-- | The minimum number of inference units used by the model. For more information, see 'StartProjectVersion' .
--
-- /Note:/ Consider using 'minInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdMinInferenceUnits :: Lens.Lens' ProjectVersionDescription (Lude.Maybe Lude.Natural)
pvdMinInferenceUnits = Lens.lens (minInferenceUnits :: ProjectVersionDescription -> Lude.Maybe Lude.Natural) (\s a -> s {minInferenceUnits = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdMinInferenceUnits "Use generic-lens or generic-optics with 'minInferenceUnits' instead." #-}

-- | The current status of the model version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdStatus :: Lens.Lens' ProjectVersionDescription (Lude.Maybe ProjectVersionStatus)
pvdStatus = Lens.lens (status :: ProjectVersionDescription -> Lude.Maybe ProjectVersionStatus) (\s a -> s {status = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The training results. @EvaluationResult@ is only returned if training is successful.
--
-- /Note:/ Consider using 'evaluationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdEvaluationResult :: Lens.Lens' ProjectVersionDescription (Lude.Maybe EvaluationResult)
pvdEvaluationResult = Lens.lens (evaluationResult :: ProjectVersionDescription -> Lude.Maybe EvaluationResult) (\s a -> s {evaluationResult = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdEvaluationResult "Use generic-lens or generic-optics with 'evaluationResult' instead." #-}

-- | The location of the summary manifest. The summary manifest provides aggregate data validation results for the training and test datasets.
--
-- /Note:/ Consider using 'manifestSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdManifestSummary :: Lens.Lens' ProjectVersionDescription (Lude.Maybe GroundTruthManifest)
pvdManifestSummary = Lens.lens (manifestSummary :: ProjectVersionDescription -> Lude.Maybe GroundTruthManifest) (\s a -> s {manifestSummary = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdManifestSummary "Use generic-lens or generic-optics with 'manifestSummary' instead." #-}

-- | Contains information about the testing results.
--
-- /Note:/ Consider using 'testingDataResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdTestingDataResult :: Lens.Lens' ProjectVersionDescription (Lude.Maybe TestingDataResult)
pvdTestingDataResult = Lens.lens (testingDataResult :: ProjectVersionDescription -> Lude.Maybe TestingDataResult) (\s a -> s {testingDataResult = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdTestingDataResult "Use generic-lens or generic-optics with 'testingDataResult' instead." #-}

-- | A descriptive message for an error or warning that occurred.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdStatusMessage :: Lens.Lens' ProjectVersionDescription (Lude.Maybe Lude.Text)
pvdStatusMessage = Lens.lens (statusMessage :: ProjectVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The Unix datetime for the date and time that training started.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdCreationTimestamp :: Lens.Lens' ProjectVersionDescription (Lude.Maybe Lude.Timestamp)
pvdCreationTimestamp = Lens.lens (creationTimestamp :: ProjectVersionDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimestamp = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The Amazon Resource Name (ARN) of the model version.
--
-- /Note:/ Consider using 'projectVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdProjectVersionARN :: Lens.Lens' ProjectVersionDescription (Lude.Maybe Lude.Text)
pvdProjectVersionARN = Lens.lens (projectVersionARN :: ProjectVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {projectVersionARN = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdProjectVersionARN "Use generic-lens or generic-optics with 'projectVersionARN' instead." #-}

-- | The location where training results are saved.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdOutputConfig :: Lens.Lens' ProjectVersionDescription (Lude.Maybe OutputConfig)
pvdOutputConfig = Lens.lens (outputConfig :: ProjectVersionDescription -> Lude.Maybe OutputConfig) (\s a -> s {outputConfig = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The duration, in seconds, that the model version has been billed for training. This value is only returned if the model version has been successfully trained.
--
-- /Note:/ Consider using 'billableTrainingTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdBillableTrainingTimeInSeconds :: Lens.Lens' ProjectVersionDescription (Lude.Maybe Lude.Natural)
pvdBillableTrainingTimeInSeconds = Lens.lens (billableTrainingTimeInSeconds :: ProjectVersionDescription -> Lude.Maybe Lude.Natural) (\s a -> s {billableTrainingTimeInSeconds = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdBillableTrainingTimeInSeconds "Use generic-lens or generic-optics with 'billableTrainingTimeInSeconds' instead." #-}

-- | The Unix date and time that training of the model ended.
--
-- /Note:/ Consider using 'trainingEndTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdTrainingEndTimestamp :: Lens.Lens' ProjectVersionDescription (Lude.Maybe Lude.Timestamp)
pvdTrainingEndTimestamp = Lens.lens (trainingEndTimestamp :: ProjectVersionDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingEndTimestamp = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdTrainingEndTimestamp "Use generic-lens or generic-optics with 'trainingEndTimestamp' instead." #-}

-- | Contains information about the training results.
--
-- /Note:/ Consider using 'trainingDataResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdTrainingDataResult :: Lens.Lens' ProjectVersionDescription (Lude.Maybe TrainingDataResult)
pvdTrainingDataResult = Lens.lens (trainingDataResult :: ProjectVersionDescription -> Lude.Maybe TrainingDataResult) (\s a -> s {trainingDataResult = a} :: ProjectVersionDescription)
{-# DEPRECATED pvdTrainingDataResult "Use generic-lens or generic-optics with 'trainingDataResult' instead." #-}

instance Lude.FromJSON ProjectVersionDescription where
  parseJSON =
    Lude.withObject
      "ProjectVersionDescription"
      ( \x ->
          ProjectVersionDescription'
            Lude.<$> (x Lude..:? "MinInferenceUnits")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "EvaluationResult")
            Lude.<*> (x Lude..:? "ManifestSummary")
            Lude.<*> (x Lude..:? "TestingDataResult")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "CreationTimestamp")
            Lude.<*> (x Lude..:? "ProjectVersionArn")
            Lude.<*> (x Lude..:? "OutputConfig")
            Lude.<*> (x Lude..:? "BillableTrainingTimeInSeconds")
            Lude.<*> (x Lude..:? "TrainingEndTimestamp")
            Lude.<*> (x Lude..:? "TrainingDataResult")
      )
