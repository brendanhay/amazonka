{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectVersionDescription where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.EvaluationResult
import Network.AWS.Rekognition.Types.GroundTruthManifest
import Network.AWS.Rekognition.Types.OutputConfig
import Network.AWS.Rekognition.Types.ProjectVersionStatus
import Network.AWS.Rekognition.Types.TestingDataResult
import Network.AWS.Rekognition.Types.TrainingDataResult

-- | The description of a version of a model.
--
--
--
-- /See:/ 'projectVersionDescription' smart constructor.
data ProjectVersionDescription = ProjectVersionDescription'
  { _pvdMinInferenceUnits ::
      !(Maybe Nat),
    _pvdStatus ::
      !(Maybe ProjectVersionStatus),
    _pvdEvaluationResult ::
      !(Maybe EvaluationResult),
    _pvdManifestSummary ::
      !(Maybe GroundTruthManifest),
    _pvdTestingDataResult ::
      !(Maybe TestingDataResult),
    _pvdStatusMessage :: !(Maybe Text),
    _pvdCreationTimestamp :: !(Maybe POSIX),
    _pvdProjectVersionARN :: !(Maybe Text),
    _pvdOutputConfig ::
      !(Maybe OutputConfig),
    _pvdBillableTrainingTimeInSeconds ::
      !(Maybe Nat),
    _pvdTrainingEndTimestamp ::
      !(Maybe POSIX),
    _pvdTrainingDataResult ::
      !(Maybe TrainingDataResult)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectVersionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvdMinInferenceUnits' - The minimum number of inference units used by the model. For more information, see 'StartProjectVersion' .
--
-- * 'pvdStatus' - The current status of the model version.
--
-- * 'pvdEvaluationResult' - The training results. @EvaluationResult@ is only returned if training is successful.
--
-- * 'pvdManifestSummary' - The location of the summary manifest. The summary manifest provides aggregate data validation results for the training and test datasets.
--
-- * 'pvdTestingDataResult' - Contains information about the testing results.
--
-- * 'pvdStatusMessage' - A descriptive message for an error or warning that occurred.
--
-- * 'pvdCreationTimestamp' - The Unix datetime for the date and time that training started.
--
-- * 'pvdProjectVersionARN' - The Amazon Resource Name (ARN) of the model version.
--
-- * 'pvdOutputConfig' - The location where training results are saved.
--
-- * 'pvdBillableTrainingTimeInSeconds' - The duration, in seconds, that the model version has been billed for training. This value is only returned if the model version has been successfully trained.
--
-- * 'pvdTrainingEndTimestamp' - The Unix date and time that training of the model ended.
--
-- * 'pvdTrainingDataResult' - Contains information about the training results.
projectVersionDescription ::
  ProjectVersionDescription
projectVersionDescription =
  ProjectVersionDescription'
    { _pvdMinInferenceUnits = Nothing,
      _pvdStatus = Nothing,
      _pvdEvaluationResult = Nothing,
      _pvdManifestSummary = Nothing,
      _pvdTestingDataResult = Nothing,
      _pvdStatusMessage = Nothing,
      _pvdCreationTimestamp = Nothing,
      _pvdProjectVersionARN = Nothing,
      _pvdOutputConfig = Nothing,
      _pvdBillableTrainingTimeInSeconds = Nothing,
      _pvdTrainingEndTimestamp = Nothing,
      _pvdTrainingDataResult = Nothing
    }

-- | The minimum number of inference units used by the model. For more information, see 'StartProjectVersion' .
pvdMinInferenceUnits :: Lens' ProjectVersionDescription (Maybe Natural)
pvdMinInferenceUnits = lens _pvdMinInferenceUnits (\s a -> s {_pvdMinInferenceUnits = a}) . mapping _Nat

-- | The current status of the model version.
pvdStatus :: Lens' ProjectVersionDescription (Maybe ProjectVersionStatus)
pvdStatus = lens _pvdStatus (\s a -> s {_pvdStatus = a})

-- | The training results. @EvaluationResult@ is only returned if training is successful.
pvdEvaluationResult :: Lens' ProjectVersionDescription (Maybe EvaluationResult)
pvdEvaluationResult = lens _pvdEvaluationResult (\s a -> s {_pvdEvaluationResult = a})

-- | The location of the summary manifest. The summary manifest provides aggregate data validation results for the training and test datasets.
pvdManifestSummary :: Lens' ProjectVersionDescription (Maybe GroundTruthManifest)
pvdManifestSummary = lens _pvdManifestSummary (\s a -> s {_pvdManifestSummary = a})

-- | Contains information about the testing results.
pvdTestingDataResult :: Lens' ProjectVersionDescription (Maybe TestingDataResult)
pvdTestingDataResult = lens _pvdTestingDataResult (\s a -> s {_pvdTestingDataResult = a})

-- | A descriptive message for an error or warning that occurred.
pvdStatusMessage :: Lens' ProjectVersionDescription (Maybe Text)
pvdStatusMessage = lens _pvdStatusMessage (\s a -> s {_pvdStatusMessage = a})

-- | The Unix datetime for the date and time that training started.
pvdCreationTimestamp :: Lens' ProjectVersionDescription (Maybe UTCTime)
pvdCreationTimestamp = lens _pvdCreationTimestamp (\s a -> s {_pvdCreationTimestamp = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the model version.
pvdProjectVersionARN :: Lens' ProjectVersionDescription (Maybe Text)
pvdProjectVersionARN = lens _pvdProjectVersionARN (\s a -> s {_pvdProjectVersionARN = a})

-- | The location where training results are saved.
pvdOutputConfig :: Lens' ProjectVersionDescription (Maybe OutputConfig)
pvdOutputConfig = lens _pvdOutputConfig (\s a -> s {_pvdOutputConfig = a})

-- | The duration, in seconds, that the model version has been billed for training. This value is only returned if the model version has been successfully trained.
pvdBillableTrainingTimeInSeconds :: Lens' ProjectVersionDescription (Maybe Natural)
pvdBillableTrainingTimeInSeconds = lens _pvdBillableTrainingTimeInSeconds (\s a -> s {_pvdBillableTrainingTimeInSeconds = a}) . mapping _Nat

-- | The Unix date and time that training of the model ended.
pvdTrainingEndTimestamp :: Lens' ProjectVersionDescription (Maybe UTCTime)
pvdTrainingEndTimestamp = lens _pvdTrainingEndTimestamp (\s a -> s {_pvdTrainingEndTimestamp = a}) . mapping _Time

-- | Contains information about the training results.
pvdTrainingDataResult :: Lens' ProjectVersionDescription (Maybe TrainingDataResult)
pvdTrainingDataResult = lens _pvdTrainingDataResult (\s a -> s {_pvdTrainingDataResult = a})

instance FromJSON ProjectVersionDescription where
  parseJSON =
    withObject
      "ProjectVersionDescription"
      ( \x ->
          ProjectVersionDescription'
            <$> (x .:? "MinInferenceUnits")
            <*> (x .:? "Status")
            <*> (x .:? "EvaluationResult")
            <*> (x .:? "ManifestSummary")
            <*> (x .:? "TestingDataResult")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "CreationTimestamp")
            <*> (x .:? "ProjectVersionArn")
            <*> (x .:? "OutputConfig")
            <*> (x .:? "BillableTrainingTimeInSeconds")
            <*> (x .:? "TrainingEndTimestamp")
            <*> (x .:? "TrainingDataResult")
      )

instance Hashable ProjectVersionDescription

instance NFData ProjectVersionDescription
