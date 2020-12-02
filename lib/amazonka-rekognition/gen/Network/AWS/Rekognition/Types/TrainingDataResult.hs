{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TrainingDataResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TrainingDataResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.TrainingData
import Network.AWS.Rekognition.Types.ValidationData

-- | Sagemaker Groundtruth format manifest files for the input, output and validation datasets that are used and created during testing.
--
--
--
-- /See:/ 'trainingDataResult' smart constructor.
data TrainingDataResult = TrainingDataResult'
  { _tInput ::
      !(Maybe TrainingData),
    _tOutput :: !(Maybe TrainingData),
    _tValidation :: !(Maybe ValidationData)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrainingDataResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tInput' - The training assets that you supplied for training.
--
-- * 'tOutput' - The images (assets) that were actually trained by Amazon Rekognition Custom Labels.
--
-- * 'tValidation' - The location of the data validation manifest. The data validation manifest is created for the training dataset during model training.
trainingDataResult ::
  TrainingDataResult
trainingDataResult =
  TrainingDataResult'
    { _tInput = Nothing,
      _tOutput = Nothing,
      _tValidation = Nothing
    }

-- | The training assets that you supplied for training.
tInput :: Lens' TrainingDataResult (Maybe TrainingData)
tInput = lens _tInput (\s a -> s {_tInput = a})

-- | The images (assets) that were actually trained by Amazon Rekognition Custom Labels.
tOutput :: Lens' TrainingDataResult (Maybe TrainingData)
tOutput = lens _tOutput (\s a -> s {_tOutput = a})

-- | The location of the data validation manifest. The data validation manifest is created for the training dataset during model training.
tValidation :: Lens' TrainingDataResult (Maybe ValidationData)
tValidation = lens _tValidation (\s a -> s {_tValidation = a})

instance FromJSON TrainingDataResult where
  parseJSON =
    withObject
      "TrainingDataResult"
      ( \x ->
          TrainingDataResult'
            <$> (x .:? "Input") <*> (x .:? "Output") <*> (x .:? "Validation")
      )

instance Hashable TrainingDataResult

instance NFData TrainingDataResult
