{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Defines the input needed to run a training job using the algorithm.
--
--
--
-- /See:/ 'trainingJobDefinition' smart constructor.
data TrainingJobDefinition = TrainingJobDefinition'
  { _tjdHyperParameters ::
      !(Maybe (Map Text (Text))),
    _tjdTrainingInputMode :: !TrainingInputMode,
    _tjdInputDataConfig :: !(List1 Channel),
    _tjdOutputDataConfig :: !OutputDataConfig,
    _tjdResourceConfig :: !ResourceConfig,
    _tjdStoppingCondition :: !StoppingCondition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrainingJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjdHyperParameters' - The hyperparameters used for the training job.
--
-- * 'tjdTrainingInputMode' - The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
--
-- * 'tjdInputDataConfig' - An array of @Channel@ objects, each of which specifies an input source.
--
-- * 'tjdOutputDataConfig' - the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- * 'tjdResourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- * 'tjdStoppingCondition' - Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts.
trainingJobDefinition ::
  -- | 'tjdTrainingInputMode'
  TrainingInputMode ->
  -- | 'tjdInputDataConfig'
  NonEmpty Channel ->
  -- | 'tjdOutputDataConfig'
  OutputDataConfig ->
  -- | 'tjdResourceConfig'
  ResourceConfig ->
  -- | 'tjdStoppingCondition'
  StoppingCondition ->
  TrainingJobDefinition
trainingJobDefinition
  pTrainingInputMode_
  pInputDataConfig_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    TrainingJobDefinition'
      { _tjdHyperParameters = Nothing,
        _tjdTrainingInputMode = pTrainingInputMode_,
        _tjdInputDataConfig = _List1 # pInputDataConfig_,
        _tjdOutputDataConfig = pOutputDataConfig_,
        _tjdResourceConfig = pResourceConfig_,
        _tjdStoppingCondition = pStoppingCondition_
      }

-- | The hyperparameters used for the training job.
tjdHyperParameters :: Lens' TrainingJobDefinition (HashMap Text (Text))
tjdHyperParameters = lens _tjdHyperParameters (\s a -> s {_tjdHyperParameters = a}) . _Default . _Map

-- | The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
tjdTrainingInputMode :: Lens' TrainingJobDefinition TrainingInputMode
tjdTrainingInputMode = lens _tjdTrainingInputMode (\s a -> s {_tjdTrainingInputMode = a})

-- | An array of @Channel@ objects, each of which specifies an input source.
tjdInputDataConfig :: Lens' TrainingJobDefinition (NonEmpty Channel)
tjdInputDataConfig = lens _tjdInputDataConfig (\s a -> s {_tjdInputDataConfig = a}) . _List1

-- | the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
tjdOutputDataConfig :: Lens' TrainingJobDefinition OutputDataConfig
tjdOutputDataConfig = lens _tjdOutputDataConfig (\s a -> s {_tjdOutputDataConfig = a})

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
tjdResourceConfig :: Lens' TrainingJobDefinition ResourceConfig
tjdResourceConfig = lens _tjdResourceConfig (\s a -> s {_tjdResourceConfig = a})

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts.
tjdStoppingCondition :: Lens' TrainingJobDefinition StoppingCondition
tjdStoppingCondition = lens _tjdStoppingCondition (\s a -> s {_tjdStoppingCondition = a})

instance FromJSON TrainingJobDefinition where
  parseJSON =
    withObject
      "TrainingJobDefinition"
      ( \x ->
          TrainingJobDefinition'
            <$> (x .:? "HyperParameters" .!= mempty)
            <*> (x .: "TrainingInputMode")
            <*> (x .: "InputDataConfig")
            <*> (x .: "OutputDataConfig")
            <*> (x .: "ResourceConfig")
            <*> (x .: "StoppingCondition")
      )

instance Hashable TrainingJobDefinition

instance NFData TrainingJobDefinition

instance ToJSON TrainingJobDefinition where
  toJSON TrainingJobDefinition' {..} =
    object
      ( catMaybes
          [ ("HyperParameters" .=) <$> _tjdHyperParameters,
            Just ("TrainingInputMode" .= _tjdTrainingInputMode),
            Just ("InputDataConfig" .= _tjdInputDataConfig),
            Just ("OutputDataConfig" .= _tjdOutputDataConfig),
            Just ("ResourceConfig" .= _tjdResourceConfig),
            Just ("StoppingCondition" .= _tjdStoppingCondition)
          ]
      )
