{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmValidationProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmValidationProfile where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TrainingJobDefinition
import Network.AWS.SageMaker.Types.TransformJobDefinition

-- | Defines a training job and a batch transform job that Amazon SageMaker runs to validate your algorithm.
--
--
-- The data provided in the validation profile is made available to your buyers on AWS Marketplace.
--
--
-- /See:/ 'algorithmValidationProfile' smart constructor.
data AlgorithmValidationProfile = AlgorithmValidationProfile'
  { _avpTransformJobDefinition ::
      !(Maybe TransformJobDefinition),
    _avpProfileName :: !Text,
    _avpTrainingJobDefinition ::
      !TrainingJobDefinition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlgorithmValidationProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avpTransformJobDefinition' - The @TransformJobDefinition@ object that describes the transform job that Amazon SageMaker runs to validate your algorithm.
--
-- * 'avpProfileName' - The name of the profile for the algorithm. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- * 'avpTrainingJobDefinition' - The @TrainingJobDefinition@ object that describes the training job that Amazon SageMaker runs to validate your algorithm.
algorithmValidationProfile ::
  -- | 'avpProfileName'
  Text ->
  -- | 'avpTrainingJobDefinition'
  TrainingJobDefinition ->
  AlgorithmValidationProfile
algorithmValidationProfile pProfileName_ pTrainingJobDefinition_ =
  AlgorithmValidationProfile'
    { _avpTransformJobDefinition = Nothing,
      _avpProfileName = pProfileName_,
      _avpTrainingJobDefinition = pTrainingJobDefinition_
    }

-- | The @TransformJobDefinition@ object that describes the transform job that Amazon SageMaker runs to validate your algorithm.
avpTransformJobDefinition :: Lens' AlgorithmValidationProfile (Maybe TransformJobDefinition)
avpTransformJobDefinition = lens _avpTransformJobDefinition (\s a -> s {_avpTransformJobDefinition = a})

-- | The name of the profile for the algorithm. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
avpProfileName :: Lens' AlgorithmValidationProfile Text
avpProfileName = lens _avpProfileName (\s a -> s {_avpProfileName = a})

-- | The @TrainingJobDefinition@ object that describes the training job that Amazon SageMaker runs to validate your algorithm.
avpTrainingJobDefinition :: Lens' AlgorithmValidationProfile TrainingJobDefinition
avpTrainingJobDefinition = lens _avpTrainingJobDefinition (\s a -> s {_avpTrainingJobDefinition = a})

instance FromJSON AlgorithmValidationProfile where
  parseJSON =
    withObject
      "AlgorithmValidationProfile"
      ( \x ->
          AlgorithmValidationProfile'
            <$> (x .:? "TransformJobDefinition")
            <*> (x .: "ProfileName")
            <*> (x .: "TrainingJobDefinition")
      )

instance Hashable AlgorithmValidationProfile

instance NFData AlgorithmValidationProfile

instance ToJSON AlgorithmValidationProfile where
  toJSON AlgorithmValidationProfile' {..} =
    object
      ( catMaybes
          [ ("TransformJobDefinition" .=) <$> _avpTransformJobDefinition,
            Just ("ProfileName" .= _avpProfileName),
            Just ("TrainingJobDefinition" .= _avpTrainingJobDefinition)
          ]
      )
