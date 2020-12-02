{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a machine learning algorithm that you can use in Amazon SageMaker and list in the AWS Marketplace.
module Network.AWS.SageMaker.CreateAlgorithm
  ( -- * Creating a Request
    createAlgorithm,
    CreateAlgorithm,

    -- * Request Lenses
    caValidationSpecification,
    caInferenceSpecification,
    caAlgorithmDescription,
    caCertifyForMarketplace,
    caAlgorithmName,
    caTrainingSpecification,

    -- * Destructuring the Response
    createAlgorithmResponse,
    CreateAlgorithmResponse,

    -- * Response Lenses
    carsResponseStatus,
    carsAlgorithmARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createAlgorithm' smart constructor.
data CreateAlgorithm = CreateAlgorithm'
  { _caValidationSpecification ::
      !(Maybe AlgorithmValidationSpecification),
    _caInferenceSpecification ::
      !(Maybe InferenceSpecification),
    _caAlgorithmDescription :: !(Maybe Text),
    _caCertifyForMarketplace :: !(Maybe Bool),
    _caAlgorithmName :: !Text,
    _caTrainingSpecification :: !TrainingSpecification
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caValidationSpecification' - Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
--
-- * 'caInferenceSpecification' - Specifies details about inference jobs that the algorithm runs, including the following:     * The Amazon ECR paths of containers that contain the inference code and model artifacts.     * The instance types that the algorithm supports for transform jobs and real-time endpoints used for inference.     * The input and output content formats that the algorithm supports for inference.
--
-- * 'caAlgorithmDescription' - A description of the algorithm.
--
-- * 'caCertifyForMarketplace' - Whether to certify the algorithm so that it can be listed in AWS Marketplace.
--
-- * 'caAlgorithmName' - The name of the algorithm.
--
-- * 'caTrainingSpecification' - Specifies details about training jobs run by this algorithm, including the following:     * The Amazon ECR path of the container and the version digest of the algorithm.     * The hyperparameters that the algorithm supports.     * The instance types that the algorithm supports for training.     * Whether the algorithm supports distributed training.     * The metrics that the algorithm emits to Amazon CloudWatch.     * Which metrics that the algorithm emits can be used as the objective metric for hyperparameter tuning jobs.     * The input channels that the algorithm supports for training data. For example, an algorithm might support @train@ , @validation@ , and @test@ channels.
createAlgorithm ::
  -- | 'caAlgorithmName'
  Text ->
  -- | 'caTrainingSpecification'
  TrainingSpecification ->
  CreateAlgorithm
createAlgorithm pAlgorithmName_ pTrainingSpecification_ =
  CreateAlgorithm'
    { _caValidationSpecification = Nothing,
      _caInferenceSpecification = Nothing,
      _caAlgorithmDescription = Nothing,
      _caCertifyForMarketplace = Nothing,
      _caAlgorithmName = pAlgorithmName_,
      _caTrainingSpecification = pTrainingSpecification_
    }

-- | Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
caValidationSpecification :: Lens' CreateAlgorithm (Maybe AlgorithmValidationSpecification)
caValidationSpecification = lens _caValidationSpecification (\s a -> s {_caValidationSpecification = a})

-- | Specifies details about inference jobs that the algorithm runs, including the following:     * The Amazon ECR paths of containers that contain the inference code and model artifacts.     * The instance types that the algorithm supports for transform jobs and real-time endpoints used for inference.     * The input and output content formats that the algorithm supports for inference.
caInferenceSpecification :: Lens' CreateAlgorithm (Maybe InferenceSpecification)
caInferenceSpecification = lens _caInferenceSpecification (\s a -> s {_caInferenceSpecification = a})

-- | A description of the algorithm.
caAlgorithmDescription :: Lens' CreateAlgorithm (Maybe Text)
caAlgorithmDescription = lens _caAlgorithmDescription (\s a -> s {_caAlgorithmDescription = a})

-- | Whether to certify the algorithm so that it can be listed in AWS Marketplace.
caCertifyForMarketplace :: Lens' CreateAlgorithm (Maybe Bool)
caCertifyForMarketplace = lens _caCertifyForMarketplace (\s a -> s {_caCertifyForMarketplace = a})

-- | The name of the algorithm.
caAlgorithmName :: Lens' CreateAlgorithm Text
caAlgorithmName = lens _caAlgorithmName (\s a -> s {_caAlgorithmName = a})

-- | Specifies details about training jobs run by this algorithm, including the following:     * The Amazon ECR path of the container and the version digest of the algorithm.     * The hyperparameters that the algorithm supports.     * The instance types that the algorithm supports for training.     * Whether the algorithm supports distributed training.     * The metrics that the algorithm emits to Amazon CloudWatch.     * Which metrics that the algorithm emits can be used as the objective metric for hyperparameter tuning jobs.     * The input channels that the algorithm supports for training data. For example, an algorithm might support @train@ , @validation@ , and @test@ channels.
caTrainingSpecification :: Lens' CreateAlgorithm TrainingSpecification
caTrainingSpecification = lens _caTrainingSpecification (\s a -> s {_caTrainingSpecification = a})

instance AWSRequest CreateAlgorithm where
  type Rs CreateAlgorithm = CreateAlgorithmResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateAlgorithmResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "AlgorithmArn")
      )

instance Hashable CreateAlgorithm

instance NFData CreateAlgorithm

instance ToHeaders CreateAlgorithm where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.CreateAlgorithm" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateAlgorithm where
  toJSON CreateAlgorithm' {..} =
    object
      ( catMaybes
          [ ("ValidationSpecification" .=) <$> _caValidationSpecification,
            ("InferenceSpecification" .=) <$> _caInferenceSpecification,
            ("AlgorithmDescription" .=) <$> _caAlgorithmDescription,
            ("CertifyForMarketplace" .=) <$> _caCertifyForMarketplace,
            Just ("AlgorithmName" .= _caAlgorithmName),
            Just ("TrainingSpecification" .= _caTrainingSpecification)
          ]
      )

instance ToPath CreateAlgorithm where
  toPath = const "/"

instance ToQuery CreateAlgorithm where
  toQuery = const mempty

-- | /See:/ 'createAlgorithmResponse' smart constructor.
data CreateAlgorithmResponse = CreateAlgorithmResponse'
  { _carsResponseStatus ::
      !Int,
    _carsAlgorithmARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAlgorithmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsResponseStatus' - -- | The response status code.
--
-- * 'carsAlgorithmARN' - The Amazon Resource Name (ARN) of the new algorithm.
createAlgorithmResponse ::
  -- | 'carsResponseStatus'
  Int ->
  -- | 'carsAlgorithmARN'
  Text ->
  CreateAlgorithmResponse
createAlgorithmResponse pResponseStatus_ pAlgorithmARN_ =
  CreateAlgorithmResponse'
    { _carsResponseStatus = pResponseStatus_,
      _carsAlgorithmARN = pAlgorithmARN_
    }

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAlgorithmResponse Int
carsResponseStatus = lens _carsResponseStatus (\s a -> s {_carsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the new algorithm.
carsAlgorithmARN :: Lens' CreateAlgorithmResponse Text
carsAlgorithmARN = lens _carsAlgorithmARN (\s a -> s {_carsAlgorithmARN = a})

instance NFData CreateAlgorithmResponse
