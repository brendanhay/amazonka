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
-- Module      : Network.AWS.SageMaker.DescribeAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified algorithm that is in your account.
module Network.AWS.SageMaker.DescribeAlgorithm
  ( -- * Creating a Request
    describeAlgorithm,
    DescribeAlgorithm,

    -- * Request Lenses
    dAlgorithmName,

    -- * Destructuring the Response
    describeAlgorithmResponse,
    DescribeAlgorithmResponse,

    -- * Response Lenses
    daarsValidationSpecification,
    daarsInferenceSpecification,
    daarsAlgorithmDescription,
    daarsCertifyForMarketplace,
    daarsProductId,
    daarsResponseStatus,
    daarsAlgorithmName,
    daarsAlgorithmARN,
    daarsCreationTime,
    daarsTrainingSpecification,
    daarsAlgorithmStatus,
    daarsAlgorithmStatusDetails,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeAlgorithm' smart constructor.
newtype DescribeAlgorithm = DescribeAlgorithm'
  { _dAlgorithmName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAlgorithmName' - The name of the algorithm to describe.
describeAlgorithm ::
  -- | 'dAlgorithmName'
  Text ->
  DescribeAlgorithm
describeAlgorithm pAlgorithmName_ =
  DescribeAlgorithm' {_dAlgorithmName = pAlgorithmName_}

-- | The name of the algorithm to describe.
dAlgorithmName :: Lens' DescribeAlgorithm Text
dAlgorithmName = lens _dAlgorithmName (\s a -> s {_dAlgorithmName = a})

instance AWSRequest DescribeAlgorithm where
  type Rs DescribeAlgorithm = DescribeAlgorithmResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeAlgorithmResponse'
            <$> (x .?> "ValidationSpecification")
            <*> (x .?> "InferenceSpecification")
            <*> (x .?> "AlgorithmDescription")
            <*> (x .?> "CertifyForMarketplace")
            <*> (x .?> "ProductId")
            <*> (pure (fromEnum s))
            <*> (x .:> "AlgorithmName")
            <*> (x .:> "AlgorithmArn")
            <*> (x .:> "CreationTime")
            <*> (x .:> "TrainingSpecification")
            <*> (x .:> "AlgorithmStatus")
            <*> (x .:> "AlgorithmStatusDetails")
      )

instance Hashable DescribeAlgorithm

instance NFData DescribeAlgorithm

instance ToHeaders DescribeAlgorithm where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeAlgorithm" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAlgorithm where
  toJSON DescribeAlgorithm' {..} =
    object (catMaybes [Just ("AlgorithmName" .= _dAlgorithmName)])

instance ToPath DescribeAlgorithm where
  toPath = const "/"

instance ToQuery DescribeAlgorithm where
  toQuery = const mempty

-- | /See:/ 'describeAlgorithmResponse' smart constructor.
data DescribeAlgorithmResponse = DescribeAlgorithmResponse'
  { _daarsValidationSpecification ::
      !( Maybe
           AlgorithmValidationSpecification
       ),
    _daarsInferenceSpecification ::
      !(Maybe InferenceSpecification),
    _daarsAlgorithmDescription ::
      !(Maybe Text),
    _daarsCertifyForMarketplace ::
      !(Maybe Bool),
    _daarsProductId :: !(Maybe Text),
    _daarsResponseStatus :: !Int,
    _daarsAlgorithmName :: !Text,
    _daarsAlgorithmARN :: !Text,
    _daarsCreationTime :: !POSIX,
    _daarsTrainingSpecification ::
      !TrainingSpecification,
    _daarsAlgorithmStatus ::
      !AlgorithmStatus,
    _daarsAlgorithmStatusDetails ::
      !AlgorithmStatusDetails
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAlgorithmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsValidationSpecification' - Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
-- * 'daarsInferenceSpecification' - Details about inference jobs that the algorithm runs.
--
-- * 'daarsAlgorithmDescription' - A brief summary about the algorithm.
--
-- * 'daarsCertifyForMarketplace' - Whether the algorithm is certified to be listed in AWS Marketplace.
--
-- * 'daarsProductId' - The product identifier of the algorithm.
--
-- * 'daarsResponseStatus' - -- | The response status code.
--
-- * 'daarsAlgorithmName' - The name of the algorithm being described.
--
-- * 'daarsAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
--
-- * 'daarsCreationTime' - A timestamp specifying when the algorithm was created.
--
-- * 'daarsTrainingSpecification' - Details about training jobs run by this algorithm.
--
-- * 'daarsAlgorithmStatus' - The current status of the algorithm.
--
-- * 'daarsAlgorithmStatusDetails' - Details about the current status of the algorithm.
describeAlgorithmResponse ::
  -- | 'daarsResponseStatus'
  Int ->
  -- | 'daarsAlgorithmName'
  Text ->
  -- | 'daarsAlgorithmARN'
  Text ->
  -- | 'daarsCreationTime'
  UTCTime ->
  -- | 'daarsTrainingSpecification'
  TrainingSpecification ->
  -- | 'daarsAlgorithmStatus'
  AlgorithmStatus ->
  -- | 'daarsAlgorithmStatusDetails'
  AlgorithmStatusDetails ->
  DescribeAlgorithmResponse
describeAlgorithmResponse
  pResponseStatus_
  pAlgorithmName_
  pAlgorithmARN_
  pCreationTime_
  pTrainingSpecification_
  pAlgorithmStatus_
  pAlgorithmStatusDetails_ =
    DescribeAlgorithmResponse'
      { _daarsValidationSpecification =
          Nothing,
        _daarsInferenceSpecification = Nothing,
        _daarsAlgorithmDescription = Nothing,
        _daarsCertifyForMarketplace = Nothing,
        _daarsProductId = Nothing,
        _daarsResponseStatus = pResponseStatus_,
        _daarsAlgorithmName = pAlgorithmName_,
        _daarsAlgorithmARN = pAlgorithmARN_,
        _daarsCreationTime = _Time # pCreationTime_,
        _daarsTrainingSpecification = pTrainingSpecification_,
        _daarsAlgorithmStatus = pAlgorithmStatus_,
        _daarsAlgorithmStatusDetails = pAlgorithmStatusDetails_
      }

-- | Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
daarsValidationSpecification :: Lens' DescribeAlgorithmResponse (Maybe AlgorithmValidationSpecification)
daarsValidationSpecification = lens _daarsValidationSpecification (\s a -> s {_daarsValidationSpecification = a})

-- | Details about inference jobs that the algorithm runs.
daarsInferenceSpecification :: Lens' DescribeAlgorithmResponse (Maybe InferenceSpecification)
daarsInferenceSpecification = lens _daarsInferenceSpecification (\s a -> s {_daarsInferenceSpecification = a})

-- | A brief summary about the algorithm.
daarsAlgorithmDescription :: Lens' DescribeAlgorithmResponse (Maybe Text)
daarsAlgorithmDescription = lens _daarsAlgorithmDescription (\s a -> s {_daarsAlgorithmDescription = a})

-- | Whether the algorithm is certified to be listed in AWS Marketplace.
daarsCertifyForMarketplace :: Lens' DescribeAlgorithmResponse (Maybe Bool)
daarsCertifyForMarketplace = lens _daarsCertifyForMarketplace (\s a -> s {_daarsCertifyForMarketplace = a})

-- | The product identifier of the algorithm.
daarsProductId :: Lens' DescribeAlgorithmResponse (Maybe Text)
daarsProductId = lens _daarsProductId (\s a -> s {_daarsProductId = a})

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAlgorithmResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\s a -> s {_daarsResponseStatus = a})

-- | The name of the algorithm being described.
daarsAlgorithmName :: Lens' DescribeAlgorithmResponse Text
daarsAlgorithmName = lens _daarsAlgorithmName (\s a -> s {_daarsAlgorithmName = a})

-- | The Amazon Resource Name (ARN) of the algorithm.
daarsAlgorithmARN :: Lens' DescribeAlgorithmResponse Text
daarsAlgorithmARN = lens _daarsAlgorithmARN (\s a -> s {_daarsAlgorithmARN = a})

-- | A timestamp specifying when the algorithm was created.
daarsCreationTime :: Lens' DescribeAlgorithmResponse UTCTime
daarsCreationTime = lens _daarsCreationTime (\s a -> s {_daarsCreationTime = a}) . _Time

-- | Details about training jobs run by this algorithm.
daarsTrainingSpecification :: Lens' DescribeAlgorithmResponse TrainingSpecification
daarsTrainingSpecification = lens _daarsTrainingSpecification (\s a -> s {_daarsTrainingSpecification = a})

-- | The current status of the algorithm.
daarsAlgorithmStatus :: Lens' DescribeAlgorithmResponse AlgorithmStatus
daarsAlgorithmStatus = lens _daarsAlgorithmStatus (\s a -> s {_daarsAlgorithmStatus = a})

-- | Details about the current status of the algorithm.
daarsAlgorithmStatusDetails :: Lens' DescribeAlgorithmResponse AlgorithmStatusDetails
daarsAlgorithmStatusDetails = lens _daarsAlgorithmStatusDetails (\s a -> s {_daarsAlgorithmStatusDetails = a})

instance NFData DescribeAlgorithmResponse
