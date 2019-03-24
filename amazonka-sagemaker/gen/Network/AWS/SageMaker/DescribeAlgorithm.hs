{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified algorithm that is in your account.
--
--
module Network.AWS.SageMaker.DescribeAlgorithm
    (
    -- * Creating a Request
      describeAlgorithm
    , DescribeAlgorithm
    -- * Request Lenses
    , dAlgorithmName

    -- * Destructuring the Response
    , describeAlgorithmResponse
    , DescribeAlgorithmResponse
    -- * Response Lenses
    , darsValidationSpecification
    , darsInferenceSpecification
    , darsAlgorithmDescription
    , darsCertifyForMarketplace
    , darsProductId
    , darsResponseStatus
    , darsAlgorithmName
    , darsAlgorithmARN
    , darsCreationTime
    , darsTrainingSpecification
    , darsAlgorithmStatus
    , darsAlgorithmStatusDetails
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeAlgorithm' smart constructor.
newtype DescribeAlgorithm = DescribeAlgorithm'
  { _dAlgorithmName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAlgorithmName' - The name of the algorithm to describe.
describeAlgorithm
    :: Text -- ^ 'dAlgorithmName'
    -> DescribeAlgorithm
describeAlgorithm pAlgorithmName_ =
  DescribeAlgorithm' {_dAlgorithmName = pAlgorithmName_}


-- | The name of the algorithm to describe.
dAlgorithmName :: Lens' DescribeAlgorithm Text
dAlgorithmName = lens _dAlgorithmName (\ s a -> s{_dAlgorithmName = a})

instance AWSRequest DescribeAlgorithm where
        type Rs DescribeAlgorithm = DescribeAlgorithmResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAlgorithmResponse' <$>
                   (x .?> "ValidationSpecification") <*>
                     (x .?> "InferenceSpecification")
                     <*> (x .?> "AlgorithmDescription")
                     <*> (x .?> "CertifyForMarketplace")
                     <*> (x .?> "ProductId")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "AlgorithmName")
                     <*> (x .:> "AlgorithmArn")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "TrainingSpecification")
                     <*> (x .:> "AlgorithmStatus")
                     <*> (x .:> "AlgorithmStatusDetails"))

instance Hashable DescribeAlgorithm where

instance NFData DescribeAlgorithm where

instance ToHeaders DescribeAlgorithm where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeAlgorithm" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAlgorithm where
        toJSON DescribeAlgorithm'{..}
          = object
              (catMaybes
                 [Just ("AlgorithmName" .= _dAlgorithmName)])

instance ToPath DescribeAlgorithm where
        toPath = const "/"

instance ToQuery DescribeAlgorithm where
        toQuery = const mempty

-- | /See:/ 'describeAlgorithmResponse' smart constructor.
data DescribeAlgorithmResponse = DescribeAlgorithmResponse'
  { _darsValidationSpecification :: !(Maybe AlgorithmValidationSpecification)
  , _darsInferenceSpecification  :: !(Maybe InferenceSpecification)
  , _darsAlgorithmDescription    :: !(Maybe Text)
  , _darsCertifyForMarketplace   :: !(Maybe Bool)
  , _darsProductId               :: !(Maybe Text)
  , _darsResponseStatus          :: !Int
  , _darsAlgorithmName           :: !Text
  , _darsAlgorithmARN            :: !Text
  , _darsCreationTime            :: !POSIX
  , _darsTrainingSpecification   :: !TrainingSpecification
  , _darsAlgorithmStatus         :: !AlgorithmStatus
  , _darsAlgorithmStatusDetails  :: !AlgorithmStatusDetails
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAlgorithmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsValidationSpecification' - Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
-- * 'darsInferenceSpecification' - Details about inference jobs that the algorithm runs.
--
-- * 'darsAlgorithmDescription' - A brief summary about the algorithm.
--
-- * 'darsCertifyForMarketplace' - Whether the algorithm is certified to be listed in AWS Marketplace.
--
-- * 'darsProductId' - The product identifier of the algorithm.
--
-- * 'darsResponseStatus' - -- | The response status code.
--
-- * 'darsAlgorithmName' - The name of the algorithm being described.
--
-- * 'darsAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
--
-- * 'darsCreationTime' - A timestamp specifying when the algorithm was created.
--
-- * 'darsTrainingSpecification' - Details about training jobs run by this algorithm.
--
-- * 'darsAlgorithmStatus' - The current status of the algorithm.
--
-- * 'darsAlgorithmStatusDetails' - Details about the current status of the algorithm.
describeAlgorithmResponse
    :: Int -- ^ 'darsResponseStatus'
    -> Text -- ^ 'darsAlgorithmName'
    -> Text -- ^ 'darsAlgorithmARN'
    -> UTCTime -- ^ 'darsCreationTime'
    -> TrainingSpecification -- ^ 'darsTrainingSpecification'
    -> AlgorithmStatus -- ^ 'darsAlgorithmStatus'
    -> AlgorithmStatusDetails -- ^ 'darsAlgorithmStatusDetails'
    -> DescribeAlgorithmResponse
describeAlgorithmResponse pResponseStatus_ pAlgorithmName_ pAlgorithmARN_ pCreationTime_ pTrainingSpecification_ pAlgorithmStatus_ pAlgorithmStatusDetails_ =
  DescribeAlgorithmResponse'
    { _darsValidationSpecification = Nothing
    , _darsInferenceSpecification = Nothing
    , _darsAlgorithmDescription = Nothing
    , _darsCertifyForMarketplace = Nothing
    , _darsProductId = Nothing
    , _darsResponseStatus = pResponseStatus_
    , _darsAlgorithmName = pAlgorithmName_
    , _darsAlgorithmARN = pAlgorithmARN_
    , _darsCreationTime = _Time # pCreationTime_
    , _darsTrainingSpecification = pTrainingSpecification_
    , _darsAlgorithmStatus = pAlgorithmStatus_
    , _darsAlgorithmStatusDetails = pAlgorithmStatusDetails_
    }


-- | Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
darsValidationSpecification :: Lens' DescribeAlgorithmResponse (Maybe AlgorithmValidationSpecification)
darsValidationSpecification = lens _darsValidationSpecification (\ s a -> s{_darsValidationSpecification = a})

-- | Details about inference jobs that the algorithm runs.
darsInferenceSpecification :: Lens' DescribeAlgorithmResponse (Maybe InferenceSpecification)
darsInferenceSpecification = lens _darsInferenceSpecification (\ s a -> s{_darsInferenceSpecification = a})

-- | A brief summary about the algorithm.
darsAlgorithmDescription :: Lens' DescribeAlgorithmResponse (Maybe Text)
darsAlgorithmDescription = lens _darsAlgorithmDescription (\ s a -> s{_darsAlgorithmDescription = a})

-- | Whether the algorithm is certified to be listed in AWS Marketplace.
darsCertifyForMarketplace :: Lens' DescribeAlgorithmResponse (Maybe Bool)
darsCertifyForMarketplace = lens _darsCertifyForMarketplace (\ s a -> s{_darsCertifyForMarketplace = a})

-- | The product identifier of the algorithm.
darsProductId :: Lens' DescribeAlgorithmResponse (Maybe Text)
darsProductId = lens _darsProductId (\ s a -> s{_darsProductId = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAlgorithmResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

-- | The name of the algorithm being described.
darsAlgorithmName :: Lens' DescribeAlgorithmResponse Text
darsAlgorithmName = lens _darsAlgorithmName (\ s a -> s{_darsAlgorithmName = a})

-- | The Amazon Resource Name (ARN) of the algorithm.
darsAlgorithmARN :: Lens' DescribeAlgorithmResponse Text
darsAlgorithmARN = lens _darsAlgorithmARN (\ s a -> s{_darsAlgorithmARN = a})

-- | A timestamp specifying when the algorithm was created.
darsCreationTime :: Lens' DescribeAlgorithmResponse UTCTime
darsCreationTime = lens _darsCreationTime (\ s a -> s{_darsCreationTime = a}) . _Time

-- | Details about training jobs run by this algorithm.
darsTrainingSpecification :: Lens' DescribeAlgorithmResponse TrainingSpecification
darsTrainingSpecification = lens _darsTrainingSpecification (\ s a -> s{_darsTrainingSpecification = a})

-- | The current status of the algorithm.
darsAlgorithmStatus :: Lens' DescribeAlgorithmResponse AlgorithmStatus
darsAlgorithmStatus = lens _darsAlgorithmStatus (\ s a -> s{_darsAlgorithmStatus = a})

-- | Details about the current status of the algorithm.
darsAlgorithmStatusDetails :: Lens' DescribeAlgorithmResponse AlgorithmStatusDetails
darsAlgorithmStatusDetails = lens _darsAlgorithmStatusDetails (\ s a -> s{_darsAlgorithmStatusDetails = a})

instance NFData DescribeAlgorithmResponse where
