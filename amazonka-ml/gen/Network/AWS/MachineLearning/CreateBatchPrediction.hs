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
-- Module      : Network.AWS.MachineLearning.CreateBatchPrediction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates predictions for a group of observations. The observations to process exist in one or more data files referenced by a @DataSource@ . This operation creates a new @BatchPrediction@ , and uses an @MLModel@ and the data files referenced by the @DataSource@ as information sources.
--
--
-- @CreateBatchPrediction@ is an asynchronous operation. In response to @CreateBatchPrediction@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @BatchPrediction@ status to @PENDING@ . After the @BatchPrediction@ completes, Amazon ML sets the status to @COMPLETED@ .
--
-- You can poll for status updates by using the 'GetBatchPrediction' operation and checking the @Status@ parameter of the result. After the @COMPLETED@ status appears, the results are available in the location specified by the @OutputUri@ parameter.
--
module Network.AWS.MachineLearning.CreateBatchPrediction
    (
    -- * Creating a Request
      createBatchPrediction
    , CreateBatchPrediction
    -- * Request Lenses
    , cbpBatchPredictionName
    , cbpBatchPredictionId
    , cbpMLModelId
    , cbpBatchPredictionDataSourceId
    , cbpOutputURI

    -- * Destructuring the Response
    , createBatchPredictionResponse
    , CreateBatchPredictionResponse
    -- * Response Lenses
    , cbprsBatchPredictionId
    , cbprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBatchPrediction' smart constructor.
data CreateBatchPrediction = CreateBatchPrediction'
  { _cbpBatchPredictionName         :: !(Maybe Text)
  , _cbpBatchPredictionId           :: !Text
  , _cbpMLModelId                   :: !Text
  , _cbpBatchPredictionDataSourceId :: !Text
  , _cbpOutputURI                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBatchPrediction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbpBatchPredictionName' - A user-supplied name or description of the @BatchPrediction@ . @BatchPredictionName@ can only use the UTF-8 character set.
--
-- * 'cbpBatchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ .
--
-- * 'cbpMLModelId' - The ID of the @MLModel@ that will generate predictions for the group of observations.
--
-- * 'cbpBatchPredictionDataSourceId' - The ID of the @DataSource@ that points to the group of observations to predict.
--
-- * 'cbpOutputURI' - The location of an Amazon Simple Storage Service (Amazon S3) bucket or directory to store the batch prediction results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'. Amazon ML needs permissions to store and retrieve the logs on your behalf. For information about how to set permissions, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
createBatchPrediction
    :: Text -- ^ 'cbpBatchPredictionId'
    -> Text -- ^ 'cbpMLModelId'
    -> Text -- ^ 'cbpBatchPredictionDataSourceId'
    -> Text -- ^ 'cbpOutputURI'
    -> CreateBatchPrediction
createBatchPrediction pBatchPredictionId_ pMLModelId_ pBatchPredictionDataSourceId_ pOutputURI_ =
  CreateBatchPrediction'
    { _cbpBatchPredictionName = Nothing
    , _cbpBatchPredictionId = pBatchPredictionId_
    , _cbpMLModelId = pMLModelId_
    , _cbpBatchPredictionDataSourceId = pBatchPredictionDataSourceId_
    , _cbpOutputURI = pOutputURI_
    }


-- | A user-supplied name or description of the @BatchPrediction@ . @BatchPredictionName@ can only use the UTF-8 character set.
cbpBatchPredictionName :: Lens' CreateBatchPrediction (Maybe Text)
cbpBatchPredictionName = lens _cbpBatchPredictionName (\ s a -> s{_cbpBatchPredictionName = a})

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ .
cbpBatchPredictionId :: Lens' CreateBatchPrediction Text
cbpBatchPredictionId = lens _cbpBatchPredictionId (\ s a -> s{_cbpBatchPredictionId = a})

-- | The ID of the @MLModel@ that will generate predictions for the group of observations.
cbpMLModelId :: Lens' CreateBatchPrediction Text
cbpMLModelId = lens _cbpMLModelId (\ s a -> s{_cbpMLModelId = a})

-- | The ID of the @DataSource@ that points to the group of observations to predict.
cbpBatchPredictionDataSourceId :: Lens' CreateBatchPrediction Text
cbpBatchPredictionDataSourceId = lens _cbpBatchPredictionDataSourceId (\ s a -> s{_cbpBatchPredictionDataSourceId = a})

-- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or directory to store the batch prediction results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'. Amazon ML needs permissions to store and retrieve the logs on your behalf. For information about how to set permissions, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
cbpOutputURI :: Lens' CreateBatchPrediction Text
cbpOutputURI = lens _cbpOutputURI (\ s a -> s{_cbpOutputURI = a})

instance AWSRequest CreateBatchPrediction where
        type Rs CreateBatchPrediction =
             CreateBatchPredictionResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 CreateBatchPredictionResponse' <$>
                   (x .?> "BatchPredictionId") <*> (pure (fromEnum s)))

instance Hashable CreateBatchPrediction where

instance NFData CreateBatchPrediction where

instance ToHeaders CreateBatchPrediction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateBatchPrediction" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBatchPrediction where
        toJSON CreateBatchPrediction'{..}
          = object
              (catMaybes
                 [("BatchPredictionName" .=) <$>
                    _cbpBatchPredictionName,
                  Just ("BatchPredictionId" .= _cbpBatchPredictionId),
                  Just ("MLModelId" .= _cbpMLModelId),
                  Just
                    ("BatchPredictionDataSourceId" .=
                       _cbpBatchPredictionDataSourceId),
                  Just ("OutputUri" .= _cbpOutputURI)])

instance ToPath CreateBatchPrediction where
        toPath = const "/"

instance ToQuery CreateBatchPrediction where
        toQuery = const mempty

-- | Represents the output of a @CreateBatchPrediction@ operation, and is an acknowledgement that Amazon ML received the request.
--
--
-- The @CreateBatchPrediction@ operation is asynchronous. You can poll for status updates by using the @>GetBatchPrediction@ operation and checking the @Status@ parameter of the result.
--
--
-- /See:/ 'createBatchPredictionResponse' smart constructor.
data CreateBatchPredictionResponse = CreateBatchPredictionResponse'
  { _cbprsBatchPredictionId :: !(Maybe Text)
  , _cbprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBatchPredictionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbprsBatchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value is identical to the value of the @BatchPredictionId@ in the request.
--
-- * 'cbprsResponseStatus' - -- | The response status code.
createBatchPredictionResponse
    :: Int -- ^ 'cbprsResponseStatus'
    -> CreateBatchPredictionResponse
createBatchPredictionResponse pResponseStatus_ =
  CreateBatchPredictionResponse'
    {_cbprsBatchPredictionId = Nothing, _cbprsResponseStatus = pResponseStatus_}


-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value is identical to the value of the @BatchPredictionId@ in the request.
cbprsBatchPredictionId :: Lens' CreateBatchPredictionResponse (Maybe Text)
cbprsBatchPredictionId = lens _cbprsBatchPredictionId (\ s a -> s{_cbprsBatchPredictionId = a})

-- | -- | The response status code.
cbprsResponseStatus :: Lens' CreateBatchPredictionResponse Int
cbprsResponseStatus = lens _cbprsResponseStatus (\ s a -> s{_cbprsResponseStatus = a})

instance NFData CreateBatchPredictionResponse where
