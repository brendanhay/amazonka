{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateBatchPrediction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generates predictions for a group of observations. The observations to
-- process exist in one or more data files referenced by a @DataSource@.
-- This operation creates a new @BatchPrediction@, and uses an @MLModel@
-- and the data files referenced by the @DataSource@ as information
-- sources.
--
-- @CreateBatchPrediction@ is an asynchronous operation. In response to
-- @CreateBatchPrediction@, Amazon Machine Learning (Amazon ML) immediately
-- returns and sets the @BatchPrediction@ status to @PENDING@. After the
-- @BatchPrediction@ completes, Amazon ML sets the status to @COMPLETED@.
--
-- You can poll for status updates by using the GetBatchPrediction
-- operation and checking the @Status@ parameter of the result. After the
-- @COMPLETED@ status appears, the results are available in the location
-- specified by the @OutputUri@ parameter.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateBatchPrediction.html>
module Network.AWS.MachineLearning.CreateBatchPrediction
    (
    -- * Request
      CreateBatchPrediction
    -- ** Request constructor
    , createBatchPrediction
    -- ** Request lenses
    , cbprqBatchPredictionName
    , cbprqBatchPredictionId
    , cbprqMLModelId
    , cbprqBatchPredictionDataSourceId
    , cbprqOutputURI

    -- * Response
    , CreateBatchPredictionResponse
    -- ** Response constructor
    , createBatchPredictionResponse
    -- ** Response lenses
    , cbprsBatchPredictionId
    , cbprsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createBatchPrediction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbprqBatchPredictionName'
--
-- * 'cbprqBatchPredictionId'
--
-- * 'cbprqMLModelId'
--
-- * 'cbprqBatchPredictionDataSourceId'
--
-- * 'cbprqOutputURI'
data CreateBatchPrediction = CreateBatchPrediction'
    { _cbprqBatchPredictionName         :: !(Maybe Text)
    , _cbprqBatchPredictionId           :: !Text
    , _cbprqMLModelId                   :: !Text
    , _cbprqBatchPredictionDataSourceId :: !Text
    , _cbprqOutputURI                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateBatchPrediction' smart constructor.
createBatchPrediction :: Text -> Text -> Text -> Text -> CreateBatchPrediction
createBatchPrediction pBatchPredictionId_ pMLModelId_ pBatchPredictionDataSourceId_ pOutputURI_ =
    CreateBatchPrediction'
    { _cbprqBatchPredictionName = Nothing
    , _cbprqBatchPredictionId = pBatchPredictionId_
    , _cbprqMLModelId = pMLModelId_
    , _cbprqBatchPredictionDataSourceId = pBatchPredictionDataSourceId_
    , _cbprqOutputURI = pOutputURI_
    }

-- | A user-supplied name or description of the @BatchPrediction@.
-- @BatchPredictionName@ can only use the UTF-8 character set.
cbprqBatchPredictionName :: Lens' CreateBatchPrediction (Maybe Text)
cbprqBatchPredictionName = lens _cbprqBatchPredictionName (\ s a -> s{_cbprqBatchPredictionName = a});

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
cbprqBatchPredictionId :: Lens' CreateBatchPrediction Text
cbprqBatchPredictionId = lens _cbprqBatchPredictionId (\ s a -> s{_cbprqBatchPredictionId = a});

-- | The ID of the @MLModel@ that will generate predictions for the group of
-- observations.
cbprqMLModelId :: Lens' CreateBatchPrediction Text
cbprqMLModelId = lens _cbprqMLModelId (\ s a -> s{_cbprqMLModelId = a});

-- | The ID of the @DataSource@ that points to the group of observations to
-- predict.
cbprqBatchPredictionDataSourceId :: Lens' CreateBatchPrediction Text
cbprqBatchPredictionDataSourceId = lens _cbprqBatchPredictionDataSourceId (\ s a -> s{_cbprqBatchPredictionDataSourceId = a});

-- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or
-- directory to store the batch prediction results. The following
-- substrings are not allowed in the s3 key portion of the \"outputURI\"
-- field: \':\', \'\/\/\', \'\/.\/\', \'\/..\/\'.
--
-- Amazon ML needs permissions to store and retrieve the logs on your
-- behalf. For information about how to set permissions, see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
cbprqOutputURI :: Lens' CreateBatchPrediction Text
cbprqOutputURI = lens _cbprqOutputURI (\ s a -> s{_cbprqOutputURI = a});

instance AWSRequest CreateBatchPrediction where
        type Sv CreateBatchPrediction = MachineLearning
        type Rs CreateBatchPrediction =
             CreateBatchPredictionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateBatchPredictionResponse' <$>
                   (x .?> "BatchPredictionId") <*> (pure (fromEnum s)))

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
              ["BatchPredictionName" .= _cbprqBatchPredictionName,
               "BatchPredictionId" .= _cbprqBatchPredictionId,
               "MLModelId" .= _cbprqMLModelId,
               "BatchPredictionDataSourceId" .=
                 _cbprqBatchPredictionDataSourceId,
               "OutputUri" .= _cbprqOutputURI]

instance ToPath CreateBatchPrediction where
        toPath = const "/"

instance ToQuery CreateBatchPrediction where
        toQuery = const mempty

-- | Represents the output of a CreateBatchPrediction operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- The CreateBatchPrediction operation is asynchronous. You can poll for
-- status updates by using the GetBatchPrediction operation and checking
-- the @Status@ parameter of the result.
--
-- /See:/ 'createBatchPredictionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbprsBatchPredictionId'
--
-- * 'cbprsStatus'
data CreateBatchPredictionResponse = CreateBatchPredictionResponse'
    { _cbprsBatchPredictionId :: !(Maybe Text)
    , _cbprsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateBatchPredictionResponse' smart constructor.
createBatchPredictionResponse :: Int -> CreateBatchPredictionResponse
createBatchPredictionResponse pStatus_ =
    CreateBatchPredictionResponse'
    { _cbprsBatchPredictionId = Nothing
    , _cbprsStatus = pStatus_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value is identical to the value of the @BatchPredictionId@ in the
-- request.
cbprsBatchPredictionId :: Lens' CreateBatchPredictionResponse (Maybe Text)
cbprsBatchPredictionId = lens _cbprsBatchPredictionId (\ s a -> s{_cbprsBatchPredictionId = a});

-- | FIXME: Undocumented member.
cbprsStatus :: Lens' CreateBatchPredictionResponse Int
cbprsStatus = lens _cbprsStatus (\ s a -> s{_cbprsStatus = a});
