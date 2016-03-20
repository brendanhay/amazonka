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
-- Module      : Network.AWS.MachineLearning.GetBatchPrediction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a 'BatchPrediction' that includes detailed metadata, status, and
-- data file information for a 'Batch Prediction' request.
module Network.AWS.MachineLearning.GetBatchPrediction
    (
    -- * Creating a Request
      getBatchPrediction
    , GetBatchPrediction
    -- * Request Lenses
    , gbpBatchPredictionId

    -- * Destructuring the Response
    , getBatchPredictionResponse
    , GetBatchPredictionResponse
    -- * Response Lenses
    , gbprsStatus
    , gbprsLastUpdatedAt
    , gbprsCreatedAt
    , gbprsInputDataLocationS3
    , gbprsMLModelId
    , gbprsBatchPredictionDataSourceId
    , gbprsBatchPredictionId
    , gbprsCreatedByIAMUser
    , gbprsName
    , gbprsLogURI
    , gbprsMessage
    , gbprsOutputURI
    , gbprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getBatchPrediction' smart constructor.
newtype GetBatchPrediction = GetBatchPrediction'
    { _gbpBatchPredictionId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBatchPrediction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpBatchPredictionId'
getBatchPrediction
    :: Text -- ^ 'gbpBatchPredictionId'
    -> GetBatchPrediction
getBatchPrediction pBatchPredictionId_ =
    GetBatchPrediction'
    { _gbpBatchPredictionId = pBatchPredictionId_
    }

-- | An ID assigned to the 'BatchPrediction' at creation.
gbpBatchPredictionId :: Lens' GetBatchPrediction Text
gbpBatchPredictionId = lens _gbpBatchPredictionId (\ s a -> s{_gbpBatchPredictionId = a});

instance AWSRequest GetBatchPrediction where
        type Rs GetBatchPrediction =
             GetBatchPredictionResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 GetBatchPredictionResponse' <$>
                   (x .?> "Status") <*> (x .?> "LastUpdatedAt") <*>
                     (x .?> "CreatedAt")
                     <*> (x .?> "InputDataLocationS3")
                     <*> (x .?> "MLModelId")
                     <*> (x .?> "BatchPredictionDataSourceId")
                     <*> (x .?> "BatchPredictionId")
                     <*> (x .?> "CreatedByIamUser")
                     <*> (x .?> "Name")
                     <*> (x .?> "LogUri")
                     <*> (x .?> "Message")
                     <*> (x .?> "OutputUri")
                     <*> (pure (fromEnum s)))

instance Hashable GetBatchPrediction

instance ToHeaders GetBatchPrediction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.GetBatchPrediction" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetBatchPrediction where
        toJSON GetBatchPrediction'{..}
          = object
              (catMaybes
                 [Just
                    ("BatchPredictionId" .= _gbpBatchPredictionId)])

instance ToPath GetBatchPrediction where
        toPath = const "/"

instance ToQuery GetBatchPrediction where
        toQuery = const mempty

-- | Represents the output of a < GetBatchPrediction> operation and describes
-- a 'BatchPrediction'.
--
-- /See:/ 'getBatchPredictionResponse' smart constructor.
data GetBatchPredictionResponse = GetBatchPredictionResponse'
    { _gbprsStatus                      :: !(Maybe EntityStatus)
    , _gbprsLastUpdatedAt               :: !(Maybe POSIX)
    , _gbprsCreatedAt                   :: !(Maybe POSIX)
    , _gbprsInputDataLocationS3         :: !(Maybe Text)
    , _gbprsMLModelId                   :: !(Maybe Text)
    , _gbprsBatchPredictionDataSourceId :: !(Maybe Text)
    , _gbprsBatchPredictionId           :: !(Maybe Text)
    , _gbprsCreatedByIAMUser            :: !(Maybe Text)
    , _gbprsName                        :: !(Maybe Text)
    , _gbprsLogURI                      :: !(Maybe Text)
    , _gbprsMessage                     :: !(Maybe Text)
    , _gbprsOutputURI                   :: !(Maybe Text)
    , _gbprsResponseStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBatchPredictionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbprsStatus'
--
-- * 'gbprsLastUpdatedAt'
--
-- * 'gbprsCreatedAt'
--
-- * 'gbprsInputDataLocationS3'
--
-- * 'gbprsMLModelId'
--
-- * 'gbprsBatchPredictionDataSourceId'
--
-- * 'gbprsBatchPredictionId'
--
-- * 'gbprsCreatedByIAMUser'
--
-- * 'gbprsName'
--
-- * 'gbprsLogURI'
--
-- * 'gbprsMessage'
--
-- * 'gbprsOutputURI'
--
-- * 'gbprsResponseStatus'
getBatchPredictionResponse
    :: Int -- ^ 'gbprsResponseStatus'
    -> GetBatchPredictionResponse
getBatchPredictionResponse pResponseStatus_ =
    GetBatchPredictionResponse'
    { _gbprsStatus = Nothing
    , _gbprsLastUpdatedAt = Nothing
    , _gbprsCreatedAt = Nothing
    , _gbprsInputDataLocationS3 = Nothing
    , _gbprsMLModelId = Nothing
    , _gbprsBatchPredictionDataSourceId = Nothing
    , _gbprsBatchPredictionId = Nothing
    , _gbprsCreatedByIAMUser = Nothing
    , _gbprsName = Nothing
    , _gbprsLogURI = Nothing
    , _gbprsMessage = Nothing
    , _gbprsOutputURI = Nothing
    , _gbprsResponseStatus = pResponseStatus_
    }

-- | The status of the 'BatchPrediction', which can be one of the following
-- values:
--
-- -   'PENDING' - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate batch predictions.
-- -   'INPROGRESS' - The batch predictions are in progress.
-- -   'FAILED' - The request to perform a batch prediction did not run to
--     completion. It is not usable.
-- -   'COMPLETED' - The batch prediction process completed successfully.
-- -   'DELETED' - The 'BatchPrediction' is marked as deleted. It is not
--     usable.
gbprsStatus :: Lens' GetBatchPredictionResponse (Maybe EntityStatus)
gbprsStatus = lens _gbprsStatus (\ s a -> s{_gbprsStatus = a});

-- | The time of the most recent edit to 'BatchPrediction'. The time is
-- expressed in epoch time.
gbprsLastUpdatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprsLastUpdatedAt = lens _gbprsLastUpdatedAt (\ s a -> s{_gbprsLastUpdatedAt = a}) . mapping _Time;

-- | The time when the 'BatchPrediction' was created. The time is expressed
-- in epoch time.
gbprsCreatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprsCreatedAt = lens _gbprsCreatedAt (\ s a -> s{_gbprsCreatedAt = a}) . mapping _Time;

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gbprsInputDataLocationS3 :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsInputDataLocationS3 = lens _gbprsInputDataLocationS3 (\ s a -> s{_gbprsInputDataLocationS3 = a});

-- | The ID of the 'MLModel' that generated predictions for the
-- 'BatchPrediction' request.
gbprsMLModelId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsMLModelId = lens _gbprsMLModelId (\ s a -> s{_gbprsMLModelId = a});

-- | The ID of the 'DataSource' that was used to create the
-- 'BatchPrediction'.
gbprsBatchPredictionDataSourceId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsBatchPredictionDataSourceId = lens _gbprsBatchPredictionDataSourceId (\ s a -> s{_gbprsBatchPredictionDataSourceId = a});

-- | An ID assigned to the 'BatchPrediction' at creation. This value should
-- be identical to the value of the 'BatchPredictionID' in the request.
gbprsBatchPredictionId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsBatchPredictionId = lens _gbprsBatchPredictionId (\ s a -> s{_gbprsBatchPredictionId = a});

-- | The AWS user account that invoked the 'BatchPrediction'. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
gbprsCreatedByIAMUser :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsCreatedByIAMUser = lens _gbprsCreatedByIAMUser (\ s a -> s{_gbprsCreatedByIAMUser = a});

-- | A user-supplied name or description of the 'BatchPrediction'.
gbprsName :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsName = lens _gbprsName (\ s a -> s{_gbprsName = a});

-- | A link to the file that contains logs of the < CreateBatchPrediction>
-- operation.
gbprsLogURI :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsLogURI = lens _gbprsLogURI (\ s a -> s{_gbprsLogURI = a});

-- | A description of the most recent details about processing the batch
-- prediction request.
gbprsMessage :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsMessage = lens _gbprsMessage (\ s a -> s{_gbprsMessage = a});

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results.
gbprsOutputURI :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsOutputURI = lens _gbprsOutputURI (\ s a -> s{_gbprsOutputURI = a});

-- | The response status code.
gbprsResponseStatus :: Lens' GetBatchPredictionResponse Int
gbprsResponseStatus = lens _gbprsResponseStatus (\ s a -> s{_gbprsResponseStatus = a});
