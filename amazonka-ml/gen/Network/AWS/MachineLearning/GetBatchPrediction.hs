{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetBatchPrediction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a @BatchPrediction@ that includes detailed metadata, status, and
-- data file information for a @Batch Prediction@ request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetBatchPrediction.html>
module Network.AWS.MachineLearning.GetBatchPrediction
    (
    -- * Request
      GetBatchPrediction
    -- ** Request constructor
    , getBatchPrediction
    -- ** Request lenses
    , gbpBatchPredictionId

    -- * Response
    , GetBatchPredictionResponse
    -- ** Response constructor
    , getBatchPredictionResponse
    -- ** Response lenses
    , gbprsLastUpdatedAt
    , gbprsCreatedAt
    , gbprsInputDataLocationS3
    , gbprsMLModelId
    , gbprsBatchPredictionDataSourceId
    , gbprsBatchPredictionId
    , gbprsName
    , gbprsCreatedByIAMUser
    , gbprsLogURI
    , gbprsMessage
    , gbprsOutputURI
    , gbprsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getBatchPrediction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpBatchPredictionId'
newtype GetBatchPrediction = GetBatchPrediction'
    { _gbpBatchPredictionId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBatchPrediction' smart constructor.
getBatchPrediction :: Text -> GetBatchPrediction
getBatchPrediction pBatchPredictionId_ =
    GetBatchPrediction'
    { _gbpBatchPredictionId = pBatchPredictionId_
    }

-- | An ID assigned to the @BatchPrediction@ at creation.
gbpBatchPredictionId :: Lens' GetBatchPrediction Text
gbpBatchPredictionId = lens _gbpBatchPredictionId (\ s a -> s{_gbpBatchPredictionId = a});

instance AWSRequest GetBatchPrediction where
        type Sv GetBatchPrediction = MachineLearning
        type Rs GetBatchPrediction =
             GetBatchPredictionResponse
        request = postJSON "GetBatchPrediction"
        response
          = receiveJSON
              (\ s h x ->
                 GetBatchPredictionResponse' <$>
                   (x .?> "LastUpdatedAt") <*> (x .?> "CreatedAt") <*>
                     (x .?> "InputDataLocationS3")
                     <*> (x .?> "MLModelId")
                     <*> (x .?> "BatchPredictionDataSourceId")
                     <*> (x .?> "BatchPredictionId")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreatedByIamUser")
                     <*> (x .?> "LogUri")
                     <*> (x .?> "Message")
                     <*> (x .?> "OutputUri")
                     <*> (pure (fromEnum s)))

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
              ["BatchPredictionId" .= _gbpBatchPredictionId]

instance ToPath GetBatchPrediction where
        toPath = const "/"

instance ToQuery GetBatchPrediction where
        toQuery = const mempty

-- | Represents the output of a GetBatchPrediction operation and describes a
-- @BatchPrediction@.
--
-- /See:/ 'getBatchPredictionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
-- * 'gbprsName'
--
-- * 'gbprsCreatedByIAMUser'
--
-- * 'gbprsLogURI'
--
-- * 'gbprsMessage'
--
-- * 'gbprsOutputURI'
--
-- * 'gbprsStatus'
data GetBatchPredictionResponse = GetBatchPredictionResponse'
    { _gbprsLastUpdatedAt               :: !(Maybe POSIX)
    , _gbprsCreatedAt                   :: !(Maybe POSIX)
    , _gbprsInputDataLocationS3         :: !(Maybe Text)
    , _gbprsMLModelId                   :: !(Maybe Text)
    , _gbprsBatchPredictionDataSourceId :: !(Maybe Text)
    , _gbprsBatchPredictionId           :: !(Maybe Text)
    , _gbprsName                        :: !(Maybe Text)
    , _gbprsCreatedByIAMUser            :: !(Maybe Text)
    , _gbprsLogURI                      :: !(Maybe Text)
    , _gbprsMessage                     :: !(Maybe Text)
    , _gbprsOutputURI                   :: !(Maybe Text)
    , _gbprsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBatchPredictionResponse' smart constructor.
getBatchPredictionResponse :: Int -> GetBatchPredictionResponse
getBatchPredictionResponse pStatus_ =
    GetBatchPredictionResponse'
    { _gbprsLastUpdatedAt = Nothing
    , _gbprsCreatedAt = Nothing
    , _gbprsInputDataLocationS3 = Nothing
    , _gbprsMLModelId = Nothing
    , _gbprsBatchPredictionDataSourceId = Nothing
    , _gbprsBatchPredictionId = Nothing
    , _gbprsName = Nothing
    , _gbprsCreatedByIAMUser = Nothing
    , _gbprsLogURI = Nothing
    , _gbprsMessage = Nothing
    , _gbprsOutputURI = Nothing
    , _gbprsStatus = pStatus_
    }

-- | The time of the most recent edit to @BatchPrediction@. The time is
-- expressed in epoch time.
gbprsLastUpdatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprsLastUpdatedAt = lens _gbprsLastUpdatedAt (\ s a -> s{_gbprsLastUpdatedAt = a}) . mapping _Time;

-- | The time when the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
gbprsCreatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprsCreatedAt = lens _gbprsCreatedAt (\ s a -> s{_gbprsCreatedAt = a}) . mapping _Time;

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gbprsInputDataLocationS3 :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsInputDataLocationS3 = lens _gbprsInputDataLocationS3 (\ s a -> s{_gbprsInputDataLocationS3 = a});

-- | The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
gbprsMLModelId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsMLModelId = lens _gbprsMLModelId (\ s a -> s{_gbprsMLModelId = a});

-- | The ID of the @DataSource@ that was used to create the
-- @BatchPrediction@.
gbprsBatchPredictionDataSourceId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsBatchPredictionDataSourceId = lens _gbprsBatchPredictionDataSourceId (\ s a -> s{_gbprsBatchPredictionDataSourceId = a});

-- | An ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
gbprsBatchPredictionId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsBatchPredictionId = lens _gbprsBatchPredictionId (\ s a -> s{_gbprsBatchPredictionId = a});

-- | A user-supplied name or description of the @BatchPrediction@.
gbprsName :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsName = lens _gbprsName (\ s a -> s{_gbprsName = a});

-- | The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
gbprsCreatedByIAMUser :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprsCreatedByIAMUser = lens _gbprsCreatedByIAMUser (\ s a -> s{_gbprsCreatedByIAMUser = a});

-- | A link to the file that contains logs of the CreateBatchPrediction
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

-- | FIXME: Undocumented member.
gbprsStatus :: Lens' GetBatchPredictionResponse Int
gbprsStatus = lens _gbprsStatus (\ s a -> s{_gbprsStatus = a});
