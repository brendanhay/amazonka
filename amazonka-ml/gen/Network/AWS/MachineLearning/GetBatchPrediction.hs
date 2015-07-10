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
    , gbprLastUpdatedAt
    , gbprCreatedAt
    , gbprInputDataLocationS3
    , gbprMLModelId
    , gbprBatchPredictionDataSourceId
    , gbprBatchPredictionId
    , gbprName
    , gbprCreatedByIAMUser
    , gbprLogURI
    , gbprMessage
    , gbprOutputURI
    , gbprStatus
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
getBatchPrediction pBatchPredictionId =
    GetBatchPrediction'
    { _gbpBatchPredictionId = pBatchPredictionId
    }

-- | An ID assigned to the @BatchPrediction@ at creation.
gbpBatchPredictionId :: Lens' GetBatchPrediction Text
gbpBatchPredictionId = lens _gbpBatchPredictionId (\ s a -> s{_gbpBatchPredictionId = a});

instance AWSRequest GetBatchPrediction where
        type Sv GetBatchPrediction = MachineLearning
        type Rs GetBatchPrediction =
             GetBatchPredictionResponse
        request = postJSON
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
-- * 'gbprLastUpdatedAt'
--
-- * 'gbprCreatedAt'
--
-- * 'gbprInputDataLocationS3'
--
-- * 'gbprMLModelId'
--
-- * 'gbprBatchPredictionDataSourceId'
--
-- * 'gbprBatchPredictionId'
--
-- * 'gbprName'
--
-- * 'gbprCreatedByIAMUser'
--
-- * 'gbprLogURI'
--
-- * 'gbprMessage'
--
-- * 'gbprOutputURI'
--
-- * 'gbprStatus'
data GetBatchPredictionResponse = GetBatchPredictionResponse'
    { _gbprLastUpdatedAt               :: !(Maybe POSIX)
    , _gbprCreatedAt                   :: !(Maybe POSIX)
    , _gbprInputDataLocationS3         :: !(Maybe Text)
    , _gbprMLModelId                   :: !(Maybe Text)
    , _gbprBatchPredictionDataSourceId :: !(Maybe Text)
    , _gbprBatchPredictionId           :: !(Maybe Text)
    , _gbprName                        :: !(Maybe Text)
    , _gbprCreatedByIAMUser            :: !(Maybe Text)
    , _gbprLogURI                      :: !(Maybe Text)
    , _gbprMessage                     :: !(Maybe Text)
    , _gbprOutputURI                   :: !(Maybe Text)
    , _gbprStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBatchPredictionResponse' smart constructor.
getBatchPredictionResponse :: Int -> GetBatchPredictionResponse
getBatchPredictionResponse pStatus =
    GetBatchPredictionResponse'
    { _gbprLastUpdatedAt = Nothing
    , _gbprCreatedAt = Nothing
    , _gbprInputDataLocationS3 = Nothing
    , _gbprMLModelId = Nothing
    , _gbprBatchPredictionDataSourceId = Nothing
    , _gbprBatchPredictionId = Nothing
    , _gbprName = Nothing
    , _gbprCreatedByIAMUser = Nothing
    , _gbprLogURI = Nothing
    , _gbprMessage = Nothing
    , _gbprOutputURI = Nothing
    , _gbprStatus = pStatus
    }

-- | The time of the most recent edit to @BatchPrediction@. The time is
-- expressed in epoch time.
gbprLastUpdatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprLastUpdatedAt = lens _gbprLastUpdatedAt (\ s a -> s{_gbprLastUpdatedAt = a}) . mapping _Time;

-- | The time when the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
gbprCreatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprCreatedAt = lens _gbprCreatedAt (\ s a -> s{_gbprCreatedAt = a}) . mapping _Time;

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gbprInputDataLocationS3 :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprInputDataLocationS3 = lens _gbprInputDataLocationS3 (\ s a -> s{_gbprInputDataLocationS3 = a});

-- | The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
gbprMLModelId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprMLModelId = lens _gbprMLModelId (\ s a -> s{_gbprMLModelId = a});

-- | The ID of the @DataSource@ that was used to create the
-- @BatchPrediction@.
gbprBatchPredictionDataSourceId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprBatchPredictionDataSourceId = lens _gbprBatchPredictionDataSourceId (\ s a -> s{_gbprBatchPredictionDataSourceId = a});

-- | An ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
gbprBatchPredictionId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprBatchPredictionId = lens _gbprBatchPredictionId (\ s a -> s{_gbprBatchPredictionId = a});

-- | A user-supplied name or description of the @BatchPrediction@.
gbprName :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprName = lens _gbprName (\ s a -> s{_gbprName = a});

-- | The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
gbprCreatedByIAMUser :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprCreatedByIAMUser = lens _gbprCreatedByIAMUser (\ s a -> s{_gbprCreatedByIAMUser = a});

-- | A link to the file that contains logs of the CreateBatchPrediction
-- operation.
gbprLogURI :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprLogURI = lens _gbprLogURI (\ s a -> s{_gbprLogURI = a});

-- | A description of the most recent details about processing the batch
-- prediction request.
gbprMessage :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprMessage = lens _gbprMessage (\ s a -> s{_gbprMessage = a});

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results.
gbprOutputURI :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprOutputURI = lens _gbprOutputURI (\ s a -> s{_gbprOutputURI = a});

-- | FIXME: Undocumented member.
gbprStatus :: Lens' GetBatchPredictionResponse Int
gbprStatus = lens _gbprStatus (\ s a -> s{_gbprStatus = a});
