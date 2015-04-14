{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.GetBatchPrediction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a 'BatchPrediction' that includes detailed metadata, status, and data
-- file information for a 'Batch Prediction' request.
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
    , gbprBatchPredictionDataSourceId
    , gbprBatchPredictionId
    , gbprCreatedAt
    , gbprCreatedByIamUser
    , gbprInputDataLocationS3
    , gbprLastUpdatedAt
    , gbprLogUri
    , gbprMLModelId
    , gbprMessage
    , gbprName
    , gbprOutputUri
    , gbprStatus
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype GetBatchPrediction = GetBatchPrediction
    { _gbpBatchPredictionId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetBatchPrediction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpBatchPredictionId' @::@ 'Text'
--
getBatchPrediction :: Text -- ^ 'gbpBatchPredictionId'
                   -> GetBatchPrediction
getBatchPrediction p1 = GetBatchPrediction
    { _gbpBatchPredictionId = p1
    }

-- | An ID assigned to the 'BatchPrediction' at creation.
gbpBatchPredictionId :: Lens' GetBatchPrediction Text
gbpBatchPredictionId =
    lens _gbpBatchPredictionId (\s a -> s { _gbpBatchPredictionId = a })

data GetBatchPredictionResponse = GetBatchPredictionResponse
    { _gbprBatchPredictionDataSourceId :: Maybe Text
    , _gbprBatchPredictionId           :: Maybe Text
    , _gbprCreatedAt                   :: Maybe POSIX
    , _gbprCreatedByIamUser            :: Maybe Text
    , _gbprInputDataLocationS3         :: Maybe Text
    , _gbprLastUpdatedAt               :: Maybe POSIX
    , _gbprLogUri                      :: Maybe Text
    , _gbprMLModelId                   :: Maybe Text
    , _gbprMessage                     :: Maybe Text
    , _gbprName                        :: Maybe Text
    , _gbprOutputUri                   :: Maybe Text
    , _gbprStatus                      :: Maybe EntityStatus
    } deriving (Eq, Read, Show)

-- | 'GetBatchPredictionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbprBatchPredictionDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'gbprBatchPredictionId' @::@ 'Maybe' 'Text'
--
-- * 'gbprCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gbprCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'gbprInputDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'gbprLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gbprLogUri' @::@ 'Maybe' 'Text'
--
-- * 'gbprMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'gbprMessage' @::@ 'Maybe' 'Text'
--
-- * 'gbprName' @::@ 'Maybe' 'Text'
--
-- * 'gbprOutputUri' @::@ 'Maybe' 'Text'
--
-- * 'gbprStatus' @::@ 'Maybe' 'EntityStatus'
--
getBatchPredictionResponse :: GetBatchPredictionResponse
getBatchPredictionResponse = GetBatchPredictionResponse
    { _gbprBatchPredictionId           = Nothing
    , _gbprMLModelId                   = Nothing
    , _gbprBatchPredictionDataSourceId = Nothing
    , _gbprInputDataLocationS3         = Nothing
    , _gbprCreatedByIamUser            = Nothing
    , _gbprCreatedAt                   = Nothing
    , _gbprLastUpdatedAt               = Nothing
    , _gbprName                        = Nothing
    , _gbprStatus                      = Nothing
    , _gbprOutputUri                   = Nothing
    , _gbprLogUri                      = Nothing
    , _gbprMessage                     = Nothing
    }

-- | The ID of the 'DataSource' that was used to create the 'BatchPrediction'.
gbprBatchPredictionDataSourceId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprBatchPredictionDataSourceId =
    lens _gbprBatchPredictionDataSourceId
        (\s a -> s { _gbprBatchPredictionDataSourceId = a })

-- | An ID assigned to the 'BatchPrediction' at creation. This value should be
-- identical to the value of the 'BatchPredictionID' in the request.
gbprBatchPredictionId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprBatchPredictionId =
    lens _gbprBatchPredictionId (\s a -> s { _gbprBatchPredictionId = a })

-- | The time when the 'BatchPrediction' was created. The time is expressed in epoch
-- time.
gbprCreatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprCreatedAt = lens _gbprCreatedAt (\s a -> s { _gbprCreatedAt = a }) . mapping _Time

-- | The AWS user account that invoked the 'BatchPrediction'. The account type can
-- be either an AWS root account or an AWS Identity and Access Management (IAM)
-- user account.
gbprCreatedByIamUser :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprCreatedByIamUser =
    lens _gbprCreatedByIamUser (\s a -> s { _gbprCreatedByIamUser = a })

-- | The location of the data file or directory in Amazon Simple Storage Service
-- (Amazon S3).
gbprInputDataLocationS3 :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprInputDataLocationS3 =
    lens _gbprInputDataLocationS3 (\s a -> s { _gbprInputDataLocationS3 = a })

-- | The time of the most recent edit to 'BatchPrediction'. The time is expressed in
-- epoch time.
gbprLastUpdatedAt :: Lens' GetBatchPredictionResponse (Maybe UTCTime)
gbprLastUpdatedAt =
    lens _gbprLastUpdatedAt (\s a -> s { _gbprLastUpdatedAt = a })
        . mapping _Time

-- | A link to the file that contains logs of the 'CreateBatchPrediction' operation.
gbprLogUri :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprLogUri = lens _gbprLogUri (\s a -> s { _gbprLogUri = a })

-- | The ID of the 'MLModel' that generated predictions for the 'BatchPrediction'
-- request.
gbprMLModelId :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprMLModelId = lens _gbprMLModelId (\s a -> s { _gbprMLModelId = a })

-- | A description of the most recent details about processing the batch
-- prediction request.
gbprMessage :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprMessage = lens _gbprMessage (\s a -> s { _gbprMessage = a })

-- | A user-supplied name or description of the 'BatchPrediction'.
gbprName :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprName = lens _gbprName (\s a -> s { _gbprName = a })

-- | The location of an Amazon S3 bucket or directory to receive the operation
-- results.
gbprOutputUri :: Lens' GetBatchPredictionResponse (Maybe Text)
gbprOutputUri = lens _gbprOutputUri (\s a -> s { _gbprOutputUri = a })

-- | The status of the 'BatchPrediction', which can be one of the following values:
--
-- 'PENDING' - Amazon Machine Learning (Amazon ML) submitted a request to
-- generate batch predictions.  'INPROGRESS' - The batch predictions are in
-- progress.  'FAILED' - The request to perform a batch prediction did not run to
-- completion. It is not usable.  'COMPLETED' - The batch prediction process
-- completed successfully.  'DELETED' - The 'BatchPrediction' is marked as deleted.
-- It is not usable.
gbprStatus :: Lens' GetBatchPredictionResponse (Maybe EntityStatus)
gbprStatus = lens _gbprStatus (\s a -> s { _gbprStatus = a })

instance ToPath GetBatchPrediction where
    toPath = const "/"

instance ToQuery GetBatchPrediction where
    toQuery = const mempty

instance ToHeaders GetBatchPrediction

instance ToJSON GetBatchPrediction where
    toJSON GetBatchPrediction{..} = object
        [ "BatchPredictionId" .= _gbpBatchPredictionId
        ]

instance AWSRequest GetBatchPrediction where
    type Sv GetBatchPrediction = MachineLearning
    type Rs GetBatchPrediction = GetBatchPredictionResponse

    request  = post "GetBatchPrediction"
    response = jsonResponse

instance FromJSON GetBatchPredictionResponse where
    parseJSON = withObject "GetBatchPredictionResponse" $ \o -> GetBatchPredictionResponse
        <$> o .:? "BatchPredictionDataSourceId"
        <*> o .:? "BatchPredictionId"
        <*> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "InputDataLocationS3"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "LogUri"
        <*> o .:? "MLModelId"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "OutputUri"
        <*> o .:? "Status"
