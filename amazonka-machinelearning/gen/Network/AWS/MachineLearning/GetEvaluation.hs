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

-- Module      : Network.AWS.MachineLearning.GetEvaluation
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

-- | Returns an 'Evaluation' that includes metadata as well as the current status of
-- the 'Evaluation'.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetEvaluation.html>
module Network.AWS.MachineLearning.GetEvaluation
    (
    -- * Request
      GetEvaluation
    -- ** Request constructor
    , getEvaluation
    -- ** Request lenses
    , geEvaluationId

    -- * Response
    , GetEvaluationResponse
    -- ** Response constructor
    , getEvaluationResponse
    -- ** Response lenses
    , gerCreatedAt
    , gerCreatedByIamUser
    , gerEvaluationDataSourceId
    , gerEvaluationId
    , gerInputDataLocationS3
    , gerLastUpdatedAt
    , gerLogUri
    , gerMLModelId
    , gerMessage
    , gerName
    , gerPerformanceMetrics
    , gerStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype GetEvaluation = GetEvaluation
    { _geEvaluationId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetEvaluation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'geEvaluationId' @::@ 'Text'
--
getEvaluation :: Text -- ^ 'geEvaluationId'
              -> GetEvaluation
getEvaluation p1 = GetEvaluation
    { _geEvaluationId = p1
    }

-- | The ID of the 'Evaluation' to retrieve. The evaluation of each 'MLModel' is
-- recorded and cataloged. The ID provides the means to access the information.
geEvaluationId :: Lens' GetEvaluation Text
geEvaluationId = lens _geEvaluationId (\s a -> s { _geEvaluationId = a })

data GetEvaluationResponse = GetEvaluationResponse
    { _gerCreatedAt              :: Maybe POSIX
    , _gerCreatedByIamUser       :: Maybe Text
    , _gerEvaluationDataSourceId :: Maybe Text
    , _gerEvaluationId           :: Maybe Text
    , _gerInputDataLocationS3    :: Maybe Text
    , _gerLastUpdatedAt          :: Maybe POSIX
    , _gerLogUri                 :: Maybe Text
    , _gerMLModelId              :: Maybe Text
    , _gerMessage                :: Maybe Text
    , _gerName                   :: Maybe Text
    , _gerPerformanceMetrics     :: Maybe PerformanceMetrics
    , _gerStatus                 :: Maybe EntityStatus
    } deriving (Eq, Read, Show)

-- | 'GetEvaluationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gerCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gerCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'gerEvaluationDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'gerEvaluationId' @::@ 'Maybe' 'Text'
--
-- * 'gerInputDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'gerLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gerLogUri' @::@ 'Maybe' 'Text'
--
-- * 'gerMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'gerMessage' @::@ 'Maybe' 'Text'
--
-- * 'gerName' @::@ 'Maybe' 'Text'
--
-- * 'gerPerformanceMetrics' @::@ 'Maybe' 'PerformanceMetrics'
--
-- * 'gerStatus' @::@ 'Maybe' 'EntityStatus'
--
getEvaluationResponse :: GetEvaluationResponse
getEvaluationResponse = GetEvaluationResponse
    { _gerEvaluationId           = Nothing
    , _gerMLModelId              = Nothing
    , _gerEvaluationDataSourceId = Nothing
    , _gerInputDataLocationS3    = Nothing
    , _gerCreatedByIamUser       = Nothing
    , _gerCreatedAt              = Nothing
    , _gerLastUpdatedAt          = Nothing
    , _gerName                   = Nothing
    , _gerStatus                 = Nothing
    , _gerPerformanceMetrics     = Nothing
    , _gerLogUri                 = Nothing
    , _gerMessage                = Nothing
    }

-- | The time that the 'Evaluation' was created. The time is expressed in epoch time.
gerCreatedAt :: Lens' GetEvaluationResponse (Maybe UTCTime)
gerCreatedAt = lens _gerCreatedAt (\s a -> s { _gerCreatedAt = a }) . mapping _Time

-- | The AWS user account that invoked the evaluation. The account type can be
-- either an AWS root account or an AWS Identity and Access Management (IAM)
-- user account.
gerCreatedByIamUser :: Lens' GetEvaluationResponse (Maybe Text)
gerCreatedByIamUser =
    lens _gerCreatedByIamUser (\s a -> s { _gerCreatedByIamUser = a })

-- | The 'DataSource' used for this evaluation.
gerEvaluationDataSourceId :: Lens' GetEvaluationResponse (Maybe Text)
gerEvaluationDataSourceId =
    lens _gerEvaluationDataSourceId
        (\s a -> s { _gerEvaluationDataSourceId = a })

-- | The evaluation ID which is same as the 'EvaluationId' in the request.
gerEvaluationId :: Lens' GetEvaluationResponse (Maybe Text)
gerEvaluationId = lens _gerEvaluationId (\s a -> s { _gerEvaluationId = a })

-- | The location of the data file or directory in Amazon Simple Storage Service
-- (Amazon S3).
gerInputDataLocationS3 :: Lens' GetEvaluationResponse (Maybe Text)
gerInputDataLocationS3 =
    lens _gerInputDataLocationS3 (\s a -> s { _gerInputDataLocationS3 = a })

-- | The time of the most recent edit to the 'BatchPrediction'. The time is
-- expressed in epoch time.
gerLastUpdatedAt :: Lens' GetEvaluationResponse (Maybe UTCTime)
gerLastUpdatedAt = lens _gerLastUpdatedAt (\s a -> s { _gerLastUpdatedAt = a }) . mapping _Time

-- | A link to the file that contains logs of the 'CreateEvaluation' operation.
gerLogUri :: Lens' GetEvaluationResponse (Maybe Text)
gerLogUri = lens _gerLogUri (\s a -> s { _gerLogUri = a })

-- | The ID of the 'MLModel' that was the focus of the evaluation.
gerMLModelId :: Lens' GetEvaluationResponse (Maybe Text)
gerMLModelId = lens _gerMLModelId (\s a -> s { _gerMLModelId = a })

-- | A description of the most recent details about evaluating the 'MLModel'.
gerMessage :: Lens' GetEvaluationResponse (Maybe Text)
gerMessage = lens _gerMessage (\s a -> s { _gerMessage = a })

-- | A user-supplied name or description of the 'Evaluation'.
gerName :: Lens' GetEvaluationResponse (Maybe Text)
gerName = lens _gerName (\s a -> s { _gerName = a })

-- | Measurements of how well the 'MLModel' performed using observations referenced
-- by the 'DataSource'. One of the following metric is returned based on the type
-- of the 'MLModel':
--
-- BinaryAUC: A binary 'MLModel' uses the Area Under the Curve (AUC) technique
-- to measure performance.
--
-- RegressionRMSE: A regression 'MLModel' uses the Root Mean Square Error
-- (RMSE) technique to measure performance. RMSE measures the difference between
-- predicted and actual values for a single variable.
--
-- MulticlassAvgFScore: A multiclass 'MLModel' uses the F1 score technique to
-- measure performance.
--
-- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg AmazonMachine Learning Developer Guide>.
gerPerformanceMetrics :: Lens' GetEvaluationResponse (Maybe PerformanceMetrics)
gerPerformanceMetrics =
    lens _gerPerformanceMetrics (\s a -> s { _gerPerformanceMetrics = a })

-- | The status of the evaluation. This element can have one of the following
-- values:
--
-- 'PENDING' - Amazon Machine Language (Amazon ML) submitted a request to
-- evaluate an 'MLModel'.  'INPROGRESS' - The evaluation is underway.  'FAILED' - The
-- request to evaluate an 'MLModel' did not run to completion. It is not usable.  'COMPLETED' - The evaluation process completed successfully.  'DELETED' - The 'Evaluation'
-- is marked as deleted. It is not usable.
gerStatus :: Lens' GetEvaluationResponse (Maybe EntityStatus)
gerStatus = lens _gerStatus (\s a -> s { _gerStatus = a })

instance ToPath GetEvaluation where
    toPath = const "/"

instance ToQuery GetEvaluation where
    toQuery = const mempty

instance ToHeaders GetEvaluation

instance ToJSON GetEvaluation where
    toJSON GetEvaluation{..} = object
        [ "EvaluationId" .= _geEvaluationId
        ]

instance AWSRequest GetEvaluation where
    type Sv GetEvaluation = MachineLearning
    type Rs GetEvaluation = GetEvaluationResponse

    request  = post "GetEvaluation"
    response = jsonResponse

instance FromJSON GetEvaluationResponse where
    parseJSON = withObject "GetEvaluationResponse" $ \o -> GetEvaluationResponse
        <$> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "EvaluationDataSourceId"
        <*> o .:? "EvaluationId"
        <*> o .:? "InputDataLocationS3"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "LogUri"
        <*> o .:? "MLModelId"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "PerformanceMetrics"
        <*> o .:? "Status"
