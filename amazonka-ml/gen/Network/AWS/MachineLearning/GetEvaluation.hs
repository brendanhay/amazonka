{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.GetEvaluation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns an @Evaluation@ that includes metadata as well as the current
-- status of the @Evaluation@.
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
    , gerPerformanceMetrics
    , gerLastUpdatedAt
    , gerCreatedAt
    , gerInputDataLocationS3
    , gerMLModelId
    , gerName
    , gerCreatedByIAMUser
    , gerLogURI
    , gerMessage
    , gerEvaluationId
    , gerEvaluationDataSourceId
    , gerStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getEvaluation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'geEvaluationId'
newtype GetEvaluation = GetEvaluation'
    { _geEvaluationId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetEvaluation' smart constructor.
getEvaluation :: Text -> GetEvaluation
getEvaluation pEvaluationId =
    GetEvaluation'
    { _geEvaluationId = pEvaluationId
    }

-- | The ID of the @Evaluation@ to retrieve. The evaluation of each @MLModel@
-- is recorded and cataloged. The ID provides the means to access the
-- information.
geEvaluationId :: Lens' GetEvaluation Text
geEvaluationId = lens _geEvaluationId (\ s a -> s{_geEvaluationId = a});

instance AWSRequest GetEvaluation where
        type Sv GetEvaluation = MachineLearning
        type Rs GetEvaluation = GetEvaluationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetEvaluationResponse' <$>
                   (x .?> "PerformanceMetrics") <*>
                     (x .?> "LastUpdatedAt")
                     <*> (x .?> "CreatedAt")
                     <*> (x .?> "InputDataLocationS3")
                     <*> (x .?> "MLModelId")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreatedByIamUser")
                     <*> (x .?> "LogUri")
                     <*> (x .?> "Message")
                     <*> (x .?> "EvaluationId")
                     <*> (x .?> "EvaluationDataSourceId")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetEvaluation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.GetEvaluation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetEvaluation where
        toJSON GetEvaluation'{..}
          = object ["EvaluationId" .= _geEvaluationId]

instance ToPath GetEvaluation where
        toPath = const "/"

instance ToQuery GetEvaluation where
        toQuery = const mempty

-- | Represents the output of a GetEvaluation operation and describes an
-- @Evaluation@.
--
-- /See:/ 'getEvaluationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gerPerformanceMetrics'
--
-- * 'gerLastUpdatedAt'
--
-- * 'gerCreatedAt'
--
-- * 'gerInputDataLocationS3'
--
-- * 'gerMLModelId'
--
-- * 'gerName'
--
-- * 'gerCreatedByIAMUser'
--
-- * 'gerLogURI'
--
-- * 'gerMessage'
--
-- * 'gerEvaluationId'
--
-- * 'gerEvaluationDataSourceId'
--
-- * 'gerStatus'
data GetEvaluationResponse = GetEvaluationResponse'
    { _gerPerformanceMetrics     :: !(Maybe PerformanceMetrics)
    , _gerLastUpdatedAt          :: !(Maybe POSIX)
    , _gerCreatedAt              :: !(Maybe POSIX)
    , _gerInputDataLocationS3    :: !(Maybe Text)
    , _gerMLModelId              :: !(Maybe Text)
    , _gerName                   :: !(Maybe Text)
    , _gerCreatedByIAMUser       :: !(Maybe Text)
    , _gerLogURI                 :: !(Maybe Text)
    , _gerMessage                :: !(Maybe Text)
    , _gerEvaluationId           :: !(Maybe Text)
    , _gerEvaluationDataSourceId :: !(Maybe Text)
    , _gerStatus                 :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetEvaluationResponse' smart constructor.
getEvaluationResponse :: Int -> GetEvaluationResponse
getEvaluationResponse pStatus =
    GetEvaluationResponse'
    { _gerPerformanceMetrics = Nothing
    , _gerLastUpdatedAt = Nothing
    , _gerCreatedAt = Nothing
    , _gerInputDataLocationS3 = Nothing
    , _gerMLModelId = Nothing
    , _gerName = Nothing
    , _gerCreatedByIAMUser = Nothing
    , _gerLogURI = Nothing
    , _gerMessage = Nothing
    , _gerEvaluationId = Nothing
    , _gerEvaluationDataSourceId = Nothing
    , _gerStatus = pStatus
    }

-- | Measurements of how well the @MLModel@ performed using observations
-- referenced by the @DataSource@. One of the following metric is returned
-- based on the type of the @MLModel@:
--
-- -   BinaryAUC: A binary @MLModel@ uses the Area Under the Curve (AUC)
--     technique to measure performance.
--
-- -   RegressionRMSE: A regression @MLModel@ uses the Root Mean Square
--     Error (RMSE) technique to measure performance. RMSE measures the
--     difference between predicted and actual values for a single
--     variable.
--
-- -   MulticlassAvgFScore: A multiclass @MLModel@ uses the F1 score
--     technique to measure performance.
--
-- For more information about performance metrics, please see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
gerPerformanceMetrics :: Lens' GetEvaluationResponse (Maybe PerformanceMetrics)
gerPerformanceMetrics = lens _gerPerformanceMetrics (\ s a -> s{_gerPerformanceMetrics = a});

-- | The time of the most recent edit to the @BatchPrediction@. The time is
-- expressed in epoch time.
gerLastUpdatedAt :: Lens' GetEvaluationResponse (Maybe UTCTime)
gerLastUpdatedAt = lens _gerLastUpdatedAt (\ s a -> s{_gerLastUpdatedAt = a}) . mapping _Time;

-- | The time that the @Evaluation@ was created. The time is expressed in
-- epoch time.
gerCreatedAt :: Lens' GetEvaluationResponse (Maybe UTCTime)
gerCreatedAt = lens _gerCreatedAt (\ s a -> s{_gerCreatedAt = a}) . mapping _Time;

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gerInputDataLocationS3 :: Lens' GetEvaluationResponse (Maybe Text)
gerInputDataLocationS3 = lens _gerInputDataLocationS3 (\ s a -> s{_gerInputDataLocationS3 = a});

-- | The ID of the @MLModel@ that was the focus of the evaluation.
gerMLModelId :: Lens' GetEvaluationResponse (Maybe Text)
gerMLModelId = lens _gerMLModelId (\ s a -> s{_gerMLModelId = a});

-- | A user-supplied name or description of the @Evaluation@.
gerName :: Lens' GetEvaluationResponse (Maybe Text)
gerName = lens _gerName (\ s a -> s{_gerName = a});

-- | The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
gerCreatedByIAMUser :: Lens' GetEvaluationResponse (Maybe Text)
gerCreatedByIAMUser = lens _gerCreatedByIAMUser (\ s a -> s{_gerCreatedByIAMUser = a});

-- | A link to the file that contains logs of the CreateEvaluation operation.
gerLogURI :: Lens' GetEvaluationResponse (Maybe Text)
gerLogURI = lens _gerLogURI (\ s a -> s{_gerLogURI = a});

-- | A description of the most recent details about evaluating the @MLModel@.
gerMessage :: Lens' GetEvaluationResponse (Maybe Text)
gerMessage = lens _gerMessage (\ s a -> s{_gerMessage = a});

-- | The evaluation ID which is same as the @EvaluationId@ in the request.
gerEvaluationId :: Lens' GetEvaluationResponse (Maybe Text)
gerEvaluationId = lens _gerEvaluationId (\ s a -> s{_gerEvaluationId = a});

-- | The @DataSource@ used for this evaluation.
gerEvaluationDataSourceId :: Lens' GetEvaluationResponse (Maybe Text)
gerEvaluationDataSourceId = lens _gerEvaluationDataSourceId (\ s a -> s{_gerEvaluationDataSourceId = a});

-- | FIXME: Undocumented member.
gerStatus :: Lens' GetEvaluationResponse Int
gerStatus = lens _gerStatus (\ s a -> s{_gerStatus = a});
