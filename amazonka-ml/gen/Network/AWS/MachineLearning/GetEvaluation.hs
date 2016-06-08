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
-- Module      : Network.AWS.MachineLearning.GetEvaluation
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an 'Evaluation' that includes metadata as well as the current status of the 'Evaluation'.
module Network.AWS.MachineLearning.GetEvaluation
    (
    -- * Creating a Request
      getEvaluation
    , GetEvaluation
    -- * Request Lenses
    , geEvaluationId

    -- * Destructuring the Response
    , getEvaluationResponse
    , GetEvaluationResponse
    -- * Response Lenses
    , gersStatus
    , gersPerformanceMetrics
    , gersLastUpdatedAt
    , gersCreatedAt
    , gersInputDataLocationS3
    , gersMLModelId
    , gersCreatedByIAMUser
    , gersName
    , gersLogURI
    , gersEvaluationId
    , gersMessage
    , gersEvaluationDataSourceId
    , gersResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getEvaluation' smart constructor.
newtype GetEvaluation = GetEvaluation'
    { _geEvaluationId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetEvaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geEvaluationId'
getEvaluation
    :: Text -- ^ 'geEvaluationId'
    -> GetEvaluation
getEvaluation pEvaluationId_ =
    GetEvaluation'
    { _geEvaluationId = pEvaluationId_
    }

-- | The ID of the 'Evaluation' to retrieve. The evaluation of each 'MLModel' is recorded and cataloged. The ID provides the means to access the information.
geEvaluationId :: Lens' GetEvaluation Text
geEvaluationId = lens _geEvaluationId (\ s a -> s{_geEvaluationId = a});

instance AWSRequest GetEvaluation where
        type Rs GetEvaluation = GetEvaluationResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 GetEvaluationResponse' <$>
                   (x .?> "Status") <*> (x .?> "PerformanceMetrics") <*>
                     (x .?> "LastUpdatedAt")
                     <*> (x .?> "CreatedAt")
                     <*> (x .?> "InputDataLocationS3")
                     <*> (x .?> "MLModelId")
                     <*> (x .?> "CreatedByIamUser")
                     <*> (x .?> "Name")
                     <*> (x .?> "LogUri")
                     <*> (x .?> "EvaluationId")
                     <*> (x .?> "Message")
                     <*> (x .?> "EvaluationDataSourceId")
                     <*> (pure (fromEnum s)))

instance Hashable GetEvaluation

instance NFData GetEvaluation

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
          = object
              (catMaybes
                 [Just ("EvaluationId" .= _geEvaluationId)])

instance ToPath GetEvaluation where
        toPath = const "/"

instance ToQuery GetEvaluation where
        toQuery = const mempty

-- | Represents the output of a < GetEvaluation> operation and describes an 'Evaluation'.
--
-- /See:/ 'getEvaluationResponse' smart constructor.
data GetEvaluationResponse = GetEvaluationResponse'
    { _gersStatus                 :: !(Maybe EntityStatus)
    , _gersPerformanceMetrics     :: !(Maybe PerformanceMetrics)
    , _gersLastUpdatedAt          :: !(Maybe POSIX)
    , _gersCreatedAt              :: !(Maybe POSIX)
    , _gersInputDataLocationS3    :: !(Maybe Text)
    , _gersMLModelId              :: !(Maybe Text)
    , _gersCreatedByIAMUser       :: !(Maybe Text)
    , _gersName                   :: !(Maybe Text)
    , _gersLogURI                 :: !(Maybe Text)
    , _gersEvaluationId           :: !(Maybe Text)
    , _gersMessage                :: !(Maybe Text)
    , _gersEvaluationDataSourceId :: !(Maybe Text)
    , _gersResponseStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetEvaluationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gersStatus'
--
-- * 'gersPerformanceMetrics'
--
-- * 'gersLastUpdatedAt'
--
-- * 'gersCreatedAt'
--
-- * 'gersInputDataLocationS3'
--
-- * 'gersMLModelId'
--
-- * 'gersCreatedByIAMUser'
--
-- * 'gersName'
--
-- * 'gersLogURI'
--
-- * 'gersEvaluationId'
--
-- * 'gersMessage'
--
-- * 'gersEvaluationDataSourceId'
--
-- * 'gersResponseStatus'
getEvaluationResponse
    :: Int -- ^ 'gersResponseStatus'
    -> GetEvaluationResponse
getEvaluationResponse pResponseStatus_ =
    GetEvaluationResponse'
    { _gersStatus = Nothing
    , _gersPerformanceMetrics = Nothing
    , _gersLastUpdatedAt = Nothing
    , _gersCreatedAt = Nothing
    , _gersInputDataLocationS3 = Nothing
    , _gersMLModelId = Nothing
    , _gersCreatedByIAMUser = Nothing
    , _gersName = Nothing
    , _gersLogURI = Nothing
    , _gersEvaluationId = Nothing
    , _gersMessage = Nothing
    , _gersEvaluationDataSourceId = Nothing
    , _gersResponseStatus = pResponseStatus_
    }

-- | The status of the evaluation. This element can have one of the following values:
--
-- -   'PENDING' - Amazon Machine Language (Amazon ML) submitted a request to evaluate an 'MLModel'.
-- -   'INPROGRESS' - The evaluation is underway.
-- -   'FAILED' - The request to evaluate an 'MLModel' did not run to completion. It is not usable.
-- -   'COMPLETED' - The evaluation process completed successfully.
-- -   'DELETED' - The 'Evaluation' is marked as deleted. It is not usable.
gersStatus :: Lens' GetEvaluationResponse (Maybe EntityStatus)
gersStatus = lens _gersStatus (\ s a -> s{_gersStatus = a});

-- | Measurements of how well the 'MLModel' performed using observations referenced by the 'DataSource'. One of the following metric is returned based on the type of the 'MLModel':
--
-- -   BinaryAUC: A binary 'MLModel' uses the Area Under the Curve (AUC) technique to measure performance.
--
-- -   RegressionRMSE: A regression 'MLModel' uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.
--
-- -   MulticlassAvgFScore: A multiclass 'MLModel' uses the F1 score technique to measure performance.
--
-- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
gersPerformanceMetrics :: Lens' GetEvaluationResponse (Maybe PerformanceMetrics)
gersPerformanceMetrics = lens _gersPerformanceMetrics (\ s a -> s{_gersPerformanceMetrics = a});

-- | The time of the most recent edit to the 'BatchPrediction'. The time is expressed in epoch time.
gersLastUpdatedAt :: Lens' GetEvaluationResponse (Maybe UTCTime)
gersLastUpdatedAt = lens _gersLastUpdatedAt (\ s a -> s{_gersLastUpdatedAt = a}) . mapping _Time;

-- | The time that the 'Evaluation' was created. The time is expressed in epoch time.
gersCreatedAt :: Lens' GetEvaluationResponse (Maybe UTCTime)
gersCreatedAt = lens _gersCreatedAt (\ s a -> s{_gersCreatedAt = a}) . mapping _Time;

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
gersInputDataLocationS3 :: Lens' GetEvaluationResponse (Maybe Text)
gersInputDataLocationS3 = lens _gersInputDataLocationS3 (\ s a -> s{_gersInputDataLocationS3 = a});

-- | The ID of the 'MLModel' that was the focus of the evaluation.
gersMLModelId :: Lens' GetEvaluationResponse (Maybe Text)
gersMLModelId = lens _gersMLModelId (\ s a -> s{_gersMLModelId = a});

-- | The AWS user account that invoked the evaluation. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
gersCreatedByIAMUser :: Lens' GetEvaluationResponse (Maybe Text)
gersCreatedByIAMUser = lens _gersCreatedByIAMUser (\ s a -> s{_gersCreatedByIAMUser = a});

-- | A user-supplied name or description of the 'Evaluation'.
gersName :: Lens' GetEvaluationResponse (Maybe Text)
gersName = lens _gersName (\ s a -> s{_gersName = a});

-- | A link to the file that contains logs of the < CreateEvaluation> operation.
gersLogURI :: Lens' GetEvaluationResponse (Maybe Text)
gersLogURI = lens _gersLogURI (\ s a -> s{_gersLogURI = a});

-- | The evaluation ID which is same as the 'EvaluationId' in the request.
gersEvaluationId :: Lens' GetEvaluationResponse (Maybe Text)
gersEvaluationId = lens _gersEvaluationId (\ s a -> s{_gersEvaluationId = a});

-- | A description of the most recent details about evaluating the 'MLModel'.
gersMessage :: Lens' GetEvaluationResponse (Maybe Text)
gersMessage = lens _gersMessage (\ s a -> s{_gersMessage = a});

-- | The 'DataSource' used for this evaluation.
gersEvaluationDataSourceId :: Lens' GetEvaluationResponse (Maybe Text)
gersEvaluationDataSourceId = lens _gersEvaluationDataSourceId (\ s a -> s{_gersEvaluationDataSourceId = a});

-- | The response status code.
gersResponseStatus :: Lens' GetEvaluationResponse Int
gersResponseStatus = lens _gersResponseStatus (\ s a -> s{_gersResponseStatus = a});

instance NFData GetEvaluationResponse
