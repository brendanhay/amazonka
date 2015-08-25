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
-- Module      : Network.AWS.MachineLearning.CreateEvaluation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'Evaluation' of an 'MLModel'. An 'MLModel' is evaluated on
-- a set of observations associated to a 'DataSource'. Like a 'DataSource'
-- for an 'MLModel', the 'DataSource' for an 'Evaluation' contains values
-- for the Target Variable. The 'Evaluation' compares the predicted result
-- for each observation to the actual outcome and provides a summary so
-- that you know how effective the 'MLModel' functions on the test data.
-- Evaluation generates a relevant performance metric such as BinaryAUC,
-- RegressionRMSE or MulticlassAvgFScore based on the corresponding
-- 'MLModelType': 'BINARY', 'REGRESSION' or 'MULTICLASS'.
--
-- 'CreateEvaluation' is an asynchronous operation. In response to
-- 'CreateEvaluation', Amazon Machine Learning (Amazon ML) immediately
-- returns and sets the evaluation status to 'PENDING'. After the
-- 'Evaluation' is created and ready for use, Amazon ML sets the status to
-- 'COMPLETED'.
--
-- You can use the GetEvaluation operation to check progress of the
-- evaluation during the creation operation.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateEvaluation.html AWS API Reference> for CreateEvaluation.
module Network.AWS.MachineLearning.CreateEvaluation
    (
    -- * Creating a Request
      createEvaluation
    , CreateEvaluation
    -- * Request Lenses
    , ceEvaluationName
    , ceEvaluationId
    , ceMLModelId
    , ceEvaluationDataSourceId

    -- * Destructuring the Response
    , createEvaluationResponse
    , CreateEvaluationResponse
    -- * Response Lenses
    , cersEvaluationId
    , cersStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createEvaluation' smart constructor.
data CreateEvaluation = CreateEvaluation'
    { _ceEvaluationName         :: !(Maybe Text)
    , _ceEvaluationId           :: !Text
    , _ceMLModelId              :: !Text
    , _ceEvaluationDataSourceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateEvaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceEvaluationName'
--
-- * 'ceEvaluationId'
--
-- * 'ceMLModelId'
--
-- * 'ceEvaluationDataSourceId'
createEvaluation
    :: Text -- ^ 'ceEvaluationId'
    -> Text -- ^ 'ceMLModelId'
    -> Text -- ^ 'ceEvaluationDataSourceId'
    -> CreateEvaluation
createEvaluation pEvaluationId_ pMLModelId_ pEvaluationDataSourceId_ =
    CreateEvaluation'
    { _ceEvaluationName = Nothing
    , _ceEvaluationId = pEvaluationId_
    , _ceMLModelId = pMLModelId_
    , _ceEvaluationDataSourceId = pEvaluationDataSourceId_
    }

-- | A user-supplied name or description of the 'Evaluation'.
ceEvaluationName :: Lens' CreateEvaluation (Maybe Text)
ceEvaluationName = lens _ceEvaluationName (\ s a -> s{_ceEvaluationName = a});

-- | A user-supplied ID that uniquely identifies the 'Evaluation'.
ceEvaluationId :: Lens' CreateEvaluation Text
ceEvaluationId = lens _ceEvaluationId (\ s a -> s{_ceEvaluationId = a});

-- | The ID of the 'MLModel' to evaluate.
--
-- The schema used in creating the 'MLModel' must match the schema of the
-- 'DataSource' used in the 'Evaluation'.
ceMLModelId :: Lens' CreateEvaluation Text
ceMLModelId = lens _ceMLModelId (\ s a -> s{_ceMLModelId = a});

-- | The ID of the 'DataSource' for the evaluation. The schema of the
-- 'DataSource' must match the schema used to create the 'MLModel'.
ceEvaluationDataSourceId :: Lens' CreateEvaluation Text
ceEvaluationDataSourceId = lens _ceEvaluationDataSourceId (\ s a -> s{_ceEvaluationDataSourceId = a});

instance AWSRequest CreateEvaluation where
        type Rs CreateEvaluation = CreateEvaluationResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 CreateEvaluationResponse' <$>
                   (x .?> "EvaluationId") <*> (pure (fromEnum s)))

instance ToHeaders CreateEvaluation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateEvaluation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEvaluation where
        toJSON CreateEvaluation'{..}
          = object
              (catMaybes
                 [("EvaluationName" .=) <$> _ceEvaluationName,
                  Just ("EvaluationId" .= _ceEvaluationId),
                  Just ("MLModelId" .= _ceMLModelId),
                  Just
                    ("EvaluationDataSourceId" .=
                       _ceEvaluationDataSourceId)])

instance ToPath CreateEvaluation where
        toPath = const "/"

instance ToQuery CreateEvaluation where
        toQuery = const mempty

-- | Represents the output of a CreateEvaluation operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- CreateEvaluation operation is asynchronous. You can poll for status
-- updates by using the GetEvaluation operation and checking the 'Status'
-- parameter.
--
-- /See:/ 'createEvaluationResponse' smart constructor.
data CreateEvaluationResponse = CreateEvaluationResponse'
    { _cersEvaluationId :: !(Maybe Text)
    , _cersStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateEvaluationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cersEvaluationId'
--
-- * 'cersStatus'
createEvaluationResponse
    :: Int -- ^ 'cersStatus'
    -> CreateEvaluationResponse
createEvaluationResponse pStatus_ =
    CreateEvaluationResponse'
    { _cersEvaluationId = Nothing
    , _cersStatus = pStatus_
    }

-- | The user-supplied ID that uniquely identifies the 'Evaluation'. This
-- value should be identical to the value of the 'EvaluationId' in the
-- request.
cersEvaluationId :: Lens' CreateEvaluationResponse (Maybe Text)
cersEvaluationId = lens _cersEvaluationId (\ s a -> s{_cersEvaluationId = a});

-- | The response status code.
cersStatus :: Lens' CreateEvaluationResponse Int
cersStatus = lens _cersStatus (\ s a -> s{_cersStatus = a});
