{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.CreateEvaluation
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

-- | Creates a new @Evaluation@ of an @MLModel@. An @MLModel@ is evaluated on
-- a set of observations associated to a @DataSource@. Like a @DataSource@
-- for an @MLModel@, the @DataSource@ for an @Evaluation@ contains values
-- for the Target Variable. The @Evaluation@ compares the predicted result
-- for each observation to the actual outcome and provides a summary so
-- that you know how effective the @MLModel@ functions on the test data.
-- Evaluation generates a relevant performance metric such as BinaryAUC,
-- RegressionRMSE or MulticlassAvgFScore based on the corresponding
-- @MLModelType@: @BINARY@, @REGRESSION@ or @MULTICLASS@.
--
-- @CreateEvaluation@ is an asynchronous operation. In response to
-- @CreateEvaluation@, Amazon Machine Learning (Amazon ML) immediately
-- returns and sets the evaluation status to @PENDING@. After the
-- @Evaluation@ is created and ready for use, Amazon ML sets the status to
-- @COMPLETED@.
--
-- You can use the GetEvaluation operation to check progress of the
-- evaluation during the creation operation.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateEvaluation.html>
module Network.AWS.MachineLearning.CreateEvaluation
    (
    -- * Request
      CreateEvaluation
    -- ** Request constructor
    , createEvaluation
    -- ** Request lenses
    , ceEvaluationName
    , ceEvaluationId
    , ceMLModelId
    , ceEvaluationDataSourceId

    -- * Response
    , CreateEvaluationResponse
    -- ** Response constructor
    , createEvaluationResponse
    -- ** Response lenses
    , cerEvaluationId
    , cerStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createEvaluation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ceEvaluationName'
--
-- * 'ceEvaluationId'
--
-- * 'ceMLModelId'
--
-- * 'ceEvaluationDataSourceId'
data CreateEvaluation = CreateEvaluation'
    { _ceEvaluationName         :: !(Maybe Text)
    , _ceEvaluationId           :: !Text
    , _ceMLModelId              :: !Text
    , _ceEvaluationDataSourceId :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateEvaluation' smart constructor.
createEvaluation :: Text -> Text -> Text -> CreateEvaluation
createEvaluation pEvaluationId pMLModelId pEvaluationDataSourceId =
    CreateEvaluation'
    { _ceEvaluationName = Nothing
    , _ceEvaluationId = pEvaluationId
    , _ceMLModelId = pMLModelId
    , _ceEvaluationDataSourceId = pEvaluationDataSourceId
    }

-- | A user-supplied name or description of the @Evaluation@.
ceEvaluationName :: Lens' CreateEvaluation (Maybe Text)
ceEvaluationName = lens _ceEvaluationName (\ s a -> s{_ceEvaluationName = a});

-- | A user-supplied ID that uniquely identifies the @Evaluation@.
ceEvaluationId :: Lens' CreateEvaluation Text
ceEvaluationId = lens _ceEvaluationId (\ s a -> s{_ceEvaluationId = a});

-- | The ID of the @MLModel@ to evaluate.
--
-- The schema used in creating the @MLModel@ must match the schema of the
-- @DataSource@ used in the @Evaluation@.
ceMLModelId :: Lens' CreateEvaluation Text
ceMLModelId = lens _ceMLModelId (\ s a -> s{_ceMLModelId = a});

-- | The ID of the @DataSource@ for the evaluation. The schema of the
-- @DataSource@ must match the schema used to create the @MLModel@.
ceEvaluationDataSourceId :: Lens' CreateEvaluation Text
ceEvaluationDataSourceId = lens _ceEvaluationDataSourceId (\ s a -> s{_ceEvaluationDataSourceId = a});

instance AWSRequest CreateEvaluation where
        type Sv CreateEvaluation = MachineLearning
        type Rs CreateEvaluation = CreateEvaluationResponse
        request = postJSON
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
              ["EvaluationName" .= _ceEvaluationName,
               "EvaluationId" .= _ceEvaluationId,
               "MLModelId" .= _ceMLModelId,
               "EvaluationDataSourceId" .=
                 _ceEvaluationDataSourceId]

instance ToPath CreateEvaluation where
        toPath = const "/"

instance ToQuery CreateEvaluation where
        toQuery = const mempty

-- | Represents the output of a CreateEvaluation operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- CreateEvaluation operation is asynchronous. You can poll for status
-- updates by using the GetEvaluation operation and checking the @Status@
-- parameter.
--
-- /See:/ 'createEvaluationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerEvaluationId'
--
-- * 'cerStatus'
data CreateEvaluationResponse = CreateEvaluationResponse'
    { _cerEvaluationId :: !(Maybe Text)
    , _cerStatus       :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateEvaluationResponse' smart constructor.
createEvaluationResponse :: Int -> CreateEvaluationResponse
createEvaluationResponse pStatus =
    CreateEvaluationResponse'
    { _cerEvaluationId = Nothing
    , _cerStatus = pStatus
    }

-- | The user-supplied ID that uniquely identifies the @Evaluation@. This
-- value should be identical to the value of the @EvaluationId@ in the
-- request.
cerEvaluationId :: Lens' CreateEvaluationResponse (Maybe Text)
cerEvaluationId = lens _cerEvaluationId (\ s a -> s{_cerEvaluationId = a});

-- | FIXME: Undocumented member.
cerStatus :: Lens' CreateEvaluationResponse Int
cerStatus = lens _cerStatus (\ s a -> s{_cerStatus = a});
