{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateEvaluation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @Evaluation@ of an @MLModel@. An @MLModel@ is evaluated on
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
    , cerqEvaluationName
    , cerqEvaluationId
    , cerqMLModelId
    , cerqEvaluationDataSourceId

    -- * Response
    , CreateEvaluationResponse
    -- ** Response constructor
    , createEvaluationResponse
    -- ** Response lenses
    , cersEvaluationId
    , cersStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createEvaluation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerqEvaluationName'
--
-- * 'cerqEvaluationId'
--
-- * 'cerqMLModelId'
--
-- * 'cerqEvaluationDataSourceId'
data CreateEvaluation = CreateEvaluation'
    { _cerqEvaluationName         :: !(Maybe Text)
    , _cerqEvaluationId           :: !Text
    , _cerqMLModelId              :: !Text
    , _cerqEvaluationDataSourceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateEvaluation' smart constructor.
createEvaluation :: Text -> Text -> Text -> CreateEvaluation
createEvaluation pEvaluationId_ pMLModelId_ pEvaluationDataSourceId_ =
    CreateEvaluation'
    { _cerqEvaluationName = Nothing
    , _cerqEvaluationId = pEvaluationId_
    , _cerqMLModelId = pMLModelId_
    , _cerqEvaluationDataSourceId = pEvaluationDataSourceId_
    }

-- | A user-supplied name or description of the @Evaluation@.
cerqEvaluationName :: Lens' CreateEvaluation (Maybe Text)
cerqEvaluationName = lens _cerqEvaluationName (\ s a -> s{_cerqEvaluationName = a});

-- | A user-supplied ID that uniquely identifies the @Evaluation@.
cerqEvaluationId :: Lens' CreateEvaluation Text
cerqEvaluationId = lens _cerqEvaluationId (\ s a -> s{_cerqEvaluationId = a});

-- | The ID of the @MLModel@ to evaluate.
--
-- The schema used in creating the @MLModel@ must match the schema of the
-- @DataSource@ used in the @Evaluation@.
cerqMLModelId :: Lens' CreateEvaluation Text
cerqMLModelId = lens _cerqMLModelId (\ s a -> s{_cerqMLModelId = a});

-- | The ID of the @DataSource@ for the evaluation. The schema of the
-- @DataSource@ must match the schema used to create the @MLModel@.
cerqEvaluationDataSourceId :: Lens' CreateEvaluation Text
cerqEvaluationDataSourceId = lens _cerqEvaluationDataSourceId (\ s a -> s{_cerqEvaluationDataSourceId = a});

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
              ["EvaluationName" .= _cerqEvaluationName,
               "EvaluationId" .= _cerqEvaluationId,
               "MLModelId" .= _cerqMLModelId,
               "EvaluationDataSourceId" .=
                 _cerqEvaluationDataSourceId]

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
-- * 'cersEvaluationId'
--
-- * 'cersStatus'
data CreateEvaluationResponse = CreateEvaluationResponse'
    { _cersEvaluationId :: !(Maybe Text)
    , _cersStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateEvaluationResponse' smart constructor.
createEvaluationResponse :: Int -> CreateEvaluationResponse
createEvaluationResponse pStatus_ =
    CreateEvaluationResponse'
    { _cersEvaluationId = Nothing
    , _cersStatus = pStatus_
    }

-- | The user-supplied ID that uniquely identifies the @Evaluation@. This
-- value should be identical to the value of the @EvaluationId@ in the
-- request.
cersEvaluationId :: Lens' CreateEvaluationResponse (Maybe Text)
cersEvaluationId = lens _cersEvaluationId (\ s a -> s{_cersEvaluationId = a});

-- | FIXME: Undocumented member.
cersStatus :: Lens' CreateEvaluationResponse Int
cersStatus = lens _cersStatus (\ s a -> s{_cersStatus = a});
