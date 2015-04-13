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

-- Module      : Network.AWS.MachineLearning.CreateEvaluation
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

-- | Creates a new 'Evaluation' of an 'MLModel'. An 'MLModel' is evaluated on a set of
-- observations associated to a 'DataSource'. Like a 'DataSource' for an 'MLModel',
-- the 'DataSource' for an 'Evaluation' contains values for the Target Variable. The 'Evaluation' compares the predicted result for each observation to the actual
-- outcome and provides a summary so that you know how effective the 'MLModel'
-- functions on the test data. Evaluation generates a relevant performance
-- metric such as BinaryAUC, RegressionRMSE or MulticlassAvgFScore based on the
-- corresponding 'MLModelType': 'BINARY', 'REGRESSION' or 'MULTICLASS'.
--
-- 'CreateEvaluation' is an asynchronous operation. In response to 'CreateEvaluation', Amazon Machine Learning (Amazon ML) immediately returns and sets the
-- evaluation status to 'PENDING'. After the 'Evaluation' is created and ready for
-- use, Amazon ML sets the status to 'COMPLETED'.
--
-- You can use the 'GetEvaluation' operation to check progress of the evaluation
-- during the creation operation.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateEvaluation.html>
module Network.AWS.MachineLearning.CreateEvaluation
    (
    -- * Request
      CreateEvaluation
    -- ** Request constructor
    , createEvaluation
    -- ** Request lenses
    , ceEvaluationDataSourceId
    , ceEvaluationId
    , ceEvaluationName
    , ceMLModelId

    -- * Response
    , CreateEvaluationResponse
    -- ** Response constructor
    , createEvaluationResponse
    -- ** Response lenses
    , cerEvaluationId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data CreateEvaluation = CreateEvaluation
    { _ceEvaluationDataSourceId :: Text
    , _ceEvaluationId           :: Text
    , _ceEvaluationName         :: Maybe Text
    , _ceMLModelId              :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateEvaluation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ceEvaluationDataSourceId' @::@ 'Text'
--
-- * 'ceEvaluationId' @::@ 'Text'
--
-- * 'ceEvaluationName' @::@ 'Maybe' 'Text'
--
-- * 'ceMLModelId' @::@ 'Text'
--
createEvaluation :: Text -- ^ 'ceEvaluationId'
                 -> Text -- ^ 'ceMLModelId'
                 -> Text -- ^ 'ceEvaluationDataSourceId'
                 -> CreateEvaluation
createEvaluation p1 p2 p3 = CreateEvaluation
    { _ceEvaluationId           = p1
    , _ceMLModelId              = p2
    , _ceEvaluationDataSourceId = p3
    , _ceEvaluationName         = Nothing
    }

-- | The ID of the 'DataSource' for the evaluation. The schema of the 'DataSource'
-- must match the schema used to create the 'MLModel'.
ceEvaluationDataSourceId :: Lens' CreateEvaluation Text
ceEvaluationDataSourceId =
    lens _ceEvaluationDataSourceId
        (\s a -> s { _ceEvaluationDataSourceId = a })

-- | A user-supplied ID that uniquely identifies the 'Evaluation'.
ceEvaluationId :: Lens' CreateEvaluation Text
ceEvaluationId = lens _ceEvaluationId (\s a -> s { _ceEvaluationId = a })

-- | A user-supplied name or description of the 'Evaluation'.
ceEvaluationName :: Lens' CreateEvaluation (Maybe Text)
ceEvaluationName = lens _ceEvaluationName (\s a -> s { _ceEvaluationName = a })

-- | The ID of the 'MLModel' to evaluate.
--
-- The schema used in creating the 'MLModel' must match the schema of the 'DataSource' used in the 'Evaluation'.
ceMLModelId :: Lens' CreateEvaluation Text
ceMLModelId = lens _ceMLModelId (\s a -> s { _ceMLModelId = a })

newtype CreateEvaluationResponse = CreateEvaluationResponse
    { _cerEvaluationId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateEvaluationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerEvaluationId' @::@ 'Maybe' 'Text'
--
createEvaluationResponse :: CreateEvaluationResponse
createEvaluationResponse = CreateEvaluationResponse
    { _cerEvaluationId = Nothing
    }

-- | The user-supplied ID that uniquely identifies the 'Evaluation'. This value
-- should be identical to the value of the 'EvaluationId' in the request.
cerEvaluationId :: Lens' CreateEvaluationResponse (Maybe Text)
cerEvaluationId = lens _cerEvaluationId (\s a -> s { _cerEvaluationId = a })

instance ToPath CreateEvaluation where
    toPath = const "/"

instance ToQuery CreateEvaluation where
    toQuery = const mempty

instance ToHeaders CreateEvaluation

instance ToJSON CreateEvaluation where
    toJSON CreateEvaluation{..} = object
        [ "EvaluationId"           .= _ceEvaluationId
        , "EvaluationName"         .= _ceEvaluationName
        , "MLModelId"              .= _ceMLModelId
        , "EvaluationDataSourceId" .= _ceEvaluationDataSourceId
        ]

instance AWSRequest CreateEvaluation where
    type Sv CreateEvaluation = MachineLearning
    type Rs CreateEvaluation = CreateEvaluationResponse

    request  = post "CreateEvaluation"
    response = jsonResponse

instance FromJSON CreateEvaluationResponse where
    parseJSON = withObject "CreateEvaluationResponse" $ \o -> CreateEvaluationResponse
        <$> o .:? "EvaluationId"
