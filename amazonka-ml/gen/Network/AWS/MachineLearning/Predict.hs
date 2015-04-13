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

-- Module      : Network.AWS.MachineLearning.Predict
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

-- | Generates a prediction for the observation using the specified 'MLModel'.
--
-- Note Not all response parameters will be populated because this is dependent
-- on the type of requested model.
--
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_Predict.html>
module Network.AWS.MachineLearning.Predict
    (
    -- * Request
      Predict
    -- ** Request constructor
    , predict
    -- ** Request lenses
    , pMLModelId
    , pPredictEndpoint
    , pRecord

    -- * Response
    , PredictResponse
    -- ** Response constructor
    , predictResponse
    -- ** Response lenses
    , prPrediction
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data Predict = Predict
    { _pMLModelId       :: Text
    , _pPredictEndpoint :: Text
    , _pRecord          :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'Predict' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pMLModelId' @::@ 'Text'
--
-- * 'pPredictEndpoint' @::@ 'Text'
--
-- * 'pRecord' @::@ 'HashMap' 'Text' 'Text'
--
predict :: Text -- ^ 'pMLModelId'
        -> Text -- ^ 'pPredictEndpoint'
        -> Predict
predict p1 p2 = Predict
    { _pMLModelId       = p1
    , _pPredictEndpoint = p2
    , _pRecord          = mempty
    }

-- | A unique identifier of the 'MLModel'.
pMLModelId :: Lens' Predict Text
pMLModelId = lens _pMLModelId (\s a -> s { _pMLModelId = a })

pPredictEndpoint :: Lens' Predict Text
pPredictEndpoint = lens _pPredictEndpoint (\s a -> s { _pPredictEndpoint = a })

pRecord :: Lens' Predict (HashMap Text Text)
pRecord = lens _pRecord (\s a -> s { _pRecord = a }) . _Map

newtype PredictResponse = PredictResponse
    { _prPrediction :: Maybe Prediction
    } deriving (Eq, Read, Show)

-- | 'PredictResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prPrediction' @::@ 'Maybe' 'Prediction'
--
predictResponse :: PredictResponse
predictResponse = PredictResponse
    { _prPrediction = Nothing
    }

prPrediction :: Lens' PredictResponse (Maybe Prediction)
prPrediction = lens _prPrediction (\s a -> s { _prPrediction = a })

instance ToPath Predict where
    toPath = const "/"

instance ToQuery Predict where
    toQuery = const mempty

instance ToHeaders Predict

instance ToJSON Predict where
    toJSON Predict{..} = object
        [ "MLModelId"       .= _pMLModelId
        , "Record"          .= _pRecord
        , "PredictEndpoint" .= _pPredictEndpoint
        ]

instance AWSRequest Predict where
    type Sv Predict = MachineLearning
    type Rs Predict = PredictResponse

    request  = post "Predict"
    response = jsonResponse

instance FromJSON PredictResponse where
    parseJSON = withObject "PredictResponse" $ \o -> PredictResponse
        <$> o .:? "Prediction"
