{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.Predict
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Generates a prediction for the observation using the specified
-- @MLModel@.
--
-- Note
--
-- Not all response parameters will be populated because this is dependent
-- on the type of requested model.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_Predict.html>
module Network.AWS.MachineLearning.Predict
    (
    -- * Request
      Predict
    -- ** Request constructor
    , predict
    -- ** Request lenses
    , preMLModelId
    , preRecord
    , prePredictEndpoint

    -- * Response
    , PredictResponse
    -- ** Response constructor
    , predictResponse
    -- ** Response lenses
    , prPrediction
    , prStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'predict' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'preMLModelId'
--
-- * 'preRecord'
--
-- * 'prePredictEndpoint'
data Predict = Predict'
    { _preMLModelId       :: !Text
    , _preRecord          :: !(Map Text Text)
    , _prePredictEndpoint :: !Text
    } deriving (Eq,Read,Show)

-- | 'Predict' smart constructor.
predict :: Text -> Text -> Predict
predict pMLModelId pPredictEndpoint =
    Predict'
    { _preMLModelId = pMLModelId
    , _preRecord = mempty
    , _prePredictEndpoint = pPredictEndpoint
    }

-- | A unique identifier of the @MLModel@.
preMLModelId :: Lens' Predict Text
preMLModelId = lens _preMLModelId (\ s a -> s{_preMLModelId = a});

-- | FIXME: Undocumented member.
preRecord :: Lens' Predict (HashMap Text Text)
preRecord = lens _preRecord (\ s a -> s{_preRecord = a}) . _Map;

-- | FIXME: Undocumented member.
prePredictEndpoint :: Lens' Predict Text
prePredictEndpoint = lens _prePredictEndpoint (\ s a -> s{_prePredictEndpoint = a});

instance AWSRequest Predict where
        type Sv Predict = MachineLearning
        type Rs Predict = PredictResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PredictResponse' <$>
                   (x .?> "Prediction") <*> (pure s))

instance ToHeaders Predict where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.Predict" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON Predict where
        toJSON Predict'{..}
          = object
              ["MLModelId" .= _preMLModelId,
               "Record" .= _preRecord,
               "PredictEndpoint" .= _prePredictEndpoint]

instance ToPath Predict where
        toPath = const "/"

instance ToQuery Predict where
        toQuery = const mempty

-- | /See:/ 'predictResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prPrediction'
--
-- * 'prStatus'
data PredictResponse = PredictResponse'
    { _prPrediction :: !(Maybe Prediction)
    , _prStatus     :: !Status
    } deriving (Eq,Read,Show)

-- | 'PredictResponse' smart constructor.
predictResponse :: Status -> PredictResponse
predictResponse pStatus =
    PredictResponse'
    { _prPrediction = Nothing
    , _prStatus = pStatus
    }

-- | FIXME: Undocumented member.
prPrediction :: Lens' PredictResponse (Maybe Prediction)
prPrediction = lens _prPrediction (\ s a -> s{_prPrediction = a});

-- | FIXME: Undocumented member.
prStatus :: Lens' PredictResponse Status
prStatus = lens _prStatus (\ s a -> s{_prStatus = a});
