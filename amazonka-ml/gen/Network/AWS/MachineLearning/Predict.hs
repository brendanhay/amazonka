{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Predict
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generates a prediction for the observation using the specified
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
    , prqMLModelId
    , prqRecord
    , prqPredictEndpoint

    -- * Response
    , PredictResponse
    -- ** Response constructor
    , predictResponse
    -- ** Response lenses
    , prsPrediction
    , prsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'predict' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prqMLModelId'
--
-- * 'prqRecord'
--
-- * 'prqPredictEndpoint'
data Predict = Predict'
    { _prqMLModelId       :: !Text
    , _prqRecord          :: !(Map Text Text)
    , _prqPredictEndpoint :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Predict' smart constructor.
predict :: Text -> Text -> Predict
predict pMLModelId_ pPredictEndpoint_ =
    Predict'
    { _prqMLModelId = pMLModelId_
    , _prqRecord = mempty
    , _prqPredictEndpoint = pPredictEndpoint_
    }

-- | A unique identifier of the @MLModel@.
prqMLModelId :: Lens' Predict Text
prqMLModelId = lens _prqMLModelId (\ s a -> s{_prqMLModelId = a});

-- | FIXME: Undocumented member.
prqRecord :: Lens' Predict (HashMap Text Text)
prqRecord = lens _prqRecord (\ s a -> s{_prqRecord = a}) . _Map;

-- | FIXME: Undocumented member.
prqPredictEndpoint :: Lens' Predict Text
prqPredictEndpoint = lens _prqPredictEndpoint (\ s a -> s{_prqPredictEndpoint = a});

instance AWSRequest Predict where
        type Sv Predict = MachineLearning
        type Rs Predict = PredictResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PredictResponse' <$>
                   (x .?> "Prediction") <*> (pure (fromEnum s)))

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
              ["MLModelId" .= _prqMLModelId,
               "Record" .= _prqRecord,
               "PredictEndpoint" .= _prqPredictEndpoint]

instance ToPath Predict where
        toPath = const "/"

instance ToQuery Predict where
        toQuery = const mempty

-- | /See:/ 'predictResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prsPrediction'
--
-- * 'prsStatus'
data PredictResponse = PredictResponse'
    { _prsPrediction :: !(Maybe Prediction)
    , _prsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PredictResponse' smart constructor.
predictResponse :: Int -> PredictResponse
predictResponse pStatus_ =
    PredictResponse'
    { _prsPrediction = Nothing
    , _prsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
prsPrediction :: Lens' PredictResponse (Maybe Prediction)
prsPrediction = lens _prsPrediction (\ s a -> s{_prsPrediction = a});

-- | FIXME: Undocumented member.
prsStatus :: Lens' PredictResponse Int
prsStatus = lens _prsStatus (\ s a -> s{_prsStatus = a});
