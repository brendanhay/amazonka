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
-- Module      : Network.AWS.MachineLearning.Predict
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_Predict.html AWS API Reference> for Predict.
module Network.AWS.MachineLearning.Predict
    (
    -- * Creating a Request
      Predict
    , predict
    -- * Request Lenses
    , pMLModelId
    , pRecord
    , pPredictEndpoint

    -- * Destructuring the Response
    , PredictResponse
    , predictResponse
    -- * Response Lenses
    , prsPrediction
    , prsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'predict' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pMLModelId'
--
-- * 'pRecord'
--
-- * 'pPredictEndpoint'
data Predict = Predict'
    { _pMLModelId       :: !Text
    , _pRecord          :: !(Map Text Text)
    , _pPredictEndpoint :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Predict' smart constructor.
predict :: Text -> Text -> Predict
predict pMLModelId_ pPredictEndpoint_ =
    Predict'
    { _pMLModelId = pMLModelId_
    , _pRecord = mempty
    , _pPredictEndpoint = pPredictEndpoint_
    }

-- | A unique identifier of the @MLModel@.
pMLModelId :: Lens' Predict Text
pMLModelId = lens _pMLModelId (\ s a -> s{_pMLModelId = a});

-- | Undocumented member.
pRecord :: Lens' Predict (HashMap Text Text)
pRecord = lens _pRecord (\ s a -> s{_pRecord = a}) . _Map;

-- | Undocumented member.
pPredictEndpoint :: Lens' Predict Text
pPredictEndpoint = lens _pPredictEndpoint (\ s a -> s{_pPredictEndpoint = a});

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
              ["MLModelId" .= _pMLModelId, "Record" .= _pRecord,
               "PredictEndpoint" .= _pPredictEndpoint]

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

-- | Undocumented member.
prsPrediction :: Lens' PredictResponse (Maybe Prediction)
prsPrediction = lens _prsPrediction (\ s a -> s{_prsPrediction = a});

-- | Undocumented member.
prsStatus :: Lens' PredictResponse Int
prsStatus = lens _prsStatus (\ s a -> s{_prsStatus = a});
