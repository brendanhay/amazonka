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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a prediction for the observation using the specified 'ML Model'.
--
-- Note
--
-- Not all response parameters will be populated. Whether a response parameter is populated depends on the type of model requested.
module Network.AWS.MachineLearning.Predict
    (
    -- * Creating a Request
      predict
    , Predict
    -- * Request Lenses
    , pMLModelId
    , pRecord
    , pPredictEndpoint

    -- * Destructuring the Response
    , predictResponse
    , PredictResponse
    -- * Response Lenses
    , prsPrediction
    , prsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'predict' smart constructor.
data Predict = Predict'
    { _pMLModelId       :: !Text
    , _pRecord          :: !(Map Text Text)
    , _pPredictEndpoint :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Predict' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pMLModelId'
--
-- * 'pRecord'
--
-- * 'pPredictEndpoint'
predict
    :: Text -- ^ 'pMLModelId'
    -> Text -- ^ 'pPredictEndpoint'
    -> Predict
predict pMLModelId_ pPredictEndpoint_ =
    Predict'
    { _pMLModelId = pMLModelId_
    , _pRecord = mempty
    , _pPredictEndpoint = pPredictEndpoint_
    }

-- | A unique identifier of the 'MLModel'.
pMLModelId :: Lens' Predict Text
pMLModelId = lens _pMLModelId (\ s a -> s{_pMLModelId = a});

-- | Undocumented member.
pRecord :: Lens' Predict (HashMap Text Text)
pRecord = lens _pRecord (\ s a -> s{_pRecord = a}) . _Map;

-- | Undocumented member.
pPredictEndpoint :: Lens' Predict Text
pPredictEndpoint = lens _pPredictEndpoint (\ s a -> s{_pPredictEndpoint = a});

instance AWSRequest Predict where
        type Rs Predict = PredictResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 PredictResponse' <$>
                   (x .?> "Prediction") <*> (pure (fromEnum s)))

instance Hashable Predict

instance NFData Predict

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
              (catMaybes
                 [Just ("MLModelId" .= _pMLModelId),
                  Just ("Record" .= _pRecord),
                  Just ("PredictEndpoint" .= _pPredictEndpoint)])

instance ToPath Predict where
        toPath = const "/"

instance ToQuery Predict where
        toQuery = const mempty

-- | /See:/ 'predictResponse' smart constructor.
data PredictResponse = PredictResponse'
    { _prsPrediction     :: !(Maybe Prediction)
    , _prsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PredictResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsPrediction'
--
-- * 'prsResponseStatus'
predictResponse
    :: Int -- ^ 'prsResponseStatus'
    -> PredictResponse
predictResponse pResponseStatus_ =
    PredictResponse'
    { _prsPrediction = Nothing
    , _prsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
prsPrediction :: Lens' PredictResponse (Maybe Prediction)
prsPrediction = lens _prsPrediction (\ s a -> s{_prsPrediction = a});

-- | The response status code.
prsResponseStatus :: Lens' PredictResponse Int
prsResponseStatus = lens _prsResponseStatus (\ s a -> s{_prsResponseStatus = a});

instance NFData PredictResponse
