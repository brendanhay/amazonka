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
-- Module      : Network.AWS.Comprehend.StopTrainingEntityRecognizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entity recognizer training job while in progress.
--
--
-- If the training job state is @TRAINING@ , the job is marked for termination and put into the @STOP_REQUESTED@ state. If the training job completes before it can be stopped, it is put into the @TRAINED@ ; otherwise the training job is stopped and putted into the @STOPPED@ state and the service sends back an HTTP 200 response with an empty HTTP body.
--
module Network.AWS.Comprehend.StopTrainingEntityRecognizer
    (
    -- * Creating a Request
      stopTrainingEntityRecognizer
    , StopTrainingEntityRecognizer
    -- * Request Lenses
    , sterEntityRecognizerARN

    -- * Destructuring the Response
    , stopTrainingEntityRecognizerResponse
    , StopTrainingEntityRecognizerResponse
    -- * Response Lenses
    , sterrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopTrainingEntityRecognizer' smart constructor.
newtype StopTrainingEntityRecognizer = StopTrainingEntityRecognizer'
  { _sterEntityRecognizerARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTrainingEntityRecognizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sterEntityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer currently being trained.
stopTrainingEntityRecognizer
    :: Text -- ^ 'sterEntityRecognizerARN'
    -> StopTrainingEntityRecognizer
stopTrainingEntityRecognizer pEntityRecognizerARN_ =
  StopTrainingEntityRecognizer'
    {_sterEntityRecognizerARN = pEntityRecognizerARN_}


-- | The Amazon Resource Name (ARN) that identifies the entity recognizer currently being trained.
sterEntityRecognizerARN :: Lens' StopTrainingEntityRecognizer Text
sterEntityRecognizerARN = lens _sterEntityRecognizerARN (\ s a -> s{_sterEntityRecognizerARN = a})

instance AWSRequest StopTrainingEntityRecognizer
         where
        type Rs StopTrainingEntityRecognizer =
             StopTrainingEntityRecognizerResponse
        request = postJSON comprehend
        response
          = receiveEmpty
              (\ s h x ->
                 StopTrainingEntityRecognizerResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StopTrainingEntityRecognizer where

instance NFData StopTrainingEntityRecognizer where

instance ToHeaders StopTrainingEntityRecognizer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.StopTrainingEntityRecognizer"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopTrainingEntityRecognizer where
        toJSON StopTrainingEntityRecognizer'{..}
          = object
              (catMaybes
                 [Just
                    ("EntityRecognizerArn" .= _sterEntityRecognizerARN)])

instance ToPath StopTrainingEntityRecognizer where
        toPath = const "/"

instance ToQuery StopTrainingEntityRecognizer where
        toQuery = const mempty

-- | /See:/ 'stopTrainingEntityRecognizerResponse' smart constructor.
newtype StopTrainingEntityRecognizerResponse = StopTrainingEntityRecognizerResponse'
  { _sterrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTrainingEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sterrsResponseStatus' - -- | The response status code.
stopTrainingEntityRecognizerResponse
    :: Int -- ^ 'sterrsResponseStatus'
    -> StopTrainingEntityRecognizerResponse
stopTrainingEntityRecognizerResponse pResponseStatus_ =
  StopTrainingEntityRecognizerResponse'
    {_sterrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sterrsResponseStatus :: Lens' StopTrainingEntityRecognizerResponse Int
sterrsResponseStatus = lens _sterrsResponseStatus (\ s a -> s{_sterrsResponseStatus = a})

instance NFData StopTrainingEntityRecognizerResponse
         where
