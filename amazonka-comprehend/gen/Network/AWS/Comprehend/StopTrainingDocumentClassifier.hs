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
-- Module      : Network.AWS.Comprehend.StopTrainingDocumentClassifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a document classifier training job while in progress.
--
--
-- If the training job state is @TRAINING@ , the job is marked for termination and put into the @STOP_REQUESTED@ state. If the training job completes before it can be stopped, it is put into the @TRAINED@ ; otherwise the training job is stopped and put into the @STOPPED@ state and the service sends back an HTTP 200 response with an empty HTTP body.
--
module Network.AWS.Comprehend.StopTrainingDocumentClassifier
    (
    -- * Creating a Request
      stopTrainingDocumentClassifier
    , StopTrainingDocumentClassifier
    -- * Request Lenses
    , stdcDocumentClassifierARN

    -- * Destructuring the Response
    , stopTrainingDocumentClassifierResponse
    , StopTrainingDocumentClassifierResponse
    -- * Response Lenses
    , stdcrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopTrainingDocumentClassifier' smart constructor.
newtype StopTrainingDocumentClassifier = StopTrainingDocumentClassifier'
  { _stdcDocumentClassifierARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTrainingDocumentClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdcDocumentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
stopTrainingDocumentClassifier
    :: Text -- ^ 'stdcDocumentClassifierARN'
    -> StopTrainingDocumentClassifier
stopTrainingDocumentClassifier pDocumentClassifierARN_ =
  StopTrainingDocumentClassifier'
    {_stdcDocumentClassifierARN = pDocumentClassifierARN_}


-- | The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
stdcDocumentClassifierARN :: Lens' StopTrainingDocumentClassifier Text
stdcDocumentClassifierARN = lens _stdcDocumentClassifierARN (\ s a -> s{_stdcDocumentClassifierARN = a})

instance AWSRequest StopTrainingDocumentClassifier
         where
        type Rs StopTrainingDocumentClassifier =
             StopTrainingDocumentClassifierResponse
        request = postJSON comprehend
        response
          = receiveEmpty
              (\ s h x ->
                 StopTrainingDocumentClassifierResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StopTrainingDocumentClassifier
         where

instance NFData StopTrainingDocumentClassifier where

instance ToHeaders StopTrainingDocumentClassifier
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.StopTrainingDocumentClassifier"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopTrainingDocumentClassifier where
        toJSON StopTrainingDocumentClassifier'{..}
          = object
              (catMaybes
                 [Just
                    ("DocumentClassifierArn" .=
                       _stdcDocumentClassifierARN)])

instance ToPath StopTrainingDocumentClassifier where
        toPath = const "/"

instance ToQuery StopTrainingDocumentClassifier where
        toQuery = const mempty

-- | /See:/ 'stopTrainingDocumentClassifierResponse' smart constructor.
newtype StopTrainingDocumentClassifierResponse = StopTrainingDocumentClassifierResponse'
  { _stdcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTrainingDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdcrsResponseStatus' - -- | The response status code.
stopTrainingDocumentClassifierResponse
    :: Int -- ^ 'stdcrsResponseStatus'
    -> StopTrainingDocumentClassifierResponse
stopTrainingDocumentClassifierResponse pResponseStatus_ =
  StopTrainingDocumentClassifierResponse'
    {_stdcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
stdcrsResponseStatus :: Lens' StopTrainingDocumentClassifierResponse Int
stdcrsResponseStatus = lens _stdcrsResponseStatus (\ s a -> s{_stdcrsResponseStatus = a})

instance NFData
           StopTrainingDocumentClassifierResponse
         where
