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
-- Module      : Network.AWS.Comprehend.DeleteDocumentClassifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created document classifier
--
--
-- Only those classifiers that are in terminated states (IN_ERROR, TRAINED) will be deleted. If an active inference job is using the model, a @ResourceInUseException@ will be returned.
--
-- This is an asynchronous action that puts the classifier into a DELETING state, and it is then removed by a background job. Once removed, the classifier disappears from your account and is no longer available for use.
--
module Network.AWS.Comprehend.DeleteDocumentClassifier
    (
    -- * Creating a Request
      deleteDocumentClassifier
    , DeleteDocumentClassifier
    -- * Request Lenses
    , dDocumentClassifierARN

    -- * Destructuring the Response
    , deleteDocumentClassifierResponse
    , DeleteDocumentClassifierResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDocumentClassifier' smart constructor.
newtype DeleteDocumentClassifier = DeleteDocumentClassifier'
  { _dDocumentClassifierARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocumentClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDocumentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
deleteDocumentClassifier
    :: Text -- ^ 'dDocumentClassifierARN'
    -> DeleteDocumentClassifier
deleteDocumentClassifier pDocumentClassifierARN_ =
  DeleteDocumentClassifier' {_dDocumentClassifierARN = pDocumentClassifierARN_}


-- | The Amazon Resource Name (ARN) that identifies the document classifier.
dDocumentClassifierARN :: Lens' DeleteDocumentClassifier Text
dDocumentClassifierARN = lens _dDocumentClassifierARN (\ s a -> s{_dDocumentClassifierARN = a})

instance AWSRequest DeleteDocumentClassifier where
        type Rs DeleteDocumentClassifier =
             DeleteDocumentClassifierResponse
        request = postJSON comprehend
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDocumentClassifierResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteDocumentClassifier where

instance NFData DeleteDocumentClassifier where

instance ToHeaders DeleteDocumentClassifier where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.DeleteDocumentClassifier" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDocumentClassifier where
        toJSON DeleteDocumentClassifier'{..}
          = object
              (catMaybes
                 [Just
                    ("DocumentClassifierArn" .=
                       _dDocumentClassifierARN)])

instance ToPath DeleteDocumentClassifier where
        toPath = const "/"

instance ToQuery DeleteDocumentClassifier where
        toQuery = const mempty

-- | /See:/ 'deleteDocumentClassifierResponse' smart constructor.
newtype DeleteDocumentClassifierResponse = DeleteDocumentClassifierResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocumentClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteDocumentClassifierResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteDocumentClassifierResponse
deleteDocumentClassifierResponse pResponseStatus_ =
  DeleteDocumentClassifierResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteDocumentClassifierResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteDocumentClassifierResponse
         where
