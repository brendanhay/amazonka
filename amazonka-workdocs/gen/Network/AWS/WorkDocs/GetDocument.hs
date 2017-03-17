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
-- Module      : Network.AWS.WorkDocs.GetDocument
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified document object.
--
--
module Network.AWS.WorkDocs.GetDocument
    (
    -- * Creating a Request
      getDocument
    , GetDocument
    -- * Request Lenses
    , gdDocumentId

    -- * Destructuring the Response
    , getDocumentResponse
    , GetDocumentResponse
    -- * Response Lenses
    , gdrsMetadata
    , gdrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getDocument' smart constructor.
newtype GetDocument = GetDocument'
    { _gdDocumentId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDocumentId' - The ID of the document object.
getDocument
    :: Text -- ^ 'gdDocumentId'
    -> GetDocument
getDocument pDocumentId_ =
    GetDocument'
    { _gdDocumentId = pDocumentId_
    }

-- | The ID of the document object.
gdDocumentId :: Lens' GetDocument Text
gdDocumentId = lens _gdDocumentId (\ s a -> s{_gdDocumentId = a});

instance AWSRequest GetDocument where
        type Rs GetDocument = GetDocumentResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentResponse' <$>
                   (x .?> "Metadata") <*> (pure (fromEnum s)))

instance Hashable GetDocument

instance NFData GetDocument

instance ToHeaders GetDocument where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDocument where
        toPath GetDocument'{..}
          = mconcat ["/api/v1/documents/", toBS _gdDocumentId]

instance ToQuery GetDocument where
        toQuery = const mempty

-- | /See:/ 'getDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
    { _gdrsMetadata       :: !(Maybe DocumentMetadata)
    , _gdrsResponseStatus :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsMetadata' - The document object.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDocumentResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDocumentResponse
getDocumentResponse pResponseStatus_ =
    GetDocumentResponse'
    { _gdrsMetadata = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }

-- | The document object.
gdrsMetadata :: Lens' GetDocumentResponse (Maybe DocumentMetadata)
gdrsMetadata = lens _gdrsMetadata (\ s a -> s{_gdrsMetadata = a});

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDocumentResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a});

instance NFData GetDocumentResponse
