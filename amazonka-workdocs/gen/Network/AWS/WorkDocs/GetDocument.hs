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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of a document.
--
--
module Network.AWS.WorkDocs.GetDocument
    (
    -- * Creating a Request
      getDocument
    , GetDocument
    -- * Request Lenses
    , gdAuthenticationToken
    , gdIncludeCustomMetadata
    , gdDocumentId

    -- * Destructuring the Response
    , getDocumentResponse
    , GetDocumentResponse
    -- * Response Lenses
    , gdrsCustomMetadata
    , gdrsMetadata
    , gdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getDocument' smart constructor.
data GetDocument = GetDocument'
  { _gdAuthenticationToken   :: !(Maybe (Sensitive Text))
  , _gdIncludeCustomMetadata :: !(Maybe Bool)
  , _gdDocumentId            :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'gdIncludeCustomMetadata' - Set this to @TRUE@ to include custom metadata in the response.
--
-- * 'gdDocumentId' - The ID of the document.
getDocument
    :: Text -- ^ 'gdDocumentId'
    -> GetDocument
getDocument pDocumentId_ =
  GetDocument'
    { _gdAuthenticationToken = Nothing
    , _gdIncludeCustomMetadata = Nothing
    , _gdDocumentId = pDocumentId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
gdAuthenticationToken :: Lens' GetDocument (Maybe Text)
gdAuthenticationToken = lens _gdAuthenticationToken (\ s a -> s{_gdAuthenticationToken = a}) . mapping _Sensitive

-- | Set this to @TRUE@ to include custom metadata in the response.
gdIncludeCustomMetadata :: Lens' GetDocument (Maybe Bool)
gdIncludeCustomMetadata = lens _gdIncludeCustomMetadata (\ s a -> s{_gdIncludeCustomMetadata = a})

-- | The ID of the document.
gdDocumentId :: Lens' GetDocument Text
gdDocumentId = lens _gdDocumentId (\ s a -> s{_gdDocumentId = a})

instance AWSRequest GetDocument where
        type Rs GetDocument = GetDocumentResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentResponse' <$>
                   (x .?> "CustomMetadata" .!@ mempty) <*>
                     (x .?> "Metadata")
                     <*> (pure (fromEnum s)))

instance Hashable GetDocument where

instance NFData GetDocument where

instance ToHeaders GetDocument where
        toHeaders GetDocument'{..}
          = mconcat
              ["Authentication" =# _gdAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath GetDocument where
        toPath GetDocument'{..}
          = mconcat ["/api/v1/documents/", toBS _gdDocumentId]

instance ToQuery GetDocument where
        toQuery GetDocument'{..}
          = mconcat
              ["includeCustomMetadata" =: _gdIncludeCustomMetadata]

-- | /See:/ 'getDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { _gdrsCustomMetadata :: !(Maybe (Map Text Text))
  , _gdrsMetadata       :: !(Maybe DocumentMetadata)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsCustomMetadata' - The custom metadata on the document.
--
-- * 'gdrsMetadata' - The metadata details of the document.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDocumentResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDocumentResponse
getDocumentResponse pResponseStatus_ =
  GetDocumentResponse'
    { _gdrsCustomMetadata = Nothing
    , _gdrsMetadata = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | The custom metadata on the document.
gdrsCustomMetadata :: Lens' GetDocumentResponse (HashMap Text Text)
gdrsCustomMetadata = lens _gdrsCustomMetadata (\ s a -> s{_gdrsCustomMetadata = a}) . _Default . _Map

-- | The metadata details of the document.
gdrsMetadata :: Lens' GetDocumentResponse (Maybe DocumentMetadata)
gdrsMetadata = lens _gdrsMetadata (\ s a -> s{_gdrsMetadata = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDocumentResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDocumentResponse where
