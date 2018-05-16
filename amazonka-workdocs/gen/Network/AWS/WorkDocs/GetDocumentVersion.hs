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
-- Module      : Network.AWS.WorkDocs.GetDocumentVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves version metadata for the specified document.
--
--
module Network.AWS.WorkDocs.GetDocumentVersion
    (
    -- * Creating a Request
      getDocumentVersion
    , GetDocumentVersion
    -- * Request Lenses
    , gdvAuthenticationToken
    , gdvIncludeCustomMetadata
    , gdvFields
    , gdvDocumentId
    , gdvVersionId

    -- * Destructuring the Response
    , getDocumentVersionResponse
    , GetDocumentVersionResponse
    -- * Response Lenses
    , gdvrsCustomMetadata
    , gdvrsMetadata
    , gdvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
  { _gdvAuthenticationToken   :: !(Maybe (Sensitive Text))
  , _gdvIncludeCustomMetadata :: !(Maybe Bool)
  , _gdvFields                :: !(Maybe Text)
  , _gdvDocumentId            :: !Text
  , _gdvVersionId             :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdvAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'gdvIncludeCustomMetadata' - Set this to TRUE to include custom metadata in the response.
--
-- * 'gdvFields' - A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
--
-- * 'gdvDocumentId' - The ID of the document.
--
-- * 'gdvVersionId' - The version ID of the document.
getDocumentVersion
    :: Text -- ^ 'gdvDocumentId'
    -> Text -- ^ 'gdvVersionId'
    -> GetDocumentVersion
getDocumentVersion pDocumentId_ pVersionId_ =
  GetDocumentVersion'
    { _gdvAuthenticationToken = Nothing
    , _gdvIncludeCustomMetadata = Nothing
    , _gdvFields = Nothing
    , _gdvDocumentId = pDocumentId_
    , _gdvVersionId = pVersionId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
gdvAuthenticationToken :: Lens' GetDocumentVersion (Maybe Text)
gdvAuthenticationToken = lens _gdvAuthenticationToken (\ s a -> s{_gdvAuthenticationToken = a}) . mapping _Sensitive

-- | Set this to TRUE to include custom metadata in the response.
gdvIncludeCustomMetadata :: Lens' GetDocumentVersion (Maybe Bool)
gdvIncludeCustomMetadata = lens _gdvIncludeCustomMetadata (\ s a -> s{_gdvIncludeCustomMetadata = a})

-- | A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
gdvFields :: Lens' GetDocumentVersion (Maybe Text)
gdvFields = lens _gdvFields (\ s a -> s{_gdvFields = a})

-- | The ID of the document.
gdvDocumentId :: Lens' GetDocumentVersion Text
gdvDocumentId = lens _gdvDocumentId (\ s a -> s{_gdvDocumentId = a})

-- | The version ID of the document.
gdvVersionId :: Lens' GetDocumentVersion Text
gdvVersionId = lens _gdvVersionId (\ s a -> s{_gdvVersionId = a})

instance AWSRequest GetDocumentVersion where
        type Rs GetDocumentVersion =
             GetDocumentVersionResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentVersionResponse' <$>
                   (x .?> "CustomMetadata" .!@ mempty) <*>
                     (x .?> "Metadata")
                     <*> (pure (fromEnum s)))

instance Hashable GetDocumentVersion where

instance NFData GetDocumentVersion where

instance ToHeaders GetDocumentVersion where
        toHeaders GetDocumentVersion'{..}
          = mconcat
              ["Authentication" =# _gdvAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath GetDocumentVersion where
        toPath GetDocumentVersion'{..}
          = mconcat
              ["/api/v1/documents/", toBS _gdvDocumentId,
               "/versions/", toBS _gdvVersionId]

instance ToQuery GetDocumentVersion where
        toQuery GetDocumentVersion'{..}
          = mconcat
              ["includeCustomMetadata" =:
                 _gdvIncludeCustomMetadata,
               "fields" =: _gdvFields]

-- | /See:/ 'getDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
  { _gdvrsCustomMetadata :: !(Maybe (Map Text Text))
  , _gdvrsMetadata       :: !(Maybe DocumentVersionMetadata)
  , _gdvrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdvrsCustomMetadata' - The custom metadata on the document version.
--
-- * 'gdvrsMetadata' - The version metadata.
--
-- * 'gdvrsResponseStatus' - -- | The response status code.
getDocumentVersionResponse
    :: Int -- ^ 'gdvrsResponseStatus'
    -> GetDocumentVersionResponse
getDocumentVersionResponse pResponseStatus_ =
  GetDocumentVersionResponse'
    { _gdvrsCustomMetadata = Nothing
    , _gdvrsMetadata = Nothing
    , _gdvrsResponseStatus = pResponseStatus_
    }


-- | The custom metadata on the document version.
gdvrsCustomMetadata :: Lens' GetDocumentVersionResponse (HashMap Text Text)
gdvrsCustomMetadata = lens _gdvrsCustomMetadata (\ s a -> s{_gdvrsCustomMetadata = a}) . _Default . _Map

-- | The version metadata.
gdvrsMetadata :: Lens' GetDocumentVersionResponse (Maybe DocumentVersionMetadata)
gdvrsMetadata = lens _gdvrsMetadata (\ s a -> s{_gdvrsMetadata = a})

-- | -- | The response status code.
gdvrsResponseStatus :: Lens' GetDocumentVersionResponse Int
gdvrsResponseStatus = lens _gdvrsResponseStatus (\ s a -> s{_gdvrsResponseStatus = a})

instance NFData GetDocumentVersionResponse where
