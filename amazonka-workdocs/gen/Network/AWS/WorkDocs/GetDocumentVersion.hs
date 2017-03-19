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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , gdvFields
    , gdvDocumentId
    , gdvVersionId

    -- * Destructuring the Response
    , getDocumentVersionResponse
    , GetDocumentVersionResponse
    -- * Response Lenses
    , gdvrsMetadata
    , gdvrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
    { _gdvFields     :: !(Maybe Text)
    , _gdvDocumentId :: !Text
    , _gdvVersionId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDocumentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
    { _gdvFields = Nothing
    , _gdvDocumentId = pDocumentId_
    , _gdvVersionId = pVersionId_
    }

-- | A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
gdvFields :: Lens' GetDocumentVersion (Maybe Text)
gdvFields = lens _gdvFields (\ s a -> s{_gdvFields = a});

-- | The ID of the document.
gdvDocumentId :: Lens' GetDocumentVersion Text
gdvDocumentId = lens _gdvDocumentId (\ s a -> s{_gdvDocumentId = a});

-- | The version ID of the document.
gdvVersionId :: Lens' GetDocumentVersion Text
gdvVersionId = lens _gdvVersionId (\ s a -> s{_gdvVersionId = a});

instance AWSRequest GetDocumentVersion where
        type Rs GetDocumentVersion =
             GetDocumentVersionResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentVersionResponse' <$>
                   (x .?> "Metadata") <*> (pure (fromEnum s)))

instance Hashable GetDocumentVersion

instance NFData GetDocumentVersion

instance ToHeaders GetDocumentVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDocumentVersion where
        toPath GetDocumentVersion'{..}
          = mconcat
              ["/api/v1/documents/", toBS _gdvDocumentId,
               "/versions/", toBS _gdvVersionId]

instance ToQuery GetDocumentVersion where
        toQuery GetDocumentVersion'{..}
          = mconcat ["fields" =: _gdvFields]

-- | /See:/ 'getDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
    { _gdvrsMetadata       :: !(Maybe DocumentVersionMetadata)
    , _gdvrsResponseStatus :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDocumentVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdvrsMetadata' - The version metadata.
--
-- * 'gdvrsResponseStatus' - -- | The response status code.
getDocumentVersionResponse
    :: Int -- ^ 'gdvrsResponseStatus'
    -> GetDocumentVersionResponse
getDocumentVersionResponse pResponseStatus_ =
    GetDocumentVersionResponse'
    { _gdvrsMetadata = Nothing
    , _gdvrsResponseStatus = pResponseStatus_
    }

-- | The version metadata.
gdvrsMetadata :: Lens' GetDocumentVersionResponse (Maybe DocumentVersionMetadata)
gdvrsMetadata = lens _gdvrsMetadata (\ s a -> s{_gdvrsMetadata = a});

-- | -- | The response status code.
gdvrsResponseStatus :: Lens' GetDocumentVersionResponse Int
gdvrsResponseStatus = lens _gdvrsResponseStatus (\ s a -> s{_gdvrsResponseStatus = a});

instance NFData GetDocumentVersionResponse
