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
-- Module      : Network.AWS.WorkDocs.UpdateDocumentVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the document version to ACTIVE.
--
--
-- Amazon WorkDocs also sets its document container to ACTIVE. This is the last step in a document upload, after the client uploads the document to an S3-presigned URL returned by 'InitiateDocumentVersionUpload' .
--
module Network.AWS.WorkDocs.UpdateDocumentVersion
    (
    -- * Creating a Request
      updateDocumentVersion
    , UpdateDocumentVersion
    -- * Request Lenses
    , udvAuthenticationToken
    , udvVersionStatus
    , udvDocumentId
    , udvVersionId

    -- * Destructuring the Response
    , updateDocumentVersionResponse
    , UpdateDocumentVersionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'updateDocumentVersion' smart constructor.
data UpdateDocumentVersion = UpdateDocumentVersion'
  { _udvAuthenticationToken :: !(Maybe (Sensitive Text))
  , _udvVersionStatus       :: !(Maybe DocumentVersionStatus)
  , _udvDocumentId          :: !Text
  , _udvVersionId           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udvAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'udvVersionStatus' - The status of the version.
--
-- * 'udvDocumentId' - The ID of the document.
--
-- * 'udvVersionId' - The version ID of the document.
updateDocumentVersion
    :: Text -- ^ 'udvDocumentId'
    -> Text -- ^ 'udvVersionId'
    -> UpdateDocumentVersion
updateDocumentVersion pDocumentId_ pVersionId_ =
  UpdateDocumentVersion'
    { _udvAuthenticationToken = Nothing
    , _udvVersionStatus = Nothing
    , _udvDocumentId = pDocumentId_
    , _udvVersionId = pVersionId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
udvAuthenticationToken :: Lens' UpdateDocumentVersion (Maybe Text)
udvAuthenticationToken = lens _udvAuthenticationToken (\ s a -> s{_udvAuthenticationToken = a}) . mapping _Sensitive

-- | The status of the version.
udvVersionStatus :: Lens' UpdateDocumentVersion (Maybe DocumentVersionStatus)
udvVersionStatus = lens _udvVersionStatus (\ s a -> s{_udvVersionStatus = a})

-- | The ID of the document.
udvDocumentId :: Lens' UpdateDocumentVersion Text
udvDocumentId = lens _udvDocumentId (\ s a -> s{_udvDocumentId = a})

-- | The version ID of the document.
udvVersionId :: Lens' UpdateDocumentVersion Text
udvVersionId = lens _udvVersionId (\ s a -> s{_udvVersionId = a})

instance AWSRequest UpdateDocumentVersion where
        type Rs UpdateDocumentVersion =
             UpdateDocumentVersionResponse
        request = patchJSON workDocs
        response = receiveNull UpdateDocumentVersionResponse'

instance Hashable UpdateDocumentVersion where

instance NFData UpdateDocumentVersion where

instance ToHeaders UpdateDocumentVersion where
        toHeaders UpdateDocumentVersion'{..}
          = mconcat
              ["Authentication" =# _udvAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON UpdateDocumentVersion where
        toJSON UpdateDocumentVersion'{..}
          = object
              (catMaybes
                 [("VersionStatus" .=) <$> _udvVersionStatus])

instance ToPath UpdateDocumentVersion where
        toPath UpdateDocumentVersion'{..}
          = mconcat
              ["/api/v1/documents/", toBS _udvDocumentId,
               "/versions/", toBS _udvVersionId]

instance ToQuery UpdateDocumentVersion where
        toQuery = const mempty

-- | /See:/ 'updateDocumentVersionResponse' smart constructor.
data UpdateDocumentVersionResponse =
  UpdateDocumentVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentVersionResponse' with the minimum fields required to make a request.
--
updateDocumentVersionResponse
    :: UpdateDocumentVersionResponse
updateDocumentVersionResponse = UpdateDocumentVersionResponse'


instance NFData UpdateDocumentVersionResponse where
