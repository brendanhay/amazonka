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
-- Module      : Network.AWS.WorkDocs.InitiateDocumentVersionUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document object and version object.
--
--
-- The client specifies the parent folder ID and name of the document to upload. The ID is optionally specified when creating a new version of an existing document. This is the first step to upload a document. Next, upload the document to the URL returned from the call, and then call 'UpdateDocumentVersion' .
--
-- To cancel the document upload, call 'AbortDocumentVersionUpload' .
--
module Network.AWS.WorkDocs.InitiateDocumentVersionUpload
    (
    -- * Creating a Request
      initiateDocumentVersionUpload
    , InitiateDocumentVersionUpload
    -- * Request Lenses
    , idvuDocumentSizeInBytes
    , idvuContentCreatedTimestamp
    , idvuAuthenticationToken
    , idvuName
    , idvuId
    , idvuContentModifiedTimestamp
    , idvuContentType
    , idvuParentFolderId

    -- * Destructuring the Response
    , initiateDocumentVersionUploadResponse
    , InitiateDocumentVersionUploadResponse
    -- * Response Lenses
    , idvursMetadata
    , idvursUploadMetadata
    , idvursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'initiateDocumentVersionUpload' smart constructor.
data InitiateDocumentVersionUpload = InitiateDocumentVersionUpload'
  { _idvuDocumentSizeInBytes      :: !(Maybe Integer)
  , _idvuContentCreatedTimestamp  :: !(Maybe POSIX)
  , _idvuAuthenticationToken      :: !(Maybe (Sensitive Text))
  , _idvuName                     :: !(Maybe Text)
  , _idvuId                       :: !(Maybe Text)
  , _idvuContentModifiedTimestamp :: !(Maybe POSIX)
  , _idvuContentType              :: !(Maybe Text)
  , _idvuParentFolderId           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InitiateDocumentVersionUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idvuDocumentSizeInBytes' - The size of the document, in bytes.
--
-- * 'idvuContentCreatedTimestamp' - The timestamp when the content of the document was originally created.
--
-- * 'idvuAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'idvuName' - The name of the document.
--
-- * 'idvuId' - The ID of the document.
--
-- * 'idvuContentModifiedTimestamp' - The timestamp when the content of the document was modified.
--
-- * 'idvuContentType' - The content type of the document.
--
-- * 'idvuParentFolderId' - The ID of the parent folder.
initiateDocumentVersionUpload
    :: Text -- ^ 'idvuParentFolderId'
    -> InitiateDocumentVersionUpload
initiateDocumentVersionUpload pParentFolderId_ =
  InitiateDocumentVersionUpload'
    { _idvuDocumentSizeInBytes = Nothing
    , _idvuContentCreatedTimestamp = Nothing
    , _idvuAuthenticationToken = Nothing
    , _idvuName = Nothing
    , _idvuId = Nothing
    , _idvuContentModifiedTimestamp = Nothing
    , _idvuContentType = Nothing
    , _idvuParentFolderId = pParentFolderId_
    }


-- | The size of the document, in bytes.
idvuDocumentSizeInBytes :: Lens' InitiateDocumentVersionUpload (Maybe Integer)
idvuDocumentSizeInBytes = lens _idvuDocumentSizeInBytes (\ s a -> s{_idvuDocumentSizeInBytes = a})

-- | The timestamp when the content of the document was originally created.
idvuContentCreatedTimestamp :: Lens' InitiateDocumentVersionUpload (Maybe UTCTime)
idvuContentCreatedTimestamp = lens _idvuContentCreatedTimestamp (\ s a -> s{_idvuContentCreatedTimestamp = a}) . mapping _Time

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
idvuAuthenticationToken :: Lens' InitiateDocumentVersionUpload (Maybe Text)
idvuAuthenticationToken = lens _idvuAuthenticationToken (\ s a -> s{_idvuAuthenticationToken = a}) . mapping _Sensitive

-- | The name of the document.
idvuName :: Lens' InitiateDocumentVersionUpload (Maybe Text)
idvuName = lens _idvuName (\ s a -> s{_idvuName = a})

-- | The ID of the document.
idvuId :: Lens' InitiateDocumentVersionUpload (Maybe Text)
idvuId = lens _idvuId (\ s a -> s{_idvuId = a})

-- | The timestamp when the content of the document was modified.
idvuContentModifiedTimestamp :: Lens' InitiateDocumentVersionUpload (Maybe UTCTime)
idvuContentModifiedTimestamp = lens _idvuContentModifiedTimestamp (\ s a -> s{_idvuContentModifiedTimestamp = a}) . mapping _Time

-- | The content type of the document.
idvuContentType :: Lens' InitiateDocumentVersionUpload (Maybe Text)
idvuContentType = lens _idvuContentType (\ s a -> s{_idvuContentType = a})

-- | The ID of the parent folder.
idvuParentFolderId :: Lens' InitiateDocumentVersionUpload Text
idvuParentFolderId = lens _idvuParentFolderId (\ s a -> s{_idvuParentFolderId = a})

instance AWSRequest InitiateDocumentVersionUpload
         where
        type Rs InitiateDocumentVersionUpload =
             InitiateDocumentVersionUploadResponse
        request = postJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 InitiateDocumentVersionUploadResponse' <$>
                   (x .?> "Metadata") <*> (x .?> "UploadMetadata") <*>
                     (pure (fromEnum s)))

instance Hashable InitiateDocumentVersionUpload where

instance NFData InitiateDocumentVersionUpload where

instance ToHeaders InitiateDocumentVersionUpload
         where
        toHeaders InitiateDocumentVersionUpload'{..}
          = mconcat
              ["Authentication" =# _idvuAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON InitiateDocumentVersionUpload where
        toJSON InitiateDocumentVersionUpload'{..}
          = object
              (catMaybes
                 [("DocumentSizeInBytes" .=) <$>
                    _idvuDocumentSizeInBytes,
                  ("ContentCreatedTimestamp" .=) <$>
                    _idvuContentCreatedTimestamp,
                  ("Name" .=) <$> _idvuName, ("Id" .=) <$> _idvuId,
                  ("ContentModifiedTimestamp" .=) <$>
                    _idvuContentModifiedTimestamp,
                  ("ContentType" .=) <$> _idvuContentType,
                  Just ("ParentFolderId" .= _idvuParentFolderId)])

instance ToPath InitiateDocumentVersionUpload where
        toPath = const "/api/v1/documents"

instance ToQuery InitiateDocumentVersionUpload where
        toQuery = const mempty

-- | /See:/ 'initiateDocumentVersionUploadResponse' smart constructor.
data InitiateDocumentVersionUploadResponse = InitiateDocumentVersionUploadResponse'
  { _idvursMetadata       :: !(Maybe DocumentMetadata)
  , _idvursUploadMetadata :: !(Maybe UploadMetadata)
  , _idvursResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InitiateDocumentVersionUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idvursMetadata' - The document metadata.
--
-- * 'idvursUploadMetadata' - The upload metadata.
--
-- * 'idvursResponseStatus' - -- | The response status code.
initiateDocumentVersionUploadResponse
    :: Int -- ^ 'idvursResponseStatus'
    -> InitiateDocumentVersionUploadResponse
initiateDocumentVersionUploadResponse pResponseStatus_ =
  InitiateDocumentVersionUploadResponse'
    { _idvursMetadata = Nothing
    , _idvursUploadMetadata = Nothing
    , _idvursResponseStatus = pResponseStatus_
    }


-- | The document metadata.
idvursMetadata :: Lens' InitiateDocumentVersionUploadResponse (Maybe DocumentMetadata)
idvursMetadata = lens _idvursMetadata (\ s a -> s{_idvursMetadata = a})

-- | The upload metadata.
idvursUploadMetadata :: Lens' InitiateDocumentVersionUploadResponse (Maybe UploadMetadata)
idvursUploadMetadata = lens _idvursUploadMetadata (\ s a -> s{_idvursUploadMetadata = a})

-- | -- | The response status code.
idvursResponseStatus :: Lens' InitiateDocumentVersionUploadResponse Int
idvursResponseStatus = lens _idvursResponseStatus (\ s a -> s{_idvursResponseStatus = a})

instance NFData InitiateDocumentVersionUploadResponse
         where
