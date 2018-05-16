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
-- Module      : Network.AWS.WorkDocs.AbortDocumentVersionUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Aborts the upload of the specified document version that was previously initiated by 'InitiateDocumentVersionUpload' . The client should make this call only when it no longer intends to upload the document version, or fails to do so.
--
--
module Network.AWS.WorkDocs.AbortDocumentVersionUpload
    (
    -- * Creating a Request
      abortDocumentVersionUpload
    , AbortDocumentVersionUpload
    -- * Request Lenses
    , advuAuthenticationToken
    , advuDocumentId
    , advuVersionId

    -- * Destructuring the Response
    , abortDocumentVersionUploadResponse
    , AbortDocumentVersionUploadResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'abortDocumentVersionUpload' smart constructor.
data AbortDocumentVersionUpload = AbortDocumentVersionUpload'
  { _advuAuthenticationToken :: !(Maybe (Sensitive Text))
  , _advuDocumentId          :: !Text
  , _advuVersionId           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortDocumentVersionUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'advuAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'advuDocumentId' - The ID of the document.
--
-- * 'advuVersionId' - The ID of the version.
abortDocumentVersionUpload
    :: Text -- ^ 'advuDocumentId'
    -> Text -- ^ 'advuVersionId'
    -> AbortDocumentVersionUpload
abortDocumentVersionUpload pDocumentId_ pVersionId_ =
  AbortDocumentVersionUpload'
    { _advuAuthenticationToken = Nothing
    , _advuDocumentId = pDocumentId_
    , _advuVersionId = pVersionId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
advuAuthenticationToken :: Lens' AbortDocumentVersionUpload (Maybe Text)
advuAuthenticationToken = lens _advuAuthenticationToken (\ s a -> s{_advuAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the document.
advuDocumentId :: Lens' AbortDocumentVersionUpload Text
advuDocumentId = lens _advuDocumentId (\ s a -> s{_advuDocumentId = a})

-- | The ID of the version.
advuVersionId :: Lens' AbortDocumentVersionUpload Text
advuVersionId = lens _advuVersionId (\ s a -> s{_advuVersionId = a})

instance AWSRequest AbortDocumentVersionUpload where
        type Rs AbortDocumentVersionUpload =
             AbortDocumentVersionUploadResponse
        request = delete workDocs
        response
          = receiveNull AbortDocumentVersionUploadResponse'

instance Hashable AbortDocumentVersionUpload where

instance NFData AbortDocumentVersionUpload where

instance ToHeaders AbortDocumentVersionUpload where
        toHeaders AbortDocumentVersionUpload'{..}
          = mconcat
              ["Authentication" =# _advuAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath AbortDocumentVersionUpload where
        toPath AbortDocumentVersionUpload'{..}
          = mconcat
              ["/api/v1/documents/", toBS _advuDocumentId,
               "/versions/", toBS _advuVersionId]

instance ToQuery AbortDocumentVersionUpload where
        toQuery = const mempty

-- | /See:/ 'abortDocumentVersionUploadResponse' smart constructor.
data AbortDocumentVersionUploadResponse =
  AbortDocumentVersionUploadResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortDocumentVersionUploadResponse' with the minimum fields required to make a request.
--
abortDocumentVersionUploadResponse
    :: AbortDocumentVersionUploadResponse
abortDocumentVersionUploadResponse = AbortDocumentVersionUploadResponse'


instance NFData AbortDocumentVersionUploadResponse
         where
