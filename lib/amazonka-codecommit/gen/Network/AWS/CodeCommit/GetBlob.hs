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
-- Module      : Network.AWS.CodeCommit.GetBlob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the base-64 encoded content of an individual blob within a repository.
--
--
module Network.AWS.CodeCommit.GetBlob
    (
    -- * Creating a Request
      getBlob
    , GetBlob
    -- * Request Lenses
    , gRepositoryName
    , gBlobId

    -- * Destructuring the Response
    , getBlobResponse
    , GetBlobResponse
    -- * Response Lenses
    , gbrsResponseStatus
    , gbrsContent
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a get blob operation.
--
--
--
-- /See:/ 'getBlob' smart constructor.
data GetBlob = GetBlob'
  { _gRepositoryName :: !Text
  , _gBlobId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBlob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gRepositoryName' - The name of the repository that contains the blob.
--
-- * 'gBlobId' - The ID of the blob, which is its SHA-1 pointer.
getBlob
    :: Text -- ^ 'gRepositoryName'
    -> Text -- ^ 'gBlobId'
    -> GetBlob
getBlob pRepositoryName_ pBlobId_ =
  GetBlob' {_gRepositoryName = pRepositoryName_, _gBlobId = pBlobId_}


-- | The name of the repository that contains the blob.
gRepositoryName :: Lens' GetBlob Text
gRepositoryName = lens _gRepositoryName (\ s a -> s{_gRepositoryName = a})

-- | The ID of the blob, which is its SHA-1 pointer.
gBlobId :: Lens' GetBlob Text
gBlobId = lens _gBlobId (\ s a -> s{_gBlobId = a})

instance AWSRequest GetBlob where
        type Rs GetBlob = GetBlobResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetBlobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "content"))

instance Hashable GetBlob where

instance NFData GetBlob where

instance ToHeaders GetBlob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetBlob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetBlob where
        toJSON GetBlob'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _gRepositoryName),
                  Just ("blobId" .= _gBlobId)])

instance ToPath GetBlob where
        toPath = const "/"

instance ToQuery GetBlob where
        toQuery = const mempty

-- | Represents the output of a get blob operation.
--
--
--
-- /See:/ 'getBlobResponse' smart constructor.
data GetBlobResponse = GetBlobResponse'
  { _gbrsResponseStatus :: !Int
  , _gbrsContent        :: !Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBlobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrsResponseStatus' - -- | The response status code.
--
-- * 'gbrsContent' - The content of the blob, usually a file.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
getBlobResponse
    :: Int -- ^ 'gbrsResponseStatus'
    -> ByteString -- ^ 'gbrsContent'
    -> GetBlobResponse
getBlobResponse pResponseStatus_ pContent_ =
  GetBlobResponse'
    {_gbrsResponseStatus = pResponseStatus_, _gbrsContent = _Base64 # pContent_}


-- | -- | The response status code.
gbrsResponseStatus :: Lens' GetBlobResponse Int
gbrsResponseStatus = lens _gbrsResponseStatus (\ s a -> s{_gbrsResponseStatus = a})

-- | The content of the blob, usually a file.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gbrsContent :: Lens' GetBlobResponse ByteString
gbrsContent = lens _gbrsContent (\ s a -> s{_gbrsContent = a}) . _Base64

instance NFData GetBlobResponse where
