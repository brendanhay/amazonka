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
-- Module      : Network.AWS.ECR.CompleteLayerUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Informs Amazon ECR that the image layer upload has completed for a specified registry, repository name, and upload ID. You can optionally provide a @sha256@ digest of the image layer for data validation purposes.
--
--
module Network.AWS.ECR.CompleteLayerUpload
    (
    -- * Creating a Request
      completeLayerUpload
    , CompleteLayerUpload
    -- * Request Lenses
    , cluRegistryId
    , cluRepositoryName
    , cluUploadId
    , cluLayerDigests

    -- * Destructuring the Response
    , completeLayerUploadResponse
    , CompleteLayerUploadResponse
    -- * Response Lenses
    , clursRegistryId
    , clursLayerDigest
    , clursRepositoryName
    , clursUploadId
    , clursResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'completeLayerUpload' smart constructor.
data CompleteLayerUpload = CompleteLayerUpload'
  { _cluRegistryId     :: !(Maybe Text)
  , _cluRepositoryName :: !Text
  , _cluUploadId       :: !Text
  , _cluLayerDigests   :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompleteLayerUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cluRegistryId' - The AWS account ID associated with the registry to which to upload layers. If you do not specify a registry, the default registry is assumed.
--
-- * 'cluRepositoryName' - The name of the repository to associate with the image layer.
--
-- * 'cluUploadId' - The upload ID from a previous 'InitiateLayerUpload' operation to associate with the image layer.
--
-- * 'cluLayerDigests' - The @sha256@ digest of the image layer.
completeLayerUpload
    :: Text -- ^ 'cluRepositoryName'
    -> Text -- ^ 'cluUploadId'
    -> NonEmpty Text -- ^ 'cluLayerDigests'
    -> CompleteLayerUpload
completeLayerUpload pRepositoryName_ pUploadId_ pLayerDigests_ =
  CompleteLayerUpload'
    { _cluRegistryId = Nothing
    , _cluRepositoryName = pRepositoryName_
    , _cluUploadId = pUploadId_
    , _cluLayerDigests = _List1 # pLayerDigests_
    }


-- | The AWS account ID associated with the registry to which to upload layers. If you do not specify a registry, the default registry is assumed.
cluRegistryId :: Lens' CompleteLayerUpload (Maybe Text)
cluRegistryId = lens _cluRegistryId (\ s a -> s{_cluRegistryId = a})

-- | The name of the repository to associate with the image layer.
cluRepositoryName :: Lens' CompleteLayerUpload Text
cluRepositoryName = lens _cluRepositoryName (\ s a -> s{_cluRepositoryName = a})

-- | The upload ID from a previous 'InitiateLayerUpload' operation to associate with the image layer.
cluUploadId :: Lens' CompleteLayerUpload Text
cluUploadId = lens _cluUploadId (\ s a -> s{_cluUploadId = a})

-- | The @sha256@ digest of the image layer.
cluLayerDigests :: Lens' CompleteLayerUpload (NonEmpty Text)
cluLayerDigests = lens _cluLayerDigests (\ s a -> s{_cluLayerDigests = a}) . _List1

instance AWSRequest CompleteLayerUpload where
        type Rs CompleteLayerUpload =
             CompleteLayerUploadResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 CompleteLayerUploadResponse' <$>
                   (x .?> "registryId") <*> (x .?> "layerDigest") <*>
                     (x .?> "repositoryName")
                     <*> (x .?> "uploadId")
                     <*> (pure (fromEnum s)))

instance Hashable CompleteLayerUpload where

instance NFData CompleteLayerUpload where

instance ToHeaders CompleteLayerUpload where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.CompleteLayerUpload"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CompleteLayerUpload where
        toJSON CompleteLayerUpload'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _cluRegistryId,
                  Just ("repositoryName" .= _cluRepositoryName),
                  Just ("uploadId" .= _cluUploadId),
                  Just ("layerDigests" .= _cluLayerDigests)])

instance ToPath CompleteLayerUpload where
        toPath = const "/"

instance ToQuery CompleteLayerUpload where
        toQuery = const mempty

-- | /See:/ 'completeLayerUploadResponse' smart constructor.
data CompleteLayerUploadResponse = CompleteLayerUploadResponse'
  { _clursRegistryId     :: !(Maybe Text)
  , _clursLayerDigest    :: !(Maybe Text)
  , _clursRepositoryName :: !(Maybe Text)
  , _clursUploadId       :: !(Maybe Text)
  , _clursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompleteLayerUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clursRegistryId' - The registry ID associated with the request.
--
-- * 'clursLayerDigest' - The @sha256@ digest of the image layer.
--
-- * 'clursRepositoryName' - The repository name associated with the request.
--
-- * 'clursUploadId' - The upload ID associated with the layer.
--
-- * 'clursResponseStatus' - -- | The response status code.
completeLayerUploadResponse
    :: Int -- ^ 'clursResponseStatus'
    -> CompleteLayerUploadResponse
completeLayerUploadResponse pResponseStatus_ =
  CompleteLayerUploadResponse'
    { _clursRegistryId = Nothing
    , _clursLayerDigest = Nothing
    , _clursRepositoryName = Nothing
    , _clursUploadId = Nothing
    , _clursResponseStatus = pResponseStatus_
    }


-- | The registry ID associated with the request.
clursRegistryId :: Lens' CompleteLayerUploadResponse (Maybe Text)
clursRegistryId = lens _clursRegistryId (\ s a -> s{_clursRegistryId = a})

-- | The @sha256@ digest of the image layer.
clursLayerDigest :: Lens' CompleteLayerUploadResponse (Maybe Text)
clursLayerDigest = lens _clursLayerDigest (\ s a -> s{_clursLayerDigest = a})

-- | The repository name associated with the request.
clursRepositoryName :: Lens' CompleteLayerUploadResponse (Maybe Text)
clursRepositoryName = lens _clursRepositoryName (\ s a -> s{_clursRepositoryName = a})

-- | The upload ID associated with the layer.
clursUploadId :: Lens' CompleteLayerUploadResponse (Maybe Text)
clursUploadId = lens _clursUploadId (\ s a -> s{_clursUploadId = a})

-- | -- | The response status code.
clursResponseStatus :: Lens' CompleteLayerUploadResponse Int
clursResponseStatus = lens _clursResponseStatus (\ s a -> s{_clursResponseStatus = a})

instance NFData CompleteLayerUploadResponse where
