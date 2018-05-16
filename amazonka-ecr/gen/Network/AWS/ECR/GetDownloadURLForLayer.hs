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
-- Module      : Network.AWS.ECR.GetDownloadURLForLayer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the pre-signed Amazon S3 download URL corresponding to an image layer. You can only get URLs for image layers that are referenced in an image.
--
--
module Network.AWS.ECR.GetDownloadURLForLayer
    (
    -- * Creating a Request
      getDownloadURLForLayer
    , GetDownloadURLForLayer
    -- * Request Lenses
    , gduflRegistryId
    , gduflRepositoryName
    , gduflLayerDigest

    -- * Destructuring the Response
    , getDownloadURLForLayerResponse
    , GetDownloadURLForLayerResponse
    -- * Response Lenses
    , gduflrsLayerDigest
    , gduflrsDownloadURL
    , gduflrsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDownloadURLForLayer' smart constructor.
data GetDownloadURLForLayer = GetDownloadURLForLayer'
  { _gduflRegistryId     :: !(Maybe Text)
  , _gduflRepositoryName :: !Text
  , _gduflLayerDigest    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDownloadURLForLayer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gduflRegistryId' - The AWS account ID associated with the registry that contains the image layer to download. If you do not specify a registry, the default registry is assumed.
--
-- * 'gduflRepositoryName' - The name of the repository that is associated with the image layer to download.
--
-- * 'gduflLayerDigest' - The digest of the image layer to download.
getDownloadURLForLayer
    :: Text -- ^ 'gduflRepositoryName'
    -> Text -- ^ 'gduflLayerDigest'
    -> GetDownloadURLForLayer
getDownloadURLForLayer pRepositoryName_ pLayerDigest_ =
  GetDownloadURLForLayer'
    { _gduflRegistryId = Nothing
    , _gduflRepositoryName = pRepositoryName_
    , _gduflLayerDigest = pLayerDigest_
    }


-- | The AWS account ID associated with the registry that contains the image layer to download. If you do not specify a registry, the default registry is assumed.
gduflRegistryId :: Lens' GetDownloadURLForLayer (Maybe Text)
gduflRegistryId = lens _gduflRegistryId (\ s a -> s{_gduflRegistryId = a})

-- | The name of the repository that is associated with the image layer to download.
gduflRepositoryName :: Lens' GetDownloadURLForLayer Text
gduflRepositoryName = lens _gduflRepositoryName (\ s a -> s{_gduflRepositoryName = a})

-- | The digest of the image layer to download.
gduflLayerDigest :: Lens' GetDownloadURLForLayer Text
gduflLayerDigest = lens _gduflLayerDigest (\ s a -> s{_gduflLayerDigest = a})

instance AWSRequest GetDownloadURLForLayer where
        type Rs GetDownloadURLForLayer =
             GetDownloadURLForLayerResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 GetDownloadURLForLayerResponse' <$>
                   (x .?> "layerDigest") <*> (x .?> "downloadUrl") <*>
                     (pure (fromEnum s)))

instance Hashable GetDownloadURLForLayer where

instance NFData GetDownloadURLForLayer where

instance ToHeaders GetDownloadURLForLayer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.GetDownloadUrlForLayer"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDownloadURLForLayer where
        toJSON GetDownloadURLForLayer'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _gduflRegistryId,
                  Just ("repositoryName" .= _gduflRepositoryName),
                  Just ("layerDigest" .= _gduflLayerDigest)])

instance ToPath GetDownloadURLForLayer where
        toPath = const "/"

instance ToQuery GetDownloadURLForLayer where
        toQuery = const mempty

-- | /See:/ 'getDownloadURLForLayerResponse' smart constructor.
data GetDownloadURLForLayerResponse = GetDownloadURLForLayerResponse'
  { _gduflrsLayerDigest    :: !(Maybe Text)
  , _gduflrsDownloadURL    :: !(Maybe Text)
  , _gduflrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDownloadURLForLayerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gduflrsLayerDigest' - The digest of the image layer to download.
--
-- * 'gduflrsDownloadURL' - The pre-signed Amazon S3 download URL for the requested layer.
--
-- * 'gduflrsResponseStatus' - -- | The response status code.
getDownloadURLForLayerResponse
    :: Int -- ^ 'gduflrsResponseStatus'
    -> GetDownloadURLForLayerResponse
getDownloadURLForLayerResponse pResponseStatus_ =
  GetDownloadURLForLayerResponse'
    { _gduflrsLayerDigest = Nothing
    , _gduflrsDownloadURL = Nothing
    , _gduflrsResponseStatus = pResponseStatus_
    }


-- | The digest of the image layer to download.
gduflrsLayerDigest :: Lens' GetDownloadURLForLayerResponse (Maybe Text)
gduflrsLayerDigest = lens _gduflrsLayerDigest (\ s a -> s{_gduflrsLayerDigest = a})

-- | The pre-signed Amazon S3 download URL for the requested layer.
gduflrsDownloadURL :: Lens' GetDownloadURLForLayerResponse (Maybe Text)
gduflrsDownloadURL = lens _gduflrsDownloadURL (\ s a -> s{_gduflrsDownloadURL = a})

-- | -- | The response status code.
gduflrsResponseStatus :: Lens' GetDownloadURLForLayerResponse Int
gduflrsResponseStatus = lens _gduflrsResponseStatus (\ s a -> s{_gduflrsResponseStatus = a})

instance NFData GetDownloadURLForLayerResponse where
