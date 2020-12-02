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
-- Module      : Network.AWS.ECR.PutImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the image manifest and tags associated with an image.
--
--
module Network.AWS.ECR.PutImage
    (
    -- * Creating a Request
      putImage
    , PutImage
    -- * Request Lenses
    , piRegistryId
    , piImageTag
    , piRepositoryName
    , piImageManifest

    -- * Destructuring the Response
    , putImageResponse
    , PutImageResponse
    -- * Response Lenses
    , pirsImage
    , pirsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putImage' smart constructor.
data PutImage = PutImage'
  { _piRegistryId     :: !(Maybe Text)
  , _piImageTag       :: !(Maybe Text)
  , _piRepositoryName :: !Text
  , _piImageManifest  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piRegistryId' - The AWS account ID associated with the registry that contains the repository in which to put the image. If you do not specify a registry, the default registry is assumed.
--
-- * 'piImageTag' - The tag to associate with the image. This parameter is required for images that use the Docker Image Manifest V2 Schema 2 or OCI formats.
--
-- * 'piRepositoryName' - The name of the repository in which to put the image.
--
-- * 'piImageManifest' - The image manifest corresponding to the image to be uploaded.
putImage
    :: Text -- ^ 'piRepositoryName'
    -> Text -- ^ 'piImageManifest'
    -> PutImage
putImage pRepositoryName_ pImageManifest_ =
  PutImage'
    { _piRegistryId = Nothing
    , _piImageTag = Nothing
    , _piRepositoryName = pRepositoryName_
    , _piImageManifest = pImageManifest_
    }


-- | The AWS account ID associated with the registry that contains the repository in which to put the image. If you do not specify a registry, the default registry is assumed.
piRegistryId :: Lens' PutImage (Maybe Text)
piRegistryId = lens _piRegistryId (\ s a -> s{_piRegistryId = a})

-- | The tag to associate with the image. This parameter is required for images that use the Docker Image Manifest V2 Schema 2 or OCI formats.
piImageTag :: Lens' PutImage (Maybe Text)
piImageTag = lens _piImageTag (\ s a -> s{_piImageTag = a})

-- | The name of the repository in which to put the image.
piRepositoryName :: Lens' PutImage Text
piRepositoryName = lens _piRepositoryName (\ s a -> s{_piRepositoryName = a})

-- | The image manifest corresponding to the image to be uploaded.
piImageManifest :: Lens' PutImage Text
piImageManifest = lens _piImageManifest (\ s a -> s{_piImageManifest = a})

instance AWSRequest PutImage where
        type Rs PutImage = PutImageResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 PutImageResponse' <$>
                   (x .?> "image") <*> (pure (fromEnum s)))

instance Hashable PutImage where

instance NFData PutImage where

instance ToHeaders PutImage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.PutImage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutImage where
        toJSON PutImage'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _piRegistryId,
                  ("imageTag" .=) <$> _piImageTag,
                  Just ("repositoryName" .= _piRepositoryName),
                  Just ("imageManifest" .= _piImageManifest)])

instance ToPath PutImage where
        toPath = const "/"

instance ToQuery PutImage where
        toQuery = const mempty

-- | /See:/ 'putImageResponse' smart constructor.
data PutImageResponse = PutImageResponse'
  { _pirsImage          :: !(Maybe Image)
  , _pirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pirsImage' - Details of the image uploaded.
--
-- * 'pirsResponseStatus' - -- | The response status code.
putImageResponse
    :: Int -- ^ 'pirsResponseStatus'
    -> PutImageResponse
putImageResponse pResponseStatus_ =
  PutImageResponse'
    {_pirsImage = Nothing, _pirsResponseStatus = pResponseStatus_}


-- | Details of the image uploaded.
pirsImage :: Lens' PutImageResponse (Maybe Image)
pirsImage = lens _pirsImage (\ s a -> s{_pirsImage = a})

-- | -- | The response status code.
pirsResponseStatus :: Lens' PutImageResponse Int
pirsResponseStatus = lens _pirsResponseStatus (\ s a -> s{_pirsResponseStatus = a})

instance NFData PutImageResponse where
