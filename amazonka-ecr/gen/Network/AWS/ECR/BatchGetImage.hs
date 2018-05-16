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
-- Module      : Network.AWS.ECR.BatchGetImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information for specified images within a specified repository. Images are specified with either @imageTag@ or @imageDigest@ .
--
--
module Network.AWS.ECR.BatchGetImage
    (
    -- * Creating a Request
      batchGetImage
    , BatchGetImage
    -- * Request Lenses
    , bgiRegistryId
    , bgiAcceptedMediaTypes
    , bgiRepositoryName
    , bgiImageIds

    -- * Destructuring the Response
    , batchGetImageResponse
    , BatchGetImageResponse
    -- * Response Lenses
    , bgirsImages
    , bgirsFailures
    , bgirsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetImage' smart constructor.
data BatchGetImage = BatchGetImage'
  { _bgiRegistryId         :: !(Maybe Text)
  , _bgiAcceptedMediaTypes :: !(Maybe (List1 Text))
  , _bgiRepositoryName     :: !Text
  , _bgiImageIds           :: ![ImageIdentifier]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgiRegistryId' - The AWS account ID associated with the registry that contains the images to describe. If you do not specify a registry, the default registry is assumed.
--
-- * 'bgiAcceptedMediaTypes' - The accepted media types for the request. Valid values: @application/vnd.docker.distribution.manifest.v1+json@ | @application/vnd.docker.distribution.manifest.v2+json@ | @application/vnd.oci.image.manifest.v1+json@
--
-- * 'bgiRepositoryName' - The repository that contains the images to describe.
--
-- * 'bgiImageIds' - A list of image ID references that correspond to images to describe. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
batchGetImage
    :: Text -- ^ 'bgiRepositoryName'
    -> BatchGetImage
batchGetImage pRepositoryName_ =
  BatchGetImage'
    { _bgiRegistryId = Nothing
    , _bgiAcceptedMediaTypes = Nothing
    , _bgiRepositoryName = pRepositoryName_
    , _bgiImageIds = mempty
    }


-- | The AWS account ID associated with the registry that contains the images to describe. If you do not specify a registry, the default registry is assumed.
bgiRegistryId :: Lens' BatchGetImage (Maybe Text)
bgiRegistryId = lens _bgiRegistryId (\ s a -> s{_bgiRegistryId = a})

-- | The accepted media types for the request. Valid values: @application/vnd.docker.distribution.manifest.v1+json@ | @application/vnd.docker.distribution.manifest.v2+json@ | @application/vnd.oci.image.manifest.v1+json@
bgiAcceptedMediaTypes :: Lens' BatchGetImage (Maybe (NonEmpty Text))
bgiAcceptedMediaTypes = lens _bgiAcceptedMediaTypes (\ s a -> s{_bgiAcceptedMediaTypes = a}) . mapping _List1

-- | The repository that contains the images to describe.
bgiRepositoryName :: Lens' BatchGetImage Text
bgiRepositoryName = lens _bgiRepositoryName (\ s a -> s{_bgiRepositoryName = a})

-- | A list of image ID references that correspond to images to describe. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
bgiImageIds :: Lens' BatchGetImage [ImageIdentifier]
bgiImageIds = lens _bgiImageIds (\ s a -> s{_bgiImageIds = a}) . _Coerce

instance AWSRequest BatchGetImage where
        type Rs BatchGetImage = BatchGetImageResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetImageResponse' <$>
                   (x .?> "images" .!@ mempty) <*>
                     (x .?> "failures" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetImage where

instance NFData BatchGetImage where

instance ToHeaders BatchGetImage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.BatchGetImage"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetImage where
        toJSON BatchGetImage'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _bgiRegistryId,
                  ("acceptedMediaTypes" .=) <$> _bgiAcceptedMediaTypes,
                  Just ("repositoryName" .= _bgiRepositoryName),
                  Just ("imageIds" .= _bgiImageIds)])

instance ToPath BatchGetImage where
        toPath = const "/"

instance ToQuery BatchGetImage where
        toQuery = const mempty

-- | /See:/ 'batchGetImageResponse' smart constructor.
data BatchGetImageResponse = BatchGetImageResponse'
  { _bgirsImages         :: !(Maybe [Image])
  , _bgirsFailures       :: !(Maybe [ImageFailure])
  , _bgirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgirsImages' - A list of image objects corresponding to the image references in the request.
--
-- * 'bgirsFailures' - Any failures associated with the call.
--
-- * 'bgirsResponseStatus' - -- | The response status code.
batchGetImageResponse
    :: Int -- ^ 'bgirsResponseStatus'
    -> BatchGetImageResponse
batchGetImageResponse pResponseStatus_ =
  BatchGetImageResponse'
    { _bgirsImages = Nothing
    , _bgirsFailures = Nothing
    , _bgirsResponseStatus = pResponseStatus_
    }


-- | A list of image objects corresponding to the image references in the request.
bgirsImages :: Lens' BatchGetImageResponse [Image]
bgirsImages = lens _bgirsImages (\ s a -> s{_bgirsImages = a}) . _Default . _Coerce

-- | Any failures associated with the call.
bgirsFailures :: Lens' BatchGetImageResponse [ImageFailure]
bgirsFailures = lens _bgirsFailures (\ s a -> s{_bgirsFailures = a}) . _Default . _Coerce

-- | -- | The response status code.
bgirsResponseStatus :: Lens' BatchGetImageResponse Int
bgirsResponseStatus = lens _bgirsResponseStatus (\ s a -> s{_bgirsResponseStatus = a})

instance NFData BatchGetImageResponse where
