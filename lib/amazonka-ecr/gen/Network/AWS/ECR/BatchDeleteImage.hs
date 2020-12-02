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
-- Module      : Network.AWS.ECR.BatchDeleteImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of specified images within a specified repository. Images are specified with either @imageTag@ or @imageDigest@ .
--
--
-- You can remove a tag from an image by specifying the image's tag in your request. When you remove the last tag from an image, the image is deleted from your repository.
--
-- You can completely delete an image (and all of its tags) by specifying the image's digest in your request.
--
module Network.AWS.ECR.BatchDeleteImage
    (
    -- * Creating a Request
      batchDeleteImage
    , BatchDeleteImage
    -- * Request Lenses
    , bdiRegistryId
    , bdiRepositoryName
    , bdiImageIds

    -- * Destructuring the Response
    , batchDeleteImageResponse
    , BatchDeleteImageResponse
    -- * Response Lenses
    , bdirsFailures
    , bdirsImageIds
    , bdirsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes specified images within a specified repository. Images are specified with either the @imageTag@ or @imageDigest@ .
--
--
--
-- /See:/ 'batchDeleteImage' smart constructor.
data BatchDeleteImage = BatchDeleteImage'
  { _bdiRegistryId     :: !(Maybe Text)
  , _bdiRepositoryName :: !Text
  , _bdiImageIds       :: ![ImageIdentifier]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdiRegistryId' - The AWS account ID associated with the registry that contains the image to delete. If you do not specify a registry, the default registry is assumed.
--
-- * 'bdiRepositoryName' - The repository that contains the image to delete.
--
-- * 'bdiImageIds' - A list of image ID references that correspond to images to delete. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
batchDeleteImage
    :: Text -- ^ 'bdiRepositoryName'
    -> BatchDeleteImage
batchDeleteImage pRepositoryName_ =
  BatchDeleteImage'
    { _bdiRegistryId = Nothing
    , _bdiRepositoryName = pRepositoryName_
    , _bdiImageIds = mempty
    }


-- | The AWS account ID associated with the registry that contains the image to delete. If you do not specify a registry, the default registry is assumed.
bdiRegistryId :: Lens' BatchDeleteImage (Maybe Text)
bdiRegistryId = lens _bdiRegistryId (\ s a -> s{_bdiRegistryId = a})

-- | The repository that contains the image to delete.
bdiRepositoryName :: Lens' BatchDeleteImage Text
bdiRepositoryName = lens _bdiRepositoryName (\ s a -> s{_bdiRepositoryName = a})

-- | A list of image ID references that correspond to images to delete. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
bdiImageIds :: Lens' BatchDeleteImage [ImageIdentifier]
bdiImageIds = lens _bdiImageIds (\ s a -> s{_bdiImageIds = a}) . _Coerce

instance AWSRequest BatchDeleteImage where
        type Rs BatchDeleteImage = BatchDeleteImageResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeleteImageResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "imageIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchDeleteImage where

instance NFData BatchDeleteImage where

instance ToHeaders BatchDeleteImage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.BatchDeleteImage"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDeleteImage where
        toJSON BatchDeleteImage'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _bdiRegistryId,
                  Just ("repositoryName" .= _bdiRepositoryName),
                  Just ("imageIds" .= _bdiImageIds)])

instance ToPath BatchDeleteImage where
        toPath = const "/"

instance ToQuery BatchDeleteImage where
        toQuery = const mempty

-- | /See:/ 'batchDeleteImageResponse' smart constructor.
data BatchDeleteImageResponse = BatchDeleteImageResponse'
  { _bdirsFailures       :: !(Maybe [ImageFailure])
  , _bdirsImageIds       :: !(Maybe [ImageIdentifier])
  , _bdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdirsFailures' - Any failures associated with the call.
--
-- * 'bdirsImageIds' - The image IDs of the deleted images.
--
-- * 'bdirsResponseStatus' - -- | The response status code.
batchDeleteImageResponse
    :: Int -- ^ 'bdirsResponseStatus'
    -> BatchDeleteImageResponse
batchDeleteImageResponse pResponseStatus_ =
  BatchDeleteImageResponse'
    { _bdirsFailures = Nothing
    , _bdirsImageIds = Nothing
    , _bdirsResponseStatus = pResponseStatus_
    }


-- | Any failures associated with the call.
bdirsFailures :: Lens' BatchDeleteImageResponse [ImageFailure]
bdirsFailures = lens _bdirsFailures (\ s a -> s{_bdirsFailures = a}) . _Default . _Coerce

-- | The image IDs of the deleted images.
bdirsImageIds :: Lens' BatchDeleteImageResponse [ImageIdentifier]
bdirsImageIds = lens _bdirsImageIds (\ s a -> s{_bdirsImageIds = a}) . _Default . _Coerce

-- | -- | The response status code.
bdirsResponseStatus :: Lens' BatchDeleteImageResponse Int
bdirsResponseStatus = lens _bdirsResponseStatus (\ s a -> s{_bdirsResponseStatus = a})

instance NFData BatchDeleteImageResponse where
