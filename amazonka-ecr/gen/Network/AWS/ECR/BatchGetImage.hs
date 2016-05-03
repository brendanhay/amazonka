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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information for specified images within a specified
-- repository. Images are specified with either 'imageTag' or
-- 'imageDigest'.
module Network.AWS.ECR.BatchGetImage
    (
    -- * Creating a Request
      batchGetImage
    , BatchGetImage
    -- * Request Lenses
    , bgiRegistryId
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

import           Network.AWS.ECR.Types
import           Network.AWS.ECR.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'batchGetImage' smart constructor.
data BatchGetImage = BatchGetImage'
    { _bgiRegistryId     :: !(Maybe Text)
    , _bgiRepositoryName :: !Text
    , _bgiImageIds       :: !(List1 ImageIdentifier)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchGetImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgiRegistryId'
--
-- * 'bgiRepositoryName'
--
-- * 'bgiImageIds'
batchGetImage
    :: Text -- ^ 'bgiRepositoryName'
    -> NonEmpty ImageIdentifier -- ^ 'bgiImageIds'
    -> BatchGetImage
batchGetImage pRepositoryName_ pImageIds_ =
    BatchGetImage'
    { _bgiRegistryId = Nothing
    , _bgiRepositoryName = pRepositoryName_
    , _bgiImageIds = _List1 # pImageIds_
    }

-- | The AWS account ID associated with the registry that contains the images
-- to describe. If you do not specify a registry, the default registry is
-- assumed.
bgiRegistryId :: Lens' BatchGetImage (Maybe Text)
bgiRegistryId = lens _bgiRegistryId (\ s a -> s{_bgiRegistryId = a});

-- | The repository that contains the images to describe.
bgiRepositoryName :: Lens' BatchGetImage Text
bgiRepositoryName = lens _bgiRepositoryName (\ s a -> s{_bgiRepositoryName = a});

-- | A list of image ID references that correspond to images to describe. The
-- format of the 'imageIds' reference is 'imageTag=tag' or
-- 'imageDigest=digest'.
bgiImageIds :: Lens' BatchGetImage (NonEmpty ImageIdentifier)
bgiImageIds = lens _bgiImageIds (\ s a -> s{_bgiImageIds = a}) . _List1;

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

instance Hashable BatchGetImage

instance NFData BatchGetImage

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchGetImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgirsImages'
--
-- * 'bgirsFailures'
--
-- * 'bgirsResponseStatus'
batchGetImageResponse
    :: Int -- ^ 'bgirsResponseStatus'
    -> BatchGetImageResponse
batchGetImageResponse pResponseStatus_ =
    BatchGetImageResponse'
    { _bgirsImages = Nothing
    , _bgirsFailures = Nothing
    , _bgirsResponseStatus = pResponseStatus_
    }

-- | A list of image objects corresponding to the image references in the
-- request.
bgirsImages :: Lens' BatchGetImageResponse [Image]
bgirsImages = lens _bgirsImages (\ s a -> s{_bgirsImages = a}) . _Default . _Coerce;

-- | Any failures associated with the call.
bgirsFailures :: Lens' BatchGetImageResponse [ImageFailure]
bgirsFailures = lens _bgirsFailures (\ s a -> s{_bgirsFailures = a}) . _Default . _Coerce;

-- | The response status code.
bgirsResponseStatus :: Lens' BatchGetImageResponse Int
bgirsResponseStatus = lens _bgirsResponseStatus (\ s a -> s{_bgirsResponseStatus = a});

instance NFData BatchGetImageResponse
