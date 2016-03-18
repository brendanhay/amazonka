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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of specified images within a specified repository. Images
-- are specified with either 'imageTag' or 'imageDigest'.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/BatchDeleteImage.html AWS API Reference> for BatchDeleteImage.
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

import           Network.AWS.ECR.Types
import           Network.AWS.ECR.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Deletes specified images within a specified repository. Images are
-- specified with either the 'imageTag' or 'imageDigest'.
--
-- /See:/ 'batchDeleteImage' smart constructor.
data BatchDeleteImage = BatchDeleteImage'
    { _bdiRegistryId     :: !(Maybe Text)
    , _bdiRepositoryName :: !Text
    , _bdiImageIds       :: !(List1 ImageIdentifier)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchDeleteImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdiRegistryId'
--
-- * 'bdiRepositoryName'
--
-- * 'bdiImageIds'
batchDeleteImage
    :: Text -- ^ 'bdiRepositoryName'
    -> NonEmpty ImageIdentifier -- ^ 'bdiImageIds'
    -> BatchDeleteImage
batchDeleteImage pRepositoryName_ pImageIds_ =
    BatchDeleteImage'
    { _bdiRegistryId = Nothing
    , _bdiRepositoryName = pRepositoryName_
    , _bdiImageIds = _List1 # pImageIds_
    }

-- | The AWS account ID associated with the registry that contains the image
-- to delete. If you do not specify a registry, the default registry is
-- assumed.
bdiRegistryId :: Lens' BatchDeleteImage (Maybe Text)
bdiRegistryId = lens _bdiRegistryId (\ s a -> s{_bdiRegistryId = a});

-- | The repository that contains the image to delete.
bdiRepositoryName :: Lens' BatchDeleteImage Text
bdiRepositoryName = lens _bdiRepositoryName (\ s a -> s{_bdiRepositoryName = a});

-- | A list of image ID references that correspond to images to delete. The
-- format of the 'imageIds' reference is 'imageTag=tag' or
-- 'imageDigest=digest'.
bdiImageIds :: Lens' BatchDeleteImage (NonEmpty ImageIdentifier)
bdiImageIds = lens _bdiImageIds (\ s a -> s{_bdiImageIds = a}) . _List1;

instance AWSRequest BatchDeleteImage where
        type Rs BatchDeleteImage = BatchDeleteImageResponse
        request = postJSON eCR
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeleteImageResponse' <$>
                   (x .?> "failures" .!@ mempty) <*> (x .?> "imageIds")
                     <*> (pure (fromEnum s)))

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
    , _bdirsImageIds       :: !(Maybe (List1 ImageIdentifier))
    , _bdirsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchDeleteImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdirsFailures'
--
-- * 'bdirsImageIds'
--
-- * 'bdirsResponseStatus'
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
bdirsFailures = lens _bdirsFailures (\ s a -> s{_bdirsFailures = a}) . _Default . _Coerce;

-- | The image IDs of the deleted images.
bdirsImageIds :: Lens' BatchDeleteImageResponse (Maybe (NonEmpty ImageIdentifier))
bdirsImageIds = lens _bdirsImageIds (\ s a -> s{_bdirsImageIds = a}) . mapping _List1;

-- | The response status code.
bdirsResponseStatus :: Lens' BatchDeleteImageResponse Int
bdirsResponseStatus = lens _bdirsResponseStatus (\ s a -> s{_bdirsResponseStatus = a});
