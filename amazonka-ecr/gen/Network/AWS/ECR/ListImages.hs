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
-- Module      : Network.AWS.ECR.ListImages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the image IDs for a given repository.
--
--
-- You can filter images based on whether or not they are tagged by setting the @tagStatus@ parameter to @TAGGED@ or @UNTAGGED@ . For example, you can filter your results to return only @UNTAGGED@ images and then pipe that result to a 'BatchDeleteImage' operation to delete them. Or, you can filter your results to return only @TAGGED@ images to list all of the tags in your repository.
--
--
-- This operation returns paginated results.
module Network.AWS.ECR.ListImages
    (
    -- * Creating a Request
      listImages
    , ListImages
    -- * Request Lenses
    , liRegistryId
    , liNextToken
    , liFilter
    , liMaxResults
    , liRepositoryName

    -- * Destructuring the Response
    , listImagesResponse
    , ListImagesResponse
    -- * Response Lenses
    , lirsImageIds
    , lirsNextToken
    , lirsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listImages' smart constructor.
data ListImages = ListImages'
  { _liRegistryId     :: !(Maybe Text)
  , _liNextToken      :: !(Maybe Text)
  , _liFilter         :: !(Maybe ListImagesFilter)
  , _liMaxResults     :: !(Maybe Nat)
  , _liRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liRegistryId' - The AWS account ID associated with the registry that contains the repository in which to list images. If you do not specify a registry, the default registry is assumed.
--
-- * 'liNextToken' - The @nextToken@ value returned from a previous paginated @ListImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- * 'liFilter' - The filter key and value with which to filter your @ListImages@ results.
--
-- * 'liMaxResults' - The maximum number of image results returned by @ListImages@ in paginated output. When this parameter is used, @ListImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListImages@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListImages@ returns up to 100 results and a @nextToken@ value, if applicable.
--
-- * 'liRepositoryName' - The repository with image IDs to be listed.
listImages
    :: Text -- ^ 'liRepositoryName'
    -> ListImages
listImages pRepositoryName_ =
  ListImages'
    { _liRegistryId = Nothing
    , _liNextToken = Nothing
    , _liFilter = Nothing
    , _liMaxResults = Nothing
    , _liRepositoryName = pRepositoryName_
    }


-- | The AWS account ID associated with the registry that contains the repository in which to list images. If you do not specify a registry, the default registry is assumed.
liRegistryId :: Lens' ListImages (Maybe Text)
liRegistryId = lens _liRegistryId (\ s a -> s{_liRegistryId = a})

-- | The @nextToken@ value returned from a previous paginated @ListImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
liNextToken :: Lens' ListImages (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | The filter key and value with which to filter your @ListImages@ results.
liFilter :: Lens' ListImages (Maybe ListImagesFilter)
liFilter = lens _liFilter (\ s a -> s{_liFilter = a})

-- | The maximum number of image results returned by @ListImages@ in paginated output. When this parameter is used, @ListImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListImages@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListImages@ returns up to 100 results and a @nextToken@ value, if applicable.
liMaxResults :: Lens' ListImages (Maybe Natural)
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . mapping _Nat

-- | The repository with image IDs to be listed.
liRepositoryName :: Lens' ListImages Text
liRepositoryName = lens _liRepositoryName (\ s a -> s{_liRepositoryName = a})

instance AWSPager ListImages where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsImageIds) = Nothing
          | otherwise =
            Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListImages where
        type Rs ListImages = ListImagesResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 ListImagesResponse' <$>
                   (x .?> "imageIds" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListImages where

instance NFData ListImages where

instance ToHeaders ListImages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.ListImages" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListImages where
        toJSON ListImages'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _liRegistryId,
                  ("nextToken" .=) <$> _liNextToken,
                  ("filter" .=) <$> _liFilter,
                  ("maxResults" .=) <$> _liMaxResults,
                  Just ("repositoryName" .= _liRepositoryName)])

instance ToPath ListImages where
        toPath = const "/"

instance ToQuery ListImages where
        toQuery = const mempty

-- | /See:/ 'listImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { _lirsImageIds       :: !(Maybe [ImageIdentifier])
  , _lirsNextToken      :: !(Maybe Text)
  , _lirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsImageIds' - The list of image IDs for the requested repository.
--
-- * 'lirsNextToken' - The @nextToken@ value to include in a future @ListImages@ request. When the results of a @ListImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listImagesResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListImagesResponse
listImagesResponse pResponseStatus_ =
  ListImagesResponse'
    { _lirsImageIds = Nothing
    , _lirsNextToken = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | The list of image IDs for the requested repository.
lirsImageIds :: Lens' ListImagesResponse [ImageIdentifier]
lirsImageIds = lens _lirsImageIds (\ s a -> s{_lirsImageIds = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @ListImages@ request. When the results of a @ListImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
lirsNextToken :: Lens' ListImagesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListImagesResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListImagesResponse where
