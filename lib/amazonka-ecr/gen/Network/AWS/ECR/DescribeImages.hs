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
-- Module      : Network.AWS.ECR.DescribeImages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about the images in a repository, including image size, image tags, and creation date.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeImages
    (
    -- * Creating a Request
      describeImages
    , DescribeImages
    -- * Request Lenses
    , diRegistryId
    , diImageIds
    , diNextToken
    , diFilter
    , diMaxResults
    , diRepositoryName

    -- * Destructuring the Response
    , describeImagesResponse
    , DescribeImagesResponse
    -- * Response Lenses
    , dirsImageDetails
    , dirsNextToken
    , dirsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImages' smart constructor.
data DescribeImages = DescribeImages'
  { _diRegistryId     :: !(Maybe Text)
  , _diImageIds       :: !(Maybe [ImageIdentifier])
  , _diNextToken      :: !(Maybe Text)
  , _diFilter         :: !(Maybe DescribeImagesFilter)
  , _diMaxResults     :: !(Maybe Nat)
  , _diRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diRegistryId' - The AWS account ID associated with the registry that contains the repository in which to describe images. If you do not specify a registry, the default registry is assumed.
--
-- * 'diImageIds' - The list of image IDs for the requested repository.
--
-- * 'diNextToken' - The @nextToken@ value returned from a previous paginated @DescribeImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
--
-- * 'diFilter' - The filter key and value with which to filter your @DescribeImages@ results.
--
-- * 'diMaxResults' - The maximum number of repository results returned by @DescribeImages@ in paginated output. When this parameter is used, @DescribeImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImages@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeImages@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify images with @imageIds@ .
--
-- * 'diRepositoryName' - A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
describeImages
    :: Text -- ^ 'diRepositoryName'
    -> DescribeImages
describeImages pRepositoryName_ =
  DescribeImages'
    { _diRegistryId = Nothing
    , _diImageIds = Nothing
    , _diNextToken = Nothing
    , _diFilter = Nothing
    , _diMaxResults = Nothing
    , _diRepositoryName = pRepositoryName_
    }


-- | The AWS account ID associated with the registry that contains the repository in which to describe images. If you do not specify a registry, the default registry is assumed.
diRegistryId :: Lens' DescribeImages (Maybe Text)
diRegistryId = lens _diRegistryId (\ s a -> s{_diRegistryId = a})

-- | The list of image IDs for the requested repository.
diImageIds :: Lens' DescribeImages [ImageIdentifier]
diImageIds = lens _diImageIds (\ s a -> s{_diImageIds = a}) . _Default . _Coerce

-- | The @nextToken@ value returned from a previous paginated @DescribeImages@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
diNextToken :: Lens' DescribeImages (Maybe Text)
diNextToken = lens _diNextToken (\ s a -> s{_diNextToken = a})

-- | The filter key and value with which to filter your @DescribeImages@ results.
diFilter :: Lens' DescribeImages (Maybe DescribeImagesFilter)
diFilter = lens _diFilter (\ s a -> s{_diFilter = a})

-- | The maximum number of repository results returned by @DescribeImages@ in paginated output. When this parameter is used, @DescribeImages@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeImages@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeImages@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify images with @imageIds@ .
diMaxResults :: Lens' DescribeImages (Maybe Natural)
diMaxResults = lens _diMaxResults (\ s a -> s{_diMaxResults = a}) . mapping _Nat

-- | A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
diRepositoryName :: Lens' DescribeImages Text
diRepositoryName = lens _diRepositoryName (\ s a -> s{_diRepositoryName = a})

instance AWSPager DescribeImages where
        page rq rs
          | stop (rs ^. dirsNextToken) = Nothing
          | stop (rs ^. dirsImageDetails) = Nothing
          | otherwise =
            Just $ rq & diNextToken .~ rs ^. dirsNextToken

instance AWSRequest DescribeImages where
        type Rs DescribeImages = DescribeImagesResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 DescribeImagesResponse' <$>
                   (x .?> "imageDetails" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeImages where

instance NFData DescribeImages where

instance ToHeaders DescribeImages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.DescribeImages"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeImages where
        toJSON DescribeImages'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _diRegistryId,
                  ("imageIds" .=) <$> _diImageIds,
                  ("nextToken" .=) <$> _diNextToken,
                  ("filter" .=) <$> _diFilter,
                  ("maxResults" .=) <$> _diMaxResults,
                  Just ("repositoryName" .= _diRepositoryName)])

instance ToPath DescribeImages where
        toPath = const "/"

instance ToQuery DescribeImages where
        toQuery = const mempty

-- | /See:/ 'describeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { _dirsImageDetails   :: !(Maybe [ImageDetail])
  , _dirsNextToken      :: !(Maybe Text)
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsImageDetails' - A list of 'ImageDetail' objects that contain data about the image.
--
-- * 'dirsNextToken' - The @nextToken@ value to include in a future @DescribeImages@ request. When the results of a @DescribeImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dirsResponseStatus' - -- | The response status code.
describeImagesResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DescribeImagesResponse
describeImagesResponse pResponseStatus_ =
  DescribeImagesResponse'
    { _dirsImageDetails = Nothing
    , _dirsNextToken = Nothing
    , _dirsResponseStatus = pResponseStatus_
    }


-- | A list of 'ImageDetail' objects that contain data about the image.
dirsImageDetails :: Lens' DescribeImagesResponse [ImageDetail]
dirsImageDetails = lens _dirsImageDetails (\ s a -> s{_dirsImageDetails = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @DescribeImages@ request. When the results of a @DescribeImages@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
dirsNextToken :: Lens' DescribeImagesResponse (Maybe Text)
dirsNextToken = lens _dirsNextToken (\ s a -> s{_dirsNextToken = a})

-- | -- | The response status code.
dirsResponseStatus :: Lens' DescribeImagesResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DescribeImagesResponse where
