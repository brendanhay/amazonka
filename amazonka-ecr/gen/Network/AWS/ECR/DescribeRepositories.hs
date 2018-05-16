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
-- Module      : Network.AWS.ECR.DescribeRepositories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes image repositories in a registry.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeRepositories
    (
    -- * Creating a Request
      describeRepositories
    , DescribeRepositories
    -- * Request Lenses
    , drRegistryId
    , drRepositoryNames
    , drNextToken
    , drMaxResults

    -- * Destructuring the Response
    , describeRepositoriesResponse
    , DescribeRepositoriesResponse
    -- * Response Lenses
    , drrsRepositories
    , drrsNextToken
    , drrsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRepositories' smart constructor.
data DescribeRepositories = DescribeRepositories'
  { _drRegistryId      :: !(Maybe Text)
  , _drRepositoryNames :: !(Maybe (List1 Text))
  , _drNextToken       :: !(Maybe Text)
  , _drMaxResults      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRepositories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRegistryId' - The AWS account ID associated with the registry that contains the repositories to be described. If you do not specify a registry, the default registry is assumed.
--
-- * 'drRepositoryNames' - A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
--
-- * 'drNextToken' - The @nextToken@ value returned from a previous paginated @DescribeRepositories@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify repositories with @repositoryNames@ .
--
-- * 'drMaxResults' - The maximum number of repository results returned by @DescribeRepositories@ in paginated output. When this parameter is used, @DescribeRepositories@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeRepositories@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeRepositories@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify repositories with @repositoryNames@ .
describeRepositories
    :: DescribeRepositories
describeRepositories =
  DescribeRepositories'
    { _drRegistryId = Nothing
    , _drRepositoryNames = Nothing
    , _drNextToken = Nothing
    , _drMaxResults = Nothing
    }


-- | The AWS account ID associated with the registry that contains the repositories to be described. If you do not specify a registry, the default registry is assumed.
drRegistryId :: Lens' DescribeRepositories (Maybe Text)
drRegistryId = lens _drRegistryId (\ s a -> s{_drRegistryId = a})

-- | A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
drRepositoryNames :: Lens' DescribeRepositories (Maybe (NonEmpty Text))
drRepositoryNames = lens _drRepositoryNames (\ s a -> s{_drRepositoryNames = a}) . mapping _List1

-- | The @nextToken@ value returned from a previous paginated @DescribeRepositories@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify repositories with @repositoryNames@ .
drNextToken :: Lens' DescribeRepositories (Maybe Text)
drNextToken = lens _drNextToken (\ s a -> s{_drNextToken = a})

-- | The maximum number of repository results returned by @DescribeRepositories@ in paginated output. When this parameter is used, @DescribeRepositories@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeRepositories@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeRepositories@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify repositories with @repositoryNames@ .
drMaxResults :: Lens' DescribeRepositories (Maybe Natural)
drMaxResults = lens _drMaxResults (\ s a -> s{_drMaxResults = a}) . mapping _Nat

instance AWSPager DescribeRepositories where
        page rq rs
          | stop (rs ^. drrsNextToken) = Nothing
          | stop (rs ^. drrsRepositories) = Nothing
          | otherwise =
            Just $ rq & drNextToken .~ rs ^. drrsNextToken

instance AWSRequest DescribeRepositories where
        type Rs DescribeRepositories =
             DescribeRepositoriesResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRepositoriesResponse' <$>
                   (x .?> "repositories" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRepositories where

instance NFData DescribeRepositories where

instance ToHeaders DescribeRepositories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.DescribeRepositories"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRepositories where
        toJSON DescribeRepositories'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _drRegistryId,
                  ("repositoryNames" .=) <$> _drRepositoryNames,
                  ("nextToken" .=) <$> _drNextToken,
                  ("maxResults" .=) <$> _drMaxResults])

instance ToPath DescribeRepositories where
        toPath = const "/"

instance ToQuery DescribeRepositories where
        toQuery = const mempty

-- | /See:/ 'describeRepositoriesResponse' smart constructor.
data DescribeRepositoriesResponse = DescribeRepositoriesResponse'
  { _drrsRepositories   :: !(Maybe [Repository])
  , _drrsNextToken      :: !(Maybe Text)
  , _drrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRepositoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRepositories' - A list of repository objects corresponding to valid repositories.
--
-- * 'drrsNextToken' - The @nextToken@ value to include in a future @DescribeRepositories@ request. When the results of a @DescribeRepositories@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeRepositoriesResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeRepositoriesResponse
describeRepositoriesResponse pResponseStatus_ =
  DescribeRepositoriesResponse'
    { _drrsRepositories = Nothing
    , _drrsNextToken = Nothing
    , _drrsResponseStatus = pResponseStatus_
    }


-- | A list of repository objects corresponding to valid repositories.
drrsRepositories :: Lens' DescribeRepositoriesResponse [Repository]
drrsRepositories = lens _drrsRepositories (\ s a -> s{_drrsRepositories = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @DescribeRepositories@ request. When the results of a @DescribeRepositories@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
drrsNextToken :: Lens' DescribeRepositoriesResponse (Maybe Text)
drrsNextToken = lens _drrsNextToken (\ s a -> s{_drrsNextToken = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeRepositoriesResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeRepositoriesResponse where
