{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing clusters.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListClusters
  ( -- * Creating a Request
    listClusters,
    ListClusters,

    -- * Request Lenses
    lcNextToken,
    lcMaxResults,

    -- * Destructuring the Response
    listClustersResponse,
    ListClustersResponse,

    -- * Response Lenses
    lcrsClusterARNs,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listClusters' smart constructor.
data ListClusters = ListClusters'
  { _lcNextToken :: !(Maybe Text),
    _lcMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - The @nextToken@ value returned from a @ListClusters@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- * 'lcMaxResults' - The maximum number of cluster results returned by @ListClusters@ in paginated output. When this parameter is used, @ListClusters@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListClusters@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListClusters@ returns up to 100 results and a @nextToken@ value if applicable.
listClusters ::
  ListClusters
listClusters =
  ListClusters' {_lcNextToken = Nothing, _lcMaxResults = Nothing}

-- | The @nextToken@ value returned from a @ListClusters@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
lcNextToken :: Lens' ListClusters (Maybe Text)
lcNextToken = lens _lcNextToken (\s a -> s {_lcNextToken = a})

-- | The maximum number of cluster results returned by @ListClusters@ in paginated output. When this parameter is used, @ListClusters@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListClusters@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListClusters@ returns up to 100 results and a @nextToken@ value if applicable.
lcMaxResults :: Lens' ListClusters (Maybe Int)
lcMaxResults = lens _lcMaxResults (\s a -> s {_lcMaxResults = a})

instance AWSPager ListClusters where
  page rq rs
    | stop (rs ^. lcrsNextToken) = Nothing
    | stop (rs ^. lcrsClusterARNs) = Nothing
    | otherwise = Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListClusters where
  type Rs ListClusters = ListClustersResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          ListClustersResponse'
            <$> (x .?> "clusterArns" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListClusters

instance NFData ListClusters

instance ToHeaders ListClusters where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonEC2ContainerServiceV20141113.ListClusters" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListClusters where
  toJSON ListClusters' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _lcNextToken,
            ("maxResults" .=) <$> _lcMaxResults
          ]
      )

instance ToPath ListClusters where
  toPath = const "/"

instance ToQuery ListClusters where
  toQuery = const mempty

-- | /See:/ 'listClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { _lcrsClusterARNs ::
      !(Maybe [Text]),
    _lcrsNextToken :: !(Maybe Text),
    _lcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsClusterARNs' - The list of full Amazon Resource Name (ARN) entries for each cluster associated with your account.
--
-- * 'lcrsNextToken' - The @nextToken@ value to include in a future @ListClusters@ request. When the results of a @ListClusters@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listClustersResponse ::
  -- | 'lcrsResponseStatus'
  Int ->
  ListClustersResponse
listClustersResponse pResponseStatus_ =
  ListClustersResponse'
    { _lcrsClusterARNs = Nothing,
      _lcrsNextToken = Nothing,
      _lcrsResponseStatus = pResponseStatus_
    }

-- | The list of full Amazon Resource Name (ARN) entries for each cluster associated with your account.
lcrsClusterARNs :: Lens' ListClustersResponse [Text]
lcrsClusterARNs = lens _lcrsClusterARNs (\s a -> s {_lcrsClusterARNs = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @ListClusters@ request. When the results of a @ListClusters@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
lcrsNextToken :: Lens' ListClustersResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\s a -> s {_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListClustersResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\s a -> s {_lcrsResponseStatus = a})

instance NFData ListClustersResponse
