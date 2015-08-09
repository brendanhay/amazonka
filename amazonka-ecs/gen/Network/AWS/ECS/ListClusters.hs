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
-- Module      : Network.AWS.ECS.ListClusters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing clusters.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListClusters.html AWS API Reference> for ListClusters.
module Network.AWS.ECS.ListClusters
    (
    -- * Creating a Request
      ListClusters
    , listClusters
    -- * Request Lenses
    , lcNextToken
    , lcMaxResults

    -- * Destructuring the Response
    , ListClustersResponse
    , listClustersResponse
    -- * Response Lenses
    , lcrsClusterARNs
    , lcrsNextToken
    , lcrsStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listClusters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcNextToken'
--
-- * 'lcMaxResults'
data ListClusters = ListClusters'
    { _lcNextToken :: !(Maybe Text)
    , _lcMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListClusters' smart constructor.
listClusters :: ListClusters
listClusters = 
    ListClusters'
    { _lcNextToken = Nothing
    , _lcMaxResults = Nothing
    }

-- | The @nextToken@ value returned from a previous paginated @ListClusters@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
lcNextToken :: Lens' ListClusters (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a});

-- | The maximum number of cluster results returned by @ListClusters@ in
-- paginated output. When this parameter is used, @ListClusters@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListClusters@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListClusters@ returns up to 100 results and
-- a @nextToken@ value if applicable.
lcMaxResults :: Lens' ListClusters (Maybe Int)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a});

instance AWSPager ListClusters where
        page rq rs
          | stop (rs ^. lcrsNextToken) = Nothing
          | stop (rs ^. lcrsClusterARNs) = Nothing
          | otherwise =
            Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListClusters where
        type Sv ListClusters = ECS
        type Rs ListClusters = ListClustersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListClustersResponse' <$>
                   (x .?> "clusterArns" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListClusters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.ListClusters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListClusters where
        toJSON ListClusters'{..}
          = object
              ["nextToken" .= _lcNextToken,
               "maxResults" .= _lcMaxResults]

instance ToPath ListClusters where
        toPath = const "/"

instance ToQuery ListClusters where
        toQuery = const mempty

-- | /See:/ 'listClustersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcrsClusterARNs'
--
-- * 'lcrsNextToken'
--
-- * 'lcrsStatus'
data ListClustersResponse = ListClustersResponse'
    { _lcrsClusterARNs :: !(Maybe [Text])
    , _lcrsNextToken :: !(Maybe Text)
    , _lcrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListClustersResponse' smart constructor.
listClustersResponse :: Int -> ListClustersResponse
listClustersResponse pStatus_ = 
    ListClustersResponse'
    { _lcrsClusterARNs = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsStatus = pStatus_
    }

-- | The list of full Amazon Resource Name (ARN) entries for each cluster
-- associated with your account.
lcrsClusterARNs :: Lens' ListClustersResponse [Text]
lcrsClusterARNs = lens _lcrsClusterARNs (\ s a -> s{_lcrsClusterARNs = a}) . _Default . _Coerce;

-- | The @nextToken@ value to include in a future @ListClusters@ request.
-- When the results of a @ListClusters@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
lcrsNextToken :: Lens' ListClustersResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a});

-- | Undocumented member.
lcrsStatus :: Lens' ListClustersResponse Int
lcrsStatus = lens _lcrsStatus (\ s a -> s{_lcrsStatus = a});
