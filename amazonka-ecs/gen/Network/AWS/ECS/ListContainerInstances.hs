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
-- Module      : Network.AWS.ECS.ListContainerInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of container instances in a specified cluster.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListContainerInstances.html AWS API Reference> for ListContainerInstances.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListContainerInstances
    (
    -- * Creating a Request
      listContainerInstances
    , ListContainerInstances
    -- * Request Lenses
    , lciCluster
    , lciNextToken
    , lciMaxResults

    -- * Destructuring the Response
    , listContainerInstancesResponse
    , ListContainerInstancesResponse
    -- * Response Lenses
    , lcirsContainerInstanceARNs
    , lcirsNextToken
    , lcirsResponseStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listContainerInstances' smart constructor.
data ListContainerInstances = ListContainerInstances'
    { _lciCluster    :: !(Maybe Text)
    , _lciNextToken  :: !(Maybe Text)
    , _lciMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListContainerInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lciCluster'
--
-- * 'lciNextToken'
--
-- * 'lciMaxResults'
listContainerInstances
    :: ListContainerInstances
listContainerInstances =
    ListContainerInstances'
    { _lciCluster = Nothing
    , _lciNextToken = Nothing
    , _lciMaxResults = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to list. If you do not specify a cluster,
-- the default cluster is assumed..
lciCluster :: Lens' ListContainerInstances (Maybe Text)
lciCluster = lens _lciCluster (\ s a -> s{_lciCluster = a});

-- | The 'nextToken' value returned from a previous paginated
-- 'ListContainerInstances' request where 'maxResults' was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the 'nextToken' value.
-- This value is 'null' when there are no more results to return.
lciNextToken :: Lens' ListContainerInstances (Maybe Text)
lciNextToken = lens _lciNextToken (\ s a -> s{_lciNextToken = a});

-- | The maximum number of container instance results returned by
-- 'ListContainerInstances' in paginated output. When this parameter is
-- used, 'ListContainerInstances' only returns 'maxResults' results in a
-- single page along with a 'nextToken' response element. The remaining
-- results of the initial request can be seen by sending another
-- 'ListContainerInstances' request with the returned 'nextToken' value.
-- This value can be between 1 and 100. If this parameter is not used, then
-- 'ListContainerInstances' returns up to 100 results and a 'nextToken'
-- value if applicable.
lciMaxResults :: Lens' ListContainerInstances (Maybe Int)
lciMaxResults = lens _lciMaxResults (\ s a -> s{_lciMaxResults = a});

instance AWSPager ListContainerInstances where
        page rq rs
          | stop (rs ^. lcirsNextToken) = Nothing
          | stop (rs ^. lcirsContainerInstanceARNs) = Nothing
          | otherwise =
            Just $ rq & lciNextToken .~ rs ^. lcirsNextToken

instance AWSRequest ListContainerInstances where
        type Rs ListContainerInstances =
             ListContainerInstancesResponse
        request = postJSON eCS
        response
          = receiveJSON
              (\ s h x ->
                 ListContainerInstancesResponse' <$>
                   (x .?> "containerInstanceArns" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListContainerInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.ListContainerInstances"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListContainerInstances where
        toJSON ListContainerInstances'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _lciCluster,
                  ("nextToken" .=) <$> _lciNextToken,
                  ("maxResults" .=) <$> _lciMaxResults])

instance ToPath ListContainerInstances where
        toPath = const "/"

instance ToQuery ListContainerInstances where
        toQuery = const mempty

-- | /See:/ 'listContainerInstancesResponse' smart constructor.
data ListContainerInstancesResponse = ListContainerInstancesResponse'
    { _lcirsContainerInstanceARNs :: !(Maybe [Text])
    , _lcirsNextToken             :: !(Maybe Text)
    , _lcirsResponseStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListContainerInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcirsContainerInstanceARNs'
--
-- * 'lcirsNextToken'
--
-- * 'lcirsResponseStatus'
listContainerInstancesResponse
    :: Int -- ^ 'lcirsResponseStatus'
    -> ListContainerInstancesResponse
listContainerInstancesResponse pResponseStatus_ =
    ListContainerInstancesResponse'
    { _lcirsContainerInstanceARNs = Nothing
    , _lcirsNextToken = Nothing
    , _lcirsResponseStatus = pResponseStatus_
    }

-- | The list of container instances with full Amazon Resource Name (ARN)
-- entries for each container instance associated with the specified
-- cluster.
lcirsContainerInstanceARNs :: Lens' ListContainerInstancesResponse [Text]
lcirsContainerInstanceARNs = lens _lcirsContainerInstanceARNs (\ s a -> s{_lcirsContainerInstanceARNs = a}) . _Default . _Coerce;

-- | The 'nextToken' value to include in a future 'ListContainerInstances'
-- request. When the results of a 'ListContainerInstances' request exceed
-- 'maxResults', this value can be used to retrieve the next page of
-- results. This value is 'null' when there are no more results to return.
lcirsNextToken :: Lens' ListContainerInstancesResponse (Maybe Text)
lcirsNextToken = lens _lcirsNextToken (\ s a -> s{_lcirsNextToken = a});

-- | The response status code.
lcirsResponseStatus :: Lens' ListContainerInstancesResponse Int
lcirsResponseStatus = lens _lcirsResponseStatus (\ s a -> s{_lcirsResponseStatus = a});
