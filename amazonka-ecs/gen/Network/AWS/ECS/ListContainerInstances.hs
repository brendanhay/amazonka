{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListContainerInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of container instances in a specified cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListContainerInstances.html>
module Network.AWS.ECS.ListContainerInstances
    (
    -- * Request
      ListContainerInstances
    -- ** Request constructor
    , listContainerInstances
    -- ** Request lenses
    , lcirqCluster
    , lcirqNextToken
    , lcirqMaxResults

    -- * Response
    , ListContainerInstancesResponse
    -- ** Response constructor
    , listContainerInstancesResponse
    -- ** Response lenses
    , lcirsContainerInstanceARNs
    , lcirsNextToken
    , lcirsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listContainerInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcirqCluster'
--
-- * 'lcirqNextToken'
--
-- * 'lcirqMaxResults'
data ListContainerInstances = ListContainerInstances'
    { _lcirqCluster    :: !(Maybe Text)
    , _lcirqNextToken  :: !(Maybe Text)
    , _lcirqMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListContainerInstances' smart constructor.
listContainerInstances :: ListContainerInstances
listContainerInstances =
    ListContainerInstances'
    { _lcirqCluster = Nothing
    , _lcirqNextToken = Nothing
    , _lcirqMaxResults = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances you want to list. If you do not specify a
-- cluster, the default cluster is assumed..
lcirqCluster :: Lens' ListContainerInstances (Maybe Text)
lcirqCluster = lens _lcirqCluster (\ s a -> s{_lcirqCluster = a});

-- | The @nextToken@ value returned from a previous paginated
-- @ListContainerInstances@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
lcirqNextToken :: Lens' ListContainerInstances (Maybe Text)
lcirqNextToken = lens _lcirqNextToken (\ s a -> s{_lcirqNextToken = a});

-- | The maximum number of container instance results returned by
-- @ListContainerInstances@ in paginated output. When this parameter is
-- used, @ListContainerInstances@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @ListContainerInstances@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter is not used, then
-- @ListContainerInstances@ returns up to 100 results and a @nextToken@
-- value if applicable.
lcirqMaxResults :: Lens' ListContainerInstances (Maybe Int)
lcirqMaxResults = lens _lcirqMaxResults (\ s a -> s{_lcirqMaxResults = a});

instance AWSPager ListContainerInstances where
        page rq rs
          | stop (rs ^. lcirsNextToken) = Nothing
          | stop (rs ^. lcirsContainerInstanceARNs) = Nothing
          | otherwise =
            Just $ rq & lcirqNextToken .~ rs ^. lcirsNextToken

instance AWSRequest ListContainerInstances where
        type Sv ListContainerInstances = ECS
        type Rs ListContainerInstances =
             ListContainerInstancesResponse
        request = postJSON
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
              ["cluster" .= _lcirqCluster,
               "nextToken" .= _lcirqNextToken,
               "maxResults" .= _lcirqMaxResults]

instance ToPath ListContainerInstances where
        toPath = const "/"

instance ToQuery ListContainerInstances where
        toQuery = const mempty

-- | /See:/ 'listContainerInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcirsContainerInstanceARNs'
--
-- * 'lcirsNextToken'
--
-- * 'lcirsStatus'
data ListContainerInstancesResponse = ListContainerInstancesResponse'
    { _lcirsContainerInstanceARNs :: !(Maybe [Text])
    , _lcirsNextToken             :: !(Maybe Text)
    , _lcirsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListContainerInstancesResponse' smart constructor.
listContainerInstancesResponse :: Int -> ListContainerInstancesResponse
listContainerInstancesResponse pStatus =
    ListContainerInstancesResponse'
    { _lcirsContainerInstanceARNs = Nothing
    , _lcirsNextToken = Nothing
    , _lcirsStatus = pStatus
    }

-- | The list of container instance full Amazon Resource Name (ARN) entries
-- for each container instance associated with the specified cluster.
lcirsContainerInstanceARNs :: Lens' ListContainerInstancesResponse [Text]
lcirsContainerInstanceARNs = lens _lcirsContainerInstanceARNs (\ s a -> s{_lcirsContainerInstanceARNs = a}) . _Default;

-- | The @nextToken@ value to include in a future @ListContainerInstances@
-- request. When the results of a @ListContainerInstances@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
lcirsNextToken :: Lens' ListContainerInstancesResponse (Maybe Text)
lcirsNextToken = lens _lcirsNextToken (\ s a -> s{_lcirsNextToken = a});

-- | FIXME: Undocumented member.
lcirsStatus :: Lens' ListContainerInstancesResponse Int
lcirsStatus = lens _lcirsStatus (\ s a -> s{_lcirsStatus = a});
