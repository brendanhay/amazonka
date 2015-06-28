{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ECS.ListTasks
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of tasks for a specified cluster. You can filter the
-- results by family name, by a particular container instance, or by the
-- desired status of the task with the @family@, @containerInstance@, and
-- @desiredStatus@ parameters.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTasks.html>
module Network.AWS.ECS.ListTasks
    (
    -- * Request
      ListTasks
    -- ** Request constructor
    , listTasks
    -- ** Request lenses
    , ltDesiredStatus
    , ltCluster
    , ltFamily
    , ltNextToken
    , ltStartedBy
    , ltServiceName
    , ltContainerInstance
    , ltMaxResults

    -- * Response
    , ListTasksResponse
    -- ** Response constructor
    , listTasksResponse
    -- ** Response lenses
    , ltrNextToken
    , ltrTaskARNs
    , ltrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltDesiredStatus'
--
-- * 'ltCluster'
--
-- * 'ltFamily'
--
-- * 'ltNextToken'
--
-- * 'ltStartedBy'
--
-- * 'ltServiceName'
--
-- * 'ltContainerInstance'
--
-- * 'ltMaxResults'
data ListTasks = ListTasks'
    { _ltDesiredStatus     :: !(Maybe DesiredStatus)
    , _ltCluster           :: !(Maybe Text)
    , _ltFamily            :: !(Maybe Text)
    , _ltNextToken         :: !(Maybe Text)
    , _ltStartedBy         :: !(Maybe Text)
    , _ltServiceName       :: !(Maybe Text)
    , _ltContainerInstance :: !(Maybe Text)
    , _ltMaxResults        :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'ListTasks' smart constructor.
listTasks :: ListTasks
listTasks =
    ListTasks'
    { _ltDesiredStatus = Nothing
    , _ltCluster = Nothing
    , _ltFamily = Nothing
    , _ltNextToken = Nothing
    , _ltStartedBy = Nothing
    , _ltServiceName = Nothing
    , _ltContainerInstance = Nothing
    , _ltMaxResults = Nothing
    }

-- | The task status that you want to filter the @ListTasks@ results with.
-- Specifying a @desiredStatus@ of @STOPPED@ will limit the results to
-- tasks that are in the @STOPPED@ status, which can be useful for
-- debugging tasks that are not starting properly or have died or finished.
-- The default status filter is @RUNNING@.
ltDesiredStatus :: Lens' ListTasks (Maybe DesiredStatus)
ltDesiredStatus = lens _ltDesiredStatus (\ s a -> s{_ltDesiredStatus = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the tasks you want to list. If you do not specify a cluster, the
-- default cluster is assumed..
ltCluster :: Lens' ListTasks (Maybe Text)
ltCluster = lens _ltCluster (\ s a -> s{_ltCluster = a});

-- | The name of the family that you want to filter the @ListTasks@ results
-- with. Specifying a @family@ will limit the results to tasks that belong
-- to that family.
ltFamily :: Lens' ListTasks (Maybe Text)
ltFamily = lens _ltFamily (\ s a -> s{_ltFamily = a});

-- | The @nextToken@ value returned from a previous paginated @ListTasks@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
ltNextToken :: Lens' ListTasks (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a});

-- | The @startedBy@ value that you want to filter the task results with.
-- Specifying a @startedBy@ value will limit the results to tasks that were
-- started with that value.
ltStartedBy :: Lens' ListTasks (Maybe Text)
ltStartedBy = lens _ltStartedBy (\ s a -> s{_ltStartedBy = a});

-- | The name of the service that you want to filter the @ListTasks@ results
-- with. Specifying a @serviceName@ will limit the results to tasks that
-- belong to that service.
ltServiceName :: Lens' ListTasks (Maybe Text)
ltServiceName = lens _ltServiceName (\ s a -> s{_ltServiceName = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance that you want to filter the @ListTasks@ results with.
-- Specifying a @containerInstance@ will limit the results to tasks that
-- belong to that container instance.
ltContainerInstance :: Lens' ListTasks (Maybe Text)
ltContainerInstance = lens _ltContainerInstance (\ s a -> s{_ltContainerInstance = a});

-- | The maximum number of task results returned by @ListTasks@ in paginated
-- output. When this parameter is used, @ListTasks@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListTasks@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter is not used, then
-- @ListTasks@ returns up to 100 results and a @nextToken@ value if
-- applicable.
ltMaxResults :: Lens' ListTasks (Maybe Int)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a});

instance AWSPager ListTasks where
        page rq rs
          | stop (rs ^. ltrNextToken) = Nothing
          | stop (rs ^. ltrTaskARNs) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrNextToken

instance AWSRequest ListTasks where
        type Sv ListTasks = ECS
        type Rs ListTasks = ListTasksResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTasksResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "taskArns" .!@ mempty)
                     <*> (pure s))

instance ToHeaders ListTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.ListTasks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTasks where
        toJSON ListTasks'{..}
          = object
              ["desiredStatus" .= _ltDesiredStatus,
               "cluster" .= _ltCluster, "family" .= _ltFamily,
               "nextToken" .= _ltNextToken,
               "startedBy" .= _ltStartedBy,
               "serviceName" .= _ltServiceName,
               "containerInstance" .= _ltContainerInstance,
               "maxResults" .= _ltMaxResults]

instance ToPath ListTasks where
        toPath = const "/"

instance ToQuery ListTasks where
        toQuery = const mempty

-- | /See:/ 'listTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltrNextToken'
--
-- * 'ltrTaskARNs'
--
-- * 'ltrStatus'
data ListTasksResponse = ListTasksResponse'
    { _ltrNextToken :: !(Maybe Text)
    , _ltrTaskARNs  :: !(Maybe [Text])
    , _ltrStatus    :: !Status
    } deriving (Eq,Show)

-- | 'ListTasksResponse' smart constructor.
listTasksResponse :: Status -> ListTasksResponse
listTasksResponse pStatus =
    ListTasksResponse'
    { _ltrNextToken = Nothing
    , _ltrTaskARNs = Nothing
    , _ltrStatus = pStatus
    }

-- | The @nextToken@ value to include in a future @ListTasks@ request. When
-- the results of a @ListTasks@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
ltrNextToken :: Lens' ListTasksResponse (Maybe Text)
ltrNextToken = lens _ltrNextToken (\ s a -> s{_ltrNextToken = a});

-- | The list of task Amazon Resource Name (ARN) entries for the @ListTasks@
-- request.
ltrTaskARNs :: Lens' ListTasksResponse [Text]
ltrTaskARNs = lens _ltrTaskARNs (\ s a -> s{_ltrTaskARNs = a}) . _Default;

-- | FIXME: Undocumented member.
ltrStatus :: Lens' ListTasksResponse Status
ltrStatus = lens _ltrStatus (\ s a -> s{_ltrStatus = a});
