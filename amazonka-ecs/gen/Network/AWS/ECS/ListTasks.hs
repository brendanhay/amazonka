{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tasks for a specified cluster. You can filter the
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
    , ltrqDesiredStatus
    , ltrqCluster
    , ltrqFamily
    , ltrqNextToken
    , ltrqStartedBy
    , ltrqServiceName
    , ltrqContainerInstance
    , ltrqMaxResults

    -- * Response
    , ListTasksResponse
    -- ** Response constructor
    , listTasksResponse
    -- ** Response lenses
    , ltrsNextToken
    , ltrsTaskARNs
    , ltrsStatus
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
-- * 'ltrqDesiredStatus'
--
-- * 'ltrqCluster'
--
-- * 'ltrqFamily'
--
-- * 'ltrqNextToken'
--
-- * 'ltrqStartedBy'
--
-- * 'ltrqServiceName'
--
-- * 'ltrqContainerInstance'
--
-- * 'ltrqMaxResults'
data ListTasks = ListTasks'
    { _ltrqDesiredStatus     :: !(Maybe DesiredStatus)
    , _ltrqCluster           :: !(Maybe Text)
    , _ltrqFamily            :: !(Maybe Text)
    , _ltrqNextToken         :: !(Maybe Text)
    , _ltrqStartedBy         :: !(Maybe Text)
    , _ltrqServiceName       :: !(Maybe Text)
    , _ltrqContainerInstance :: !(Maybe Text)
    , _ltrqMaxResults        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTasks' smart constructor.
listTasks :: ListTasks
listTasks =
    ListTasks'
    { _ltrqDesiredStatus = Nothing
    , _ltrqCluster = Nothing
    , _ltrqFamily = Nothing
    , _ltrqNextToken = Nothing
    , _ltrqStartedBy = Nothing
    , _ltrqServiceName = Nothing
    , _ltrqContainerInstance = Nothing
    , _ltrqMaxResults = Nothing
    }

-- | The task status that you want to filter the @ListTasks@ results with.
-- Specifying a @desiredStatus@ of @STOPPED@ will limit the results to
-- tasks that are in the @STOPPED@ status, which can be useful for
-- debugging tasks that are not starting properly or have died or finished.
-- The default status filter is @RUNNING@.
ltrqDesiredStatus :: Lens' ListTasks (Maybe DesiredStatus)
ltrqDesiredStatus = lens _ltrqDesiredStatus (\ s a -> s{_ltrqDesiredStatus = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the tasks you want to list. If you do not specify a cluster, the
-- default cluster is assumed..
ltrqCluster :: Lens' ListTasks (Maybe Text)
ltrqCluster = lens _ltrqCluster (\ s a -> s{_ltrqCluster = a});

-- | The name of the family that you want to filter the @ListTasks@ results
-- with. Specifying a @family@ will limit the results to tasks that belong
-- to that family.
ltrqFamily :: Lens' ListTasks (Maybe Text)
ltrqFamily = lens _ltrqFamily (\ s a -> s{_ltrqFamily = a});

-- | The @nextToken@ value returned from a previous paginated @ListTasks@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
ltrqNextToken :: Lens' ListTasks (Maybe Text)
ltrqNextToken = lens _ltrqNextToken (\ s a -> s{_ltrqNextToken = a});

-- | The @startedBy@ value that you want to filter the task results with.
-- Specifying a @startedBy@ value will limit the results to tasks that were
-- started with that value.
ltrqStartedBy :: Lens' ListTasks (Maybe Text)
ltrqStartedBy = lens _ltrqStartedBy (\ s a -> s{_ltrqStartedBy = a});

-- | The name of the service that you want to filter the @ListTasks@ results
-- with. Specifying a @serviceName@ will limit the results to tasks that
-- belong to that service.
ltrqServiceName :: Lens' ListTasks (Maybe Text)
ltrqServiceName = lens _ltrqServiceName (\ s a -> s{_ltrqServiceName = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance that you want to filter the @ListTasks@ results with.
-- Specifying a @containerInstance@ will limit the results to tasks that
-- belong to that container instance.
ltrqContainerInstance :: Lens' ListTasks (Maybe Text)
ltrqContainerInstance = lens _ltrqContainerInstance (\ s a -> s{_ltrqContainerInstance = a});

-- | The maximum number of task results returned by @ListTasks@ in paginated
-- output. When this parameter is used, @ListTasks@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListTasks@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter is not used, then
-- @ListTasks@ returns up to 100 results and a @nextToken@ value if
-- applicable.
ltrqMaxResults :: Lens' ListTasks (Maybe Int)
ltrqMaxResults = lens _ltrqMaxResults (\ s a -> s{_ltrqMaxResults = a});

instance AWSPager ListTasks where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTaskARNs) = Nothing
          | otherwise =
            Just $ rq & ltrqNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListTasks where
        type Sv ListTasks = ECS
        type Rs ListTasks = ListTasksResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTasksResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "taskArns" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["desiredStatus" .= _ltrqDesiredStatus,
               "cluster" .= _ltrqCluster, "family" .= _ltrqFamily,
               "nextToken" .= _ltrqNextToken,
               "startedBy" .= _ltrqStartedBy,
               "serviceName" .= _ltrqServiceName,
               "containerInstance" .= _ltrqContainerInstance,
               "maxResults" .= _ltrqMaxResults]

instance ToPath ListTasks where
        toPath = const "/"

instance ToQuery ListTasks where
        toQuery = const mempty

-- | /See:/ 'listTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltrsNextToken'
--
-- * 'ltrsTaskARNs'
--
-- * 'ltrsStatus'
data ListTasksResponse = ListTasksResponse'
    { _ltrsNextToken :: !(Maybe Text)
    , _ltrsTaskARNs  :: !(Maybe [Text])
    , _ltrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTasksResponse' smart constructor.
listTasksResponse :: Int -> ListTasksResponse
listTasksResponse pStatus_ =
    ListTasksResponse'
    { _ltrsNextToken = Nothing
    , _ltrsTaskARNs = Nothing
    , _ltrsStatus = pStatus_
    }

-- | The @nextToken@ value to include in a future @ListTasks@ request. When
-- the results of a @ListTasks@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
ltrsNextToken :: Lens' ListTasksResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a});

-- | The list of task Amazon Resource Name (ARN) entries for the @ListTasks@
-- request.
ltrsTaskARNs :: Lens' ListTasksResponse [Text]
ltrsTaskARNs = lens _ltrsTaskARNs (\ s a -> s{_ltrsTaskARNs = a}) . _Default;

-- | FIXME: Undocumented member.
ltrsStatus :: Lens' ListTasksResponse Int
ltrsStatus = lens _ltrsStatus (\ s a -> s{_ltrsStatus = a});
