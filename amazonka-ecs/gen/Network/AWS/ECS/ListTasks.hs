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
-- Module      : Network.AWS.ECS.ListTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tasks for a specified cluster. You can filter the
-- results by family name, by a particular container instance, or by the
-- desired status of the task with the 'family', 'containerInstance', and
-- 'desiredStatus' parameters.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTasks.html AWS API Reference> for ListTasks.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTasks
    (
    -- * Creating a Request
      listTasks
    , ListTasks
    -- * Request Lenses
    , ltDesiredStatus
    , ltCluster
    , ltFamily
    , ltNextToken
    , ltStartedBy
    , ltServiceName
    , ltContainerInstance
    , ltMaxResults

    -- * Destructuring the Response
    , listTasksResponse
    , ListTasksResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsTaskARNs
    , ltrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listTasks' smart constructor.
data ListTasks = ListTasks'
    { _ltDesiredStatus     :: !(Maybe DesiredStatus)
    , _ltCluster           :: !(Maybe Text)
    , _ltFamily            :: !(Maybe Text)
    , _ltNextToken         :: !(Maybe Text)
    , _ltStartedBy         :: !(Maybe Text)
    , _ltServiceName       :: !(Maybe Text)
    , _ltContainerInstance :: !(Maybe Text)
    , _ltMaxResults        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
listTasks
    :: ListTasks
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

-- | The task status that you want to filter the 'ListTasks' results with.
-- Specifying a 'desiredStatus' of 'STOPPED' will limit the results to
-- tasks that are in the 'STOPPED' status, which can be useful for
-- debugging tasks that are not starting properly or have died or finished.
-- The default status filter is 'RUNNING'.
ltDesiredStatus :: Lens' ListTasks (Maybe DesiredStatus)
ltDesiredStatus = lens _ltDesiredStatus (\ s a -> s{_ltDesiredStatus = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the tasks you want to list. If you do not specify a cluster, the
-- default cluster is assumed..
ltCluster :: Lens' ListTasks (Maybe Text)
ltCluster = lens _ltCluster (\ s a -> s{_ltCluster = a});

-- | The name of the family that you want to filter the 'ListTasks' results
-- with. Specifying a 'family' will limit the results to tasks that belong
-- to that family.
ltFamily :: Lens' ListTasks (Maybe Text)
ltFamily = lens _ltFamily (\ s a -> s{_ltFamily = a});

-- | The 'nextToken' value returned from a previous paginated 'ListTasks'
-- request where 'maxResults' was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the 'nextToken' value. This value is 'null' when
-- there are no more results to return.
ltNextToken :: Lens' ListTasks (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a});

-- | The 'startedBy' value that you want to filter the task results with.
-- Specifying a 'startedBy' value will limit the results to tasks that were
-- started with that value.
ltStartedBy :: Lens' ListTasks (Maybe Text)
ltStartedBy = lens _ltStartedBy (\ s a -> s{_ltStartedBy = a});

-- | The name of the service that you want to filter the 'ListTasks' results
-- with. Specifying a 'serviceName' will limit the results to tasks that
-- belong to that service.
ltServiceName :: Lens' ListTasks (Maybe Text)
ltServiceName = lens _ltServiceName (\ s a -> s{_ltServiceName = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance that you want to filter the 'ListTasks' results with.
-- Specifying a 'containerInstance' will limit the results to tasks that
-- belong to that container instance.
ltContainerInstance :: Lens' ListTasks (Maybe Text)
ltContainerInstance = lens _ltContainerInstance (\ s a -> s{_ltContainerInstance = a});

-- | The maximum number of task results returned by 'ListTasks' in paginated
-- output. When this parameter is used, 'ListTasks' only returns
-- 'maxResults' results in a single page along with a 'nextToken' response
-- element. The remaining results of the initial request can be seen by
-- sending another 'ListTasks' request with the returned 'nextToken' value.
-- This value can be between 1 and 100. If this parameter is not used, then
-- 'ListTasks' returns up to 100 results and a 'nextToken' value if
-- applicable.
ltMaxResults :: Lens' ListTasks (Maybe Int)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a});

instance AWSPager ListTasks where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTaskARNs) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

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
              (catMaybes
                 [("desiredStatus" .=) <$> _ltDesiredStatus,
                  ("cluster" .=) <$> _ltCluster,
                  ("family" .=) <$> _ltFamily,
                  ("nextToken" .=) <$> _ltNextToken,
                  ("startedBy" .=) <$> _ltStartedBy,
                  ("serviceName" .=) <$> _ltServiceName,
                  ("containerInstance" .=) <$> _ltContainerInstance,
                  ("maxResults" .=) <$> _ltMaxResults])

instance ToPath ListTasks where
        toPath = const "/"

instance ToQuery ListTasks where
        toQuery = const mempty

-- | /See:/ 'listTasksResponse' smart constructor.
data ListTasksResponse = ListTasksResponse'
    { _ltrsNextToken :: !(Maybe Text)
    , _ltrsTaskARNs  :: !(Maybe [Text])
    , _ltrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken'
--
-- * 'ltrsTaskARNs'
--
-- * 'ltrsStatus'
listTasksResponse
    :: Int -- ^ 'ltrsStatus'
    -> ListTasksResponse
listTasksResponse pStatus_ =
    ListTasksResponse'
    { _ltrsNextToken = Nothing
    , _ltrsTaskARNs = Nothing
    , _ltrsStatus = pStatus_
    }

-- | The 'nextToken' value to include in a future 'ListTasks' request. When
-- the results of a 'ListTasks' request exceed 'maxResults', this value can
-- be used to retrieve the next page of results. This value is 'null' when
-- there are no more results to return.
ltrsNextToken :: Lens' ListTasksResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a});

-- | The list of task Amazon Resource Name (ARN) entries for the 'ListTasks'
-- request.
ltrsTaskARNs :: Lens' ListTasksResponse [Text]
ltrsTaskARNs = lens _ltrsTaskARNs (\ s a -> s{_ltrsTaskARNs = a}) . _Default . _Coerce;

-- | The response status code.
ltrsStatus :: Lens' ListTasksResponse Int
ltrsStatus = lens _ltrsStatus (\ s a -> s{_ltrsStatus = a});
