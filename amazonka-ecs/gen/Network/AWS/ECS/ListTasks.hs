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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tasks for a specified cluster. You can filter the results by family name, by a particular container instance, or by the desired status of the task with the @family@ , @containerInstance@ , and @desiredStatus@ parameters.
--
--
-- Recently stopped tasks might appear in the returned results. Currently, stopped tasks appear in the returned results for at least one hour.
--
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
    , ltLaunchType
    , ltContainerInstance
    , ltMaxResults

    -- * Destructuring the Response
    , listTasksResponse
    , ListTasksResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsTaskARNs
    , ltrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTasks' smart constructor.
data ListTasks = ListTasks'
  { _ltDesiredStatus     :: !(Maybe DesiredStatus)
  , _ltCluster           :: !(Maybe Text)
  , _ltFamily            :: !(Maybe Text)
  , _ltNextToken         :: !(Maybe Text)
  , _ltStartedBy         :: !(Maybe Text)
  , _ltServiceName       :: !(Maybe Text)
  , _ltLaunchType        :: !(Maybe LaunchType)
  , _ltContainerInstance :: !(Maybe Text)
  , _ltMaxResults        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltDesiredStatus' - The task desired status with which to filter the @ListTasks@ results. Specifying a @desiredStatus@ of @STOPPED@ limits the results to tasks that Amazon ECS has set the desired status to @STOPPED@ , which can be useful for debugging tasks that are not starting properly or have died or finished. The default status filter is @RUNNING@ , which shows tasks that Amazon ECS has set the desired status to @RUNNING@ .
--
-- * 'ltCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the tasks to list. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'ltFamily' - The name of the family with which to filter the @ListTasks@ results. Specifying a @family@ limits the results to tasks that belong to that family.
--
-- * 'ltNextToken' - The @nextToken@ value returned from a previous paginated @ListTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- * 'ltStartedBy' - The @startedBy@ value with which to filter the task results. Specifying a @startedBy@ value limits the results to tasks that were started with that value.
--
-- * 'ltServiceName' - The name of the service with which to filter the @ListTasks@ results. Specifying a @serviceName@ limits the results to tasks that belong to that service.
--
-- * 'ltLaunchType' - The launch type for services you want to list.
--
-- * 'ltContainerInstance' - The container instance ID or full ARN of the container instance with which to filter the @ListTasks@ results. Specifying a @containerInstance@ limits the results to tasks that belong to that container instance.
--
-- * 'ltMaxResults' - The maximum number of task results returned by @ListTasks@ in paginated output. When this parameter is used, @ListTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTasks@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTasks@ returns up to 100 results and a @nextToken@ value if applicable.
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
    , _ltLaunchType = Nothing
    , _ltContainerInstance = Nothing
    , _ltMaxResults = Nothing
    }


-- | The task desired status with which to filter the @ListTasks@ results. Specifying a @desiredStatus@ of @STOPPED@ limits the results to tasks that Amazon ECS has set the desired status to @STOPPED@ , which can be useful for debugging tasks that are not starting properly or have died or finished. The default status filter is @RUNNING@ , which shows tasks that Amazon ECS has set the desired status to @RUNNING@ .
ltDesiredStatus :: Lens' ListTasks (Maybe DesiredStatus)
ltDesiredStatus = lens _ltDesiredStatus (\ s a -> s{_ltDesiredStatus = a})

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the tasks to list. If you do not specify a cluster, the default cluster is assumed.
ltCluster :: Lens' ListTasks (Maybe Text)
ltCluster = lens _ltCluster (\ s a -> s{_ltCluster = a})

-- | The name of the family with which to filter the @ListTasks@ results. Specifying a @family@ limits the results to tasks that belong to that family.
ltFamily :: Lens' ListTasks (Maybe Text)
ltFamily = lens _ltFamily (\ s a -> s{_ltFamily = a})

-- | The @nextToken@ value returned from a previous paginated @ListTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
ltNextToken :: Lens' ListTasks (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The @startedBy@ value with which to filter the task results. Specifying a @startedBy@ value limits the results to tasks that were started with that value.
ltStartedBy :: Lens' ListTasks (Maybe Text)
ltStartedBy = lens _ltStartedBy (\ s a -> s{_ltStartedBy = a})

-- | The name of the service with which to filter the @ListTasks@ results. Specifying a @serviceName@ limits the results to tasks that belong to that service.
ltServiceName :: Lens' ListTasks (Maybe Text)
ltServiceName = lens _ltServiceName (\ s a -> s{_ltServiceName = a})

-- | The launch type for services you want to list.
ltLaunchType :: Lens' ListTasks (Maybe LaunchType)
ltLaunchType = lens _ltLaunchType (\ s a -> s{_ltLaunchType = a})

-- | The container instance ID or full ARN of the container instance with which to filter the @ListTasks@ results. Specifying a @containerInstance@ limits the results to tasks that belong to that container instance.
ltContainerInstance :: Lens' ListTasks (Maybe Text)
ltContainerInstance = lens _ltContainerInstance (\ s a -> s{_ltContainerInstance = a})

-- | The maximum number of task results returned by @ListTasks@ in paginated output. When this parameter is used, @ListTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTasks@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTasks@ returns up to 100 results and a @nextToken@ value if applicable.
ltMaxResults :: Lens' ListTasks (Maybe Int)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a})

instance AWSPager ListTasks where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTaskARNs) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListTasks where
        type Rs ListTasks = ListTasksResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 ListTasksResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "taskArns" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTasks where

instance NFData ListTasks where

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
                  ("launchType" .=) <$> _ltLaunchType,
                  ("containerInstance" .=) <$> _ltContainerInstance,
                  ("maxResults" .=) <$> _ltMaxResults])

instance ToPath ListTasks where
        toPath = const "/"

instance ToQuery ListTasks where
        toQuery = const mempty

-- | /See:/ 'listTasksResponse' smart constructor.
data ListTasksResponse = ListTasksResponse'
  { _ltrsNextToken      :: !(Maybe Text)
  , _ltrsTaskARNs       :: !(Maybe [Text])
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken' - The @nextToken@ value to include in a future @ListTasks@ request. When the results of a @ListTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ltrsTaskARNs' - The list of task ARN entries for the @ListTasks@ request.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTasksResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTasksResponse
listTasksResponse pResponseStatus_ =
  ListTasksResponse'
    { _ltrsNextToken = Nothing
    , _ltrsTaskARNs = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | The @nextToken@ value to include in a future @ListTasks@ request. When the results of a @ListTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
ltrsNextToken :: Lens' ListTasksResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | The list of task ARN entries for the @ListTasks@ request.
ltrsTaskARNs :: Lens' ListTasksResponse [Text]
ltrsTaskARNs = lens _ltrsTaskARNs (\ s a -> s{_ltrsTaskARNs = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTasksResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTasksResponse where
