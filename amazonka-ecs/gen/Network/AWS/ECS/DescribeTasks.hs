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
-- Module      : Network.AWS.ECS.DescribeTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specified task or tasks.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTasks.html AWS API Reference> for DescribeTasks.
module Network.AWS.ECS.DescribeTasks
    (
    -- * Creating a Request
      describeTasks
    , DescribeTasks
    -- * Request Lenses
    , dtCluster
    , dtTasks

    -- * Destructuring the Response
    , describeTasksResponse
    , DescribeTasksResponse
    -- * Response Lenses
    , dtrsFailures
    , dtrsTasks
    , dtrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTasks' smart constructor.
data DescribeTasks = DescribeTasks'
    { _dtCluster :: !(Maybe Text)
    , _dtTasks   :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtCluster'
--
-- * 'dtTasks'
describeTasks
    :: DescribeTasks
describeTasks =
    DescribeTasks'
    { _dtCluster = Nothing
    , _dtTasks = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task you want to describe. If you do not specify a cluster,
-- the default cluster is assumed.
dtCluster :: Lens' DescribeTasks (Maybe Text)
dtCluster = lens _dtCluster (\ s a -> s{_dtCluster = a});

-- | A space-separated list of task UUIDs or full Amazon Resource Name (ARN)
-- entries.
dtTasks :: Lens' DescribeTasks [Text]
dtTasks = lens _dtTasks (\ s a -> s{_dtTasks = a}) . _Coerce;

instance AWSRequest DescribeTasks where
        type Rs DescribeTasks = DescribeTasksResponse
        request = postJSON eCS
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTasksResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "tasks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DescribeTasks"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTasks where
        toJSON DescribeTasks'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _dtCluster,
                  Just ("tasks" .= _dtTasks)])

instance ToPath DescribeTasks where
        toPath = const "/"

instance ToQuery DescribeTasks where
        toQuery = const mempty

-- | /See:/ 'describeTasksResponse' smart constructor.
data DescribeTasksResponse = DescribeTasksResponse'
    { _dtrsFailures :: !(Maybe [Failure])
    , _dtrsTasks    :: !(Maybe [Task])
    , _dtrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsFailures'
--
-- * 'dtrsTasks'
--
-- * 'dtrsStatus'
describeTasksResponse
    :: Int -- ^ 'dtrsStatus'
    -> DescribeTasksResponse
describeTasksResponse pStatus_ =
    DescribeTasksResponse'
    { _dtrsFailures = Nothing
    , _dtrsTasks = Nothing
    , _dtrsStatus = pStatus_
    }

-- | Undocumented member.
dtrsFailures :: Lens' DescribeTasksResponse [Failure]
dtrsFailures = lens _dtrsFailures (\ s a -> s{_dtrsFailures = a}) . _Default . _Coerce;

-- | The list of tasks.
dtrsTasks :: Lens' DescribeTasksResponse [Task]
dtrsTasks = lens _dtrsTasks (\ s a -> s{_dtrsTasks = a}) . _Default . _Coerce;

-- | The response status code.
dtrsStatus :: Lens' DescribeTasksResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
