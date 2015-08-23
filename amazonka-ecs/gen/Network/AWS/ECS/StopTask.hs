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
-- Module      : Network.AWS.ECS.StopTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running task.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StopTask.html AWS API Reference> for StopTask.
module Network.AWS.ECS.StopTask
    (
    -- * Creating a Request
      stopTask
    , StopTask
    -- * Request Lenses
    , stCluster
    , stTask

    -- * Destructuring the Response
    , stopTaskResponse
    , StopTaskResponse
    -- * Response Lenses
    , srsTask
    , srsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'stopTask' smart constructor.
data StopTask = StopTask'
    { _stCluster :: !(Maybe Text)
    , _stTask    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stCluster'
--
-- * 'stTask'
stopTask
    :: Text -- ^ 'stTask'
    -> StopTask
stopTask pTask_ =
    StopTask'
    { _stCluster = Nothing
    , _stTask = pTask_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task you want to stop. If you do not specify a cluster, the
-- default cluster is assumed..
stCluster :: Lens' StopTask (Maybe Text)
stCluster = lens _stCluster (\ s a -> s{_stCluster = a});

-- | The task UUIDs or full Amazon Resource Name (ARN) entry of the task you
-- would like to stop.
stTask :: Lens' StopTask Text
stTask = lens _stTask (\ s a -> s{_stTask = a});

instance AWSRequest StopTask where
        type Sv StopTask = ECS
        type Rs StopTask = StopTaskResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 StopTaskResponse' <$>
                   (x .?> "task") <*> (pure (fromEnum s)))

instance ToHeaders StopTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.StopTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopTask where
        toJSON StopTask'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _stCluster,
                  Just ("task" .= _stTask)])

instance ToPath StopTask where
        toPath = const "/"

instance ToQuery StopTask where
        toQuery = const mempty

-- | /See:/ 'stopTaskResponse' smart constructor.
data StopTaskResponse = StopTaskResponse'
    { _srsTask   :: !(Maybe Task)
    , _srsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsTask'
--
-- * 'srsStatus'
stopTaskResponse
    :: Int -- ^ 'srsStatus'
    -> StopTaskResponse
stopTaskResponse pStatus_ =
    StopTaskResponse'
    { _srsTask = Nothing
    , _srsStatus = pStatus_
    }

-- | Undocumented member.
srsTask :: Lens' StopTaskResponse (Maybe Task)
srsTask = lens _srsTask (\ s a -> s{_srsTask = a});

-- | The response status code.
srsStatus :: Lens' StopTaskResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
