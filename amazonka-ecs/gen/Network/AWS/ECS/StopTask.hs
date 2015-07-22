{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.StopTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Stops a running task.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StopTask.html>
module Network.AWS.ECS.StopTask
    (
    -- * Request
      StopTask
    -- ** Request constructor
    , stopTask
    -- ** Request lenses
    , strqCluster
    , strqTask

    -- * Response
    , StopTaskResponse
    -- ** Response constructor
    , stopTaskResponse
    -- ** Response lenses
    , strsTask
    , strsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'stopTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'strqCluster'
--
-- * 'strqTask'
data StopTask = StopTask'
    { _strqCluster :: !(Maybe Text)
    , _strqTask    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopTask' smart constructor.
stopTask :: Text -> StopTask
stopTask pTask =
    StopTask'
    { _strqCluster = Nothing
    , _strqTask = pTask
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task you want to stop. If you do not specify a cluster, the
-- default cluster is assumed..
strqCluster :: Lens' StopTask (Maybe Text)
strqCluster = lens _strqCluster (\ s a -> s{_strqCluster = a});

-- | The task UUIDs or full Amazon Resource Name (ARN) entry of the task you
-- would like to stop.
strqTask :: Lens' StopTask Text
strqTask = lens _strqTask (\ s a -> s{_strqTask = a});

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
              ["cluster" .= _strqCluster, "task" .= _strqTask]

instance ToPath StopTask where
        toPath = const "/"

instance ToQuery StopTask where
        toQuery = const mempty

-- | /See:/ 'stopTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'strsTask'
--
-- * 'strsStatus'
data StopTaskResponse = StopTaskResponse'
    { _strsTask   :: !(Maybe Task)
    , _strsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StopTaskResponse' smart constructor.
stopTaskResponse :: Int -> StopTaskResponse
stopTaskResponse pStatus =
    StopTaskResponse'
    { _strsTask = Nothing
    , _strsStatus = pStatus
    }

-- | FIXME: Undocumented member.
strsTask :: Lens' StopTaskResponse (Maybe Task)
strsTask = lens _strsTask (\ s a -> s{_strsTask = a});

-- | FIXME: Undocumented member.
strsStatus :: Lens' StopTaskResponse Int
strsStatus = lens _strsStatus (\ s a -> s{_strsStatus = a});
