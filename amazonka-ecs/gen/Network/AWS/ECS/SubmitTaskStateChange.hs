{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitTaskStateChange
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon EC2 Container Service agent, and
-- it is not intended for use outside of the agent.
--
-- Sent to acknowledge that a task changed states.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitTaskStateChange.html>
module Network.AWS.ECS.SubmitTaskStateChange
    (
    -- * Request
      SubmitTaskStateChange
    -- ** Request constructor
    , submitTaskStateChange
    -- ** Request lenses
    , stscrqStatus
    , stscrqCluster
    , stscrqReason
    , stscrqTask

    -- * Response
    , SubmitTaskStateChangeResponse
    -- ** Response constructor
    , submitTaskStateChangeResponse
    -- ** Response lenses
    , stscrsAcknowledgment
    , stscrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'submitTaskStateChange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stscrqStatus'
--
-- * 'stscrqCluster'
--
-- * 'stscrqReason'
--
-- * 'stscrqTask'
data SubmitTaskStateChange = SubmitTaskStateChange'
    { _stscrqStatus  :: !(Maybe Text)
    , _stscrqCluster :: !(Maybe Text)
    , _stscrqReason  :: !(Maybe Text)
    , _stscrqTask    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubmitTaskStateChange' smart constructor.
submitTaskStateChange :: SubmitTaskStateChange
submitTaskStateChange =
    SubmitTaskStateChange'
    { _stscrqStatus = Nothing
    , _stscrqCluster = Nothing
    , _stscrqReason = Nothing
    , _stscrqTask = Nothing
    }

-- | The status of the state change request.
stscrqStatus :: Lens' SubmitTaskStateChange (Maybe Text)
stscrqStatus = lens _stscrqStatus (\ s a -> s{_stscrqStatus = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
stscrqCluster :: Lens' SubmitTaskStateChange (Maybe Text)
stscrqCluster = lens _stscrqCluster (\ s a -> s{_stscrqCluster = a});

-- | The reason for the state change request.
stscrqReason :: Lens' SubmitTaskStateChange (Maybe Text)
stscrqReason = lens _stscrqReason (\ s a -> s{_stscrqReason = a});

-- | The task UUID or full Amazon Resource Name (ARN) of the task in the
-- state change request.
stscrqTask :: Lens' SubmitTaskStateChange (Maybe Text)
stscrqTask = lens _stscrqTask (\ s a -> s{_stscrqTask = a});

instance AWSRequest SubmitTaskStateChange where
        type Sv SubmitTaskStateChange = ECS
        type Rs SubmitTaskStateChange =
             SubmitTaskStateChangeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 SubmitTaskStateChangeResponse' <$>
                   (x .?> "acknowledgment") <*> (pure (fromEnum s)))

instance ToHeaders SubmitTaskStateChange where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.SubmitTaskStateChange"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SubmitTaskStateChange where
        toJSON SubmitTaskStateChange'{..}
          = object
              ["status" .= _stscrqStatus,
               "cluster" .= _stscrqCluster,
               "reason" .= _stscrqReason, "task" .= _stscrqTask]

instance ToPath SubmitTaskStateChange where
        toPath = const "/"

instance ToQuery SubmitTaskStateChange where
        toQuery = const mempty

-- | /See:/ 'submitTaskStateChangeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stscrsAcknowledgment'
--
-- * 'stscrsStatus'
data SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse'
    { _stscrsAcknowledgment :: !(Maybe Text)
    , _stscrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubmitTaskStateChangeResponse' smart constructor.
submitTaskStateChangeResponse :: Int -> SubmitTaskStateChangeResponse
submitTaskStateChangeResponse pStatus_ =
    SubmitTaskStateChangeResponse'
    { _stscrsAcknowledgment = Nothing
    , _stscrsStatus = pStatus_
    }

-- | Acknowledgement of the state change.
stscrsAcknowledgment :: Lens' SubmitTaskStateChangeResponse (Maybe Text)
stscrsAcknowledgment = lens _stscrsAcknowledgment (\ s a -> s{_stscrsAcknowledgment = a});

-- | FIXME: Undocumented member.
stscrsStatus :: Lens' SubmitTaskStateChangeResponse Int
stscrsStatus = lens _stscrsStatus (\ s a -> s{_stscrsStatus = a});
