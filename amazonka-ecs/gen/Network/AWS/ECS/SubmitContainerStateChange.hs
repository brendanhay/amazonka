{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitContainerStateChange
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon EC2 Container Service agent, and
-- it is not intended for use outside of the agent.
--
-- Sent to acknowledge that a container changed states.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitContainerStateChange.html>
module Network.AWS.ECS.SubmitContainerStateChange
    (
    -- * Request
      SubmitContainerStateChange
    -- ** Request constructor
    , submitContainerStateChange
    -- ** Request lenses
    , scscrqNetworkBindings
    , scscrqStatus
    , scscrqCluster
    , scscrqContainerName
    , scscrqReason
    , scscrqExitCode
    , scscrqTask

    -- * Response
    , SubmitContainerStateChangeResponse
    -- ** Response constructor
    , submitContainerStateChangeResponse
    -- ** Response lenses
    , scscrsAcknowledgment
    , scscrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'submitContainerStateChange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scscrqNetworkBindings'
--
-- * 'scscrqStatus'
--
-- * 'scscrqCluster'
--
-- * 'scscrqContainerName'
--
-- * 'scscrqReason'
--
-- * 'scscrqExitCode'
--
-- * 'scscrqTask'
data SubmitContainerStateChange = SubmitContainerStateChange'
    { _scscrqNetworkBindings :: !(Maybe [NetworkBinding])
    , _scscrqStatus          :: !(Maybe Text)
    , _scscrqCluster         :: !(Maybe Text)
    , _scscrqContainerName   :: !(Maybe Text)
    , _scscrqReason          :: !(Maybe Text)
    , _scscrqExitCode        :: !(Maybe Int)
    , _scscrqTask            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubmitContainerStateChange' smart constructor.
submitContainerStateChange :: SubmitContainerStateChange
submitContainerStateChange =
    SubmitContainerStateChange'
    { _scscrqNetworkBindings = Nothing
    , _scscrqStatus = Nothing
    , _scscrqCluster = Nothing
    , _scscrqContainerName = Nothing
    , _scscrqReason = Nothing
    , _scscrqExitCode = Nothing
    , _scscrqTask = Nothing
    }

-- | The network bindings of the container.
scscrqNetworkBindings :: Lens' SubmitContainerStateChange [NetworkBinding]
scscrqNetworkBindings = lens _scscrqNetworkBindings (\ s a -> s{_scscrqNetworkBindings = a}) . _Default;

-- | The status of the state change request.
scscrqStatus :: Lens' SubmitContainerStateChange (Maybe Text)
scscrqStatus = lens _scscrqStatus (\ s a -> s{_scscrqStatus = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container.
scscrqCluster :: Lens' SubmitContainerStateChange (Maybe Text)
scscrqCluster = lens _scscrqCluster (\ s a -> s{_scscrqCluster = a});

-- | The name of the container.
scscrqContainerName :: Lens' SubmitContainerStateChange (Maybe Text)
scscrqContainerName = lens _scscrqContainerName (\ s a -> s{_scscrqContainerName = a});

-- | The reason for the state change request.
scscrqReason :: Lens' SubmitContainerStateChange (Maybe Text)
scscrqReason = lens _scscrqReason (\ s a -> s{_scscrqReason = a});

-- | The exit code returned for the state change request.
scscrqExitCode :: Lens' SubmitContainerStateChange (Maybe Int)
scscrqExitCode = lens _scscrqExitCode (\ s a -> s{_scscrqExitCode = a});

-- | The task UUID or full Amazon Resource Name (ARN) of the task that hosts
-- the container.
scscrqTask :: Lens' SubmitContainerStateChange (Maybe Text)
scscrqTask = lens _scscrqTask (\ s a -> s{_scscrqTask = a});

instance AWSRequest SubmitContainerStateChange where
        type Sv SubmitContainerStateChange = ECS
        type Rs SubmitContainerStateChange =
             SubmitContainerStateChangeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 SubmitContainerStateChangeResponse' <$>
                   (x .?> "acknowledgment") <*> (pure (fromEnum s)))

instance ToHeaders SubmitContainerStateChange where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.SubmitContainerStateChange"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SubmitContainerStateChange where
        toJSON SubmitContainerStateChange'{..}
          = object
              ["networkBindings" .= _scscrqNetworkBindings,
               "status" .= _scscrqStatus,
               "cluster" .= _scscrqCluster,
               "containerName" .= _scscrqContainerName,
               "reason" .= _scscrqReason,
               "exitCode" .= _scscrqExitCode, "task" .= _scscrqTask]

instance ToPath SubmitContainerStateChange where
        toPath = const "/"

instance ToQuery SubmitContainerStateChange where
        toQuery = const mempty

-- | /See:/ 'submitContainerStateChangeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scscrsAcknowledgment'
--
-- * 'scscrsStatus'
data SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse'
    { _scscrsAcknowledgment :: !(Maybe Text)
    , _scscrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubmitContainerStateChangeResponse' smart constructor.
submitContainerStateChangeResponse :: Int -> SubmitContainerStateChangeResponse
submitContainerStateChangeResponse pStatus_ =
    SubmitContainerStateChangeResponse'
    { _scscrsAcknowledgment = Nothing
    , _scscrsStatus = pStatus_
    }

-- | Acknowledgement of the state change.
scscrsAcknowledgment :: Lens' SubmitContainerStateChangeResponse (Maybe Text)
scscrsAcknowledgment = lens _scscrsAcknowledgment (\ s a -> s{_scscrsAcknowledgment = a});

-- | FIXME: Undocumented member.
scscrsStatus :: Lens' SubmitContainerStateChangeResponse Int
scscrsStatus = lens _scscrsStatus (\ s a -> s{_scscrsStatus = a});
