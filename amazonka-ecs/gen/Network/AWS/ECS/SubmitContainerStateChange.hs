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
-- Module      : Network.AWS.ECS.SubmitContainerStateChange
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon EC2 Container Service agent, and
-- it is not intended for use outside of the agent.
--
-- Sent to acknowledge that a container changed states.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitContainerStateChange.html AWS API Reference> for SubmitContainerStateChange.
module Network.AWS.ECS.SubmitContainerStateChange
    (
    -- * Creating a Request
      submitContainerStateChange
    , SubmitContainerStateChange
    -- * Request Lenses
    , scscNetworkBindings
    , scscStatus
    , scscCluster
    , scscContainerName
    , scscReason
    , scscExitCode
    , scscTask

    -- * Destructuring the Response
    , submitContainerStateChangeResponse
    , SubmitContainerStateChangeResponse
    -- * Response Lenses
    , scscrsAcknowledgment
    , scscrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'submitContainerStateChange' smart constructor.
data SubmitContainerStateChange = SubmitContainerStateChange'
    { _scscNetworkBindings :: !(Maybe [NetworkBinding])
    , _scscStatus          :: !(Maybe Text)
    , _scscCluster         :: !(Maybe Text)
    , _scscContainerName   :: !(Maybe Text)
    , _scscReason          :: !(Maybe Text)
    , _scscExitCode        :: !(Maybe Int)
    , _scscTask            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubmitContainerStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scscNetworkBindings'
--
-- * 'scscStatus'
--
-- * 'scscCluster'
--
-- * 'scscContainerName'
--
-- * 'scscReason'
--
-- * 'scscExitCode'
--
-- * 'scscTask'
submitContainerStateChange
    :: SubmitContainerStateChange
submitContainerStateChange =
    SubmitContainerStateChange'
    { _scscNetworkBindings = Nothing
    , _scscStatus = Nothing
    , _scscCluster = Nothing
    , _scscContainerName = Nothing
    , _scscReason = Nothing
    , _scscExitCode = Nothing
    , _scscTask = Nothing
    }

-- | The network bindings of the container.
scscNetworkBindings :: Lens' SubmitContainerStateChange [NetworkBinding]
scscNetworkBindings = lens _scscNetworkBindings (\ s a -> s{_scscNetworkBindings = a}) . _Default . _Coerce;

-- | The status of the state change request.
scscStatus :: Lens' SubmitContainerStateChange (Maybe Text)
scscStatus = lens _scscStatus (\ s a -> s{_scscStatus = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container.
scscCluster :: Lens' SubmitContainerStateChange (Maybe Text)
scscCluster = lens _scscCluster (\ s a -> s{_scscCluster = a});

-- | The name of the container.
scscContainerName :: Lens' SubmitContainerStateChange (Maybe Text)
scscContainerName = lens _scscContainerName (\ s a -> s{_scscContainerName = a});

-- | The reason for the state change request.
scscReason :: Lens' SubmitContainerStateChange (Maybe Text)
scscReason = lens _scscReason (\ s a -> s{_scscReason = a});

-- | The exit code returned for the state change request.
scscExitCode :: Lens' SubmitContainerStateChange (Maybe Int)
scscExitCode = lens _scscExitCode (\ s a -> s{_scscExitCode = a});

-- | The task UUID or full Amazon Resource Name (ARN) of the task that hosts
-- the container.
scscTask :: Lens' SubmitContainerStateChange (Maybe Text)
scscTask = lens _scscTask (\ s a -> s{_scscTask = a});

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
              ["networkBindings" .= _scscNetworkBindings,
               "status" .= _scscStatus, "cluster" .= _scscCluster,
               "containerName" .= _scscContainerName,
               "reason" .= _scscReason, "exitCode" .= _scscExitCode,
               "task" .= _scscTask]

instance ToPath SubmitContainerStateChange where
        toPath = const "/"

instance ToQuery SubmitContainerStateChange where
        toQuery = const mempty

-- | /See:/ 'submitContainerStateChangeResponse' smart constructor.
data SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse'
    { _scscrsAcknowledgment :: !(Maybe Text)
    , _scscrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubmitContainerStateChangeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scscrsAcknowledgment'
--
-- * 'scscrsStatus'
submitContainerStateChangeResponse
    :: Int -- ^ 'scscrsStatus'
    -> SubmitContainerStateChangeResponse
submitContainerStateChangeResponse pStatus_ =
    SubmitContainerStateChangeResponse'
    { _scscrsAcknowledgment = Nothing
    , _scscrsStatus = pStatus_
    }

-- | Acknowledgement of the state change.
scscrsAcknowledgment :: Lens' SubmitContainerStateChangeResponse (Maybe Text)
scscrsAcknowledgment = lens _scscrsAcknowledgment (\ s a -> s{_scscrsAcknowledgment = a});

-- | The response status code.
scscrsStatus :: Lens' SubmitContainerStateChangeResponse Int
scscrsStatus = lens _scscrsStatus (\ s a -> s{_scscrsStatus = a});
