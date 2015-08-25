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
-- Module      : Network.AWS.ECS.SubmitTaskStateChange
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon EC2 Container Service agent, and
-- it is not intended for use outside of the agent.
--
-- Sent to acknowledge that a task changed states.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitTaskStateChange.html AWS API Reference> for SubmitTaskStateChange.
module Network.AWS.ECS.SubmitTaskStateChange
    (
    -- * Creating a Request
      submitTaskStateChange
    , SubmitTaskStateChange
    -- * Request Lenses
    , stscStatus
    , stscCluster
    , stscReason
    , stscTask

    -- * Destructuring the Response
    , submitTaskStateChangeResponse
    , SubmitTaskStateChangeResponse
    -- * Response Lenses
    , stscrsAcknowledgment
    , stscrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'submitTaskStateChange' smart constructor.
data SubmitTaskStateChange = SubmitTaskStateChange'
    { _stscStatus  :: !(Maybe Text)
    , _stscCluster :: !(Maybe Text)
    , _stscReason  :: !(Maybe Text)
    , _stscTask    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubmitTaskStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stscStatus'
--
-- * 'stscCluster'
--
-- * 'stscReason'
--
-- * 'stscTask'
submitTaskStateChange
    :: SubmitTaskStateChange
submitTaskStateChange =
    SubmitTaskStateChange'
    { _stscStatus = Nothing
    , _stscCluster = Nothing
    , _stscReason = Nothing
    , _stscTask = Nothing
    }

-- | The status of the state change request.
stscStatus :: Lens' SubmitTaskStateChange (Maybe Text)
stscStatus = lens _stscStatus (\ s a -> s{_stscStatus = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
stscCluster :: Lens' SubmitTaskStateChange (Maybe Text)
stscCluster = lens _stscCluster (\ s a -> s{_stscCluster = a});

-- | The reason for the state change request.
stscReason :: Lens' SubmitTaskStateChange (Maybe Text)
stscReason = lens _stscReason (\ s a -> s{_stscReason = a});

-- | The task UUID or full Amazon Resource Name (ARN) of the task in the
-- state change request.
stscTask :: Lens' SubmitTaskStateChange (Maybe Text)
stscTask = lens _stscTask (\ s a -> s{_stscTask = a});

instance AWSRequest SubmitTaskStateChange where
        type Rs SubmitTaskStateChange =
             SubmitTaskStateChangeResponse
        request = postJSON eCS
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
              (catMaybes
                 [("status" .=) <$> _stscStatus,
                  ("cluster" .=) <$> _stscCluster,
                  ("reason" .=) <$> _stscReason,
                  ("task" .=) <$> _stscTask])

instance ToPath SubmitTaskStateChange where
        toPath = const "/"

instance ToQuery SubmitTaskStateChange where
        toQuery = const mempty

-- | /See:/ 'submitTaskStateChangeResponse' smart constructor.
data SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse'
    { _stscrsAcknowledgment :: !(Maybe Text)
    , _stscrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubmitTaskStateChangeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stscrsAcknowledgment'
--
-- * 'stscrsStatus'
submitTaskStateChangeResponse
    :: Int -- ^ 'stscrsStatus'
    -> SubmitTaskStateChangeResponse
submitTaskStateChangeResponse pStatus_ =
    SubmitTaskStateChangeResponse'
    { _stscrsAcknowledgment = Nothing
    , _stscrsStatus = pStatus_
    }

-- | Acknowledgement of the state change.
stscrsAcknowledgment :: Lens' SubmitTaskStateChangeResponse (Maybe Text)
stscrsAcknowledgment = lens _stscrsAcknowledgment (\ s a -> s{_stscrsAcknowledgment = a});

-- | The response status code.
stscrsStatus :: Lens' SubmitTaskStateChangeResponse Int
stscrsStatus = lens _stscrsStatus (\ s a -> s{_stscrsStatus = a});
