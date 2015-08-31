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
-- Module      : Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a heartbeat for the lifecycle action associated with a specific
-- token. This extends the timeout by the length of time defined by the
-- 'HeartbeatTimeout' parameter of PutLifecycleHook.
--
-- This operation is a part of the basic sequence for adding a lifecycle
-- hook to an Auto Scaling group:
--
-- 1.  Create a notification target. A target can be either an Amazon SQS
--     queue or an Amazon SNS topic.
-- 2.  Create an IAM role. This role allows Auto Scaling to publish
--     lifecycle notifications to the designated SQS queue or SNS topic.
-- 3.  Create the lifecycle hook. You can create a hook that acts when
--     instances launch or when instances terminate.
-- 4.  __If necessary, record the lifecycle action heartbeat to keep the
--     instance in a pending state.__
-- 5.  Complete the lifecycle action.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingPendingState.html Auto Scaling Pending State>
-- and
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingTerminatingState.html Auto Scaling Terminating State>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_RecordLifecycleActionHeartbeat.html AWS API Reference> for RecordLifecycleActionHeartbeat.
module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
    (
    -- * Creating a Request
      recordLifecycleActionHeartbeat
    , RecordLifecycleActionHeartbeat
    -- * Request Lenses
    , rlahLifecycleHookName
    , rlahAutoScalingGroupName
    , rlahLifecycleActionToken

    -- * Destructuring the Response
    , recordLifecycleActionHeartbeatResponse
    , RecordLifecycleActionHeartbeatResponse
    -- * Response Lenses
    , rlahrsResponseStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'recordLifecycleActionHeartbeat' smart constructor.
data RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeat'
    { _rlahLifecycleHookName    :: !Text
    , _rlahAutoScalingGroupName :: !Text
    , _rlahLifecycleActionToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecordLifecycleActionHeartbeat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlahLifecycleHookName'
--
-- * 'rlahAutoScalingGroupName'
--
-- * 'rlahLifecycleActionToken'
recordLifecycleActionHeartbeat
    :: Text -- ^ 'rlahLifecycleHookName'
    -> Text -- ^ 'rlahAutoScalingGroupName'
    -> Text -- ^ 'rlahLifecycleActionToken'
    -> RecordLifecycleActionHeartbeat
recordLifecycleActionHeartbeat pLifecycleHookName_ pAutoScalingGroupName_ pLifecycleActionToken_ =
    RecordLifecycleActionHeartbeat'
    { _rlahLifecycleHookName = pLifecycleHookName_
    , _rlahAutoScalingGroupName = pAutoScalingGroupName_
    , _rlahLifecycleActionToken = pLifecycleActionToken_
    }

-- | The name of the lifecycle hook.
rlahLifecycleHookName :: Lens' RecordLifecycleActionHeartbeat Text
rlahLifecycleHookName = lens _rlahLifecycleHookName (\ s a -> s{_rlahLifecycleHookName = a});

-- | The name of the Auto Scaling group for the hook.
rlahAutoScalingGroupName :: Lens' RecordLifecycleActionHeartbeat Text
rlahAutoScalingGroupName = lens _rlahAutoScalingGroupName (\ s a -> s{_rlahAutoScalingGroupName = a});

-- | A token that uniquely identifies a specific lifecycle action associated
-- with an instance. Auto Scaling sends this token to the notification
-- target you specified when you created the lifecycle hook.
rlahLifecycleActionToken :: Lens' RecordLifecycleActionHeartbeat Text
rlahLifecycleActionToken = lens _rlahLifecycleActionToken (\ s a -> s{_rlahLifecycleActionToken = a});

instance AWSRequest RecordLifecycleActionHeartbeat
         where
        type Rs RecordLifecycleActionHeartbeat =
             RecordLifecycleActionHeartbeatResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "RecordLifecycleActionHeartbeatResult"
              (\ s h x ->
                 RecordLifecycleActionHeartbeatResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders RecordLifecycleActionHeartbeat
         where
        toHeaders = const mempty

instance ToPath RecordLifecycleActionHeartbeat where
        toPath = const "/"

instance ToQuery RecordLifecycleActionHeartbeat where
        toQuery RecordLifecycleActionHeartbeat'{..}
          = mconcat
              ["Action" =:
                 ("RecordLifecycleActionHeartbeat" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "LifecycleHookName" =: _rlahLifecycleHookName,
               "AutoScalingGroupName" =: _rlahAutoScalingGroupName,
               "LifecycleActionToken" =: _rlahLifecycleActionToken]

-- | /See:/ 'recordLifecycleActionHeartbeatResponse' smart constructor.
newtype RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse'
    { _rlahrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecordLifecycleActionHeartbeatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlahrsResponseStatus'
recordLifecycleActionHeartbeatResponse
    :: Int -- ^ 'rlahrsResponseStatus'
    -> RecordLifecycleActionHeartbeatResponse
recordLifecycleActionHeartbeatResponse pResponseStatus_ =
    RecordLifecycleActionHeartbeatResponse'
    { _rlahrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
rlahrsResponseStatus :: Lens' RecordLifecycleActionHeartbeatResponse Int
rlahrsResponseStatus = lens _rlahrsResponseStatus (\ s a -> s{_rlahrsResponseStatus = a});
