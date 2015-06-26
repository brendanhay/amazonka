{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Records a heartbeat for the lifecycle action associated with a specific
-- token. This extends the timeout by the length of time defined by the
-- @HeartbeatTimeout@ parameter of PutLifecycleHook.
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
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_RecordLifecycleActionHeartbeat.html>
module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
    (
    -- * Request
      RecordLifecycleActionHeartbeat
    -- ** Request constructor
    , recordLifecycleActionHeartbeat
    -- ** Request lenses
    , rlahLifecycleHookName
    , rlahAutoScalingGroupName
    , rlahLifecycleActionToken

    -- * Response
    , RecordLifecycleActionHeartbeatResponse
    -- ** Response constructor
    , recordLifecycleActionHeartbeatResponse
    -- ** Response lenses
    , rlahrStatusCode
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'recordLifecycleActionHeartbeat' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rlahLifecycleHookName'
--
-- * 'rlahAutoScalingGroupName'
--
-- * 'rlahLifecycleActionToken'
data RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeat'{_rlahLifecycleHookName :: Text, _rlahAutoScalingGroupName :: Text, _rlahLifecycleActionToken :: Text} deriving (Eq, Read, Show)

-- | 'RecordLifecycleActionHeartbeat' smart constructor.
recordLifecycleActionHeartbeat :: Text -> Text -> Text -> RecordLifecycleActionHeartbeat
recordLifecycleActionHeartbeat pLifecycleHookName pAutoScalingGroupName pLifecycleActionToken = RecordLifecycleActionHeartbeat'{_rlahLifecycleHookName = pLifecycleHookName, _rlahAutoScalingGroupName = pAutoScalingGroupName, _rlahLifecycleActionToken = pLifecycleActionToken};

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
        type Sv RecordLifecycleActionHeartbeat = AutoScaling
        type Rs RecordLifecycleActionHeartbeat =
             RecordLifecycleActionHeartbeatResponse
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rlahrStatusCode'
newtype RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse'{_rlahrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'RecordLifecycleActionHeartbeatResponse' smart constructor.
recordLifecycleActionHeartbeatResponse :: Int -> RecordLifecycleActionHeartbeatResponse
recordLifecycleActionHeartbeatResponse pStatusCode = RecordLifecycleActionHeartbeatResponse'{_rlahrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
rlahrStatusCode :: Lens' RecordLifecycleActionHeartbeatResponse Int
rlahrStatusCode = lens _rlahrStatusCode (\ s a -> s{_rlahrStatusCode = a});
