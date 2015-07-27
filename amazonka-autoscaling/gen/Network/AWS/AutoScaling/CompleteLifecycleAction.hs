{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CompleteLifecycleAction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Completes the lifecycle action for the associated token initiated under
-- the given lifecycle hook with the specified result.
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
-- 4.  If necessary, record the lifecycle action heartbeat to keep the
--     instance in a pending state.
-- 5.  __Complete the lifecycle action__.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingPendingState.html Auto Scaling Pending State>
-- and
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingTerminatingState.html Auto Scaling Terminating State>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CompleteLifecycleAction.html>
module Network.AWS.AutoScaling.CompleteLifecycleAction
    (
    -- * Request
      CompleteLifecycleAction
    -- ** Request constructor
    , completeLifecycleAction
    -- ** Request lenses
    , claLifecycleHookName
    , claAutoScalingGroupName
    , claLifecycleActionToken
    , claLifecycleActionResult

    -- * Response
    , CompleteLifecycleActionResponse
    -- ** Response constructor
    , completeLifecycleActionResponse
    -- ** Response lenses
    , clarsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'completeLifecycleAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'claLifecycleHookName'
--
-- * 'claAutoScalingGroupName'
--
-- * 'claLifecycleActionToken'
--
-- * 'claLifecycleActionResult'
data CompleteLifecycleAction = CompleteLifecycleAction'
    { _claLifecycleHookName     :: !Text
    , _claAutoScalingGroupName  :: !Text
    , _claLifecycleActionToken  :: !Text
    , _claLifecycleActionResult :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompleteLifecycleAction' smart constructor.
completeLifecycleAction :: Text -> Text -> Text -> Text -> CompleteLifecycleAction
completeLifecycleAction pLifecycleHookName_ pAutoScalingGroupName_ pLifecycleActionToken_ pLifecycleActionResult_ =
    CompleteLifecycleAction'
    { _claLifecycleHookName = pLifecycleHookName_
    , _claAutoScalingGroupName = pAutoScalingGroupName_
    , _claLifecycleActionToken = pLifecycleActionToken_
    , _claLifecycleActionResult = pLifecycleActionResult_
    }

-- | The name of the lifecycle hook.
claLifecycleHookName :: Lens' CompleteLifecycleAction Text
claLifecycleHookName = lens _claLifecycleHookName (\ s a -> s{_claLifecycleHookName = a});

-- | The name of the group for the lifecycle hook.
claAutoScalingGroupName :: Lens' CompleteLifecycleAction Text
claAutoScalingGroupName = lens _claAutoScalingGroupName (\ s a -> s{_claAutoScalingGroupName = a});

-- | A universally unique identifier (UUID) that identifies a specific
-- lifecycle action associated with an instance. Auto Scaling sends this
-- token to the notification target you specified when you created the
-- lifecycle hook.
claLifecycleActionToken :: Lens' CompleteLifecycleAction Text
claLifecycleActionToken = lens _claLifecycleActionToken (\ s a -> s{_claLifecycleActionToken = a});

-- | The action for the group to take. This parameter can be either
-- @CONTINUE@ or @ABANDON@.
claLifecycleActionResult :: Lens' CompleteLifecycleAction Text
claLifecycleActionResult = lens _claLifecycleActionResult (\ s a -> s{_claLifecycleActionResult = a});

instance AWSRequest CompleteLifecycleAction where
        type Sv CompleteLifecycleAction = AutoScaling
        type Rs CompleteLifecycleAction =
             CompleteLifecycleActionResponse
        request = postQuery
        response
          = receiveXMLWrapper "CompleteLifecycleActionResult"
              (\ s h x ->
                 CompleteLifecycleActionResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders CompleteLifecycleAction where
        toHeaders = const mempty

instance ToPath CompleteLifecycleAction where
        toPath = const "/"

instance ToQuery CompleteLifecycleAction where
        toQuery CompleteLifecycleAction'{..}
          = mconcat
              ["Action" =:
                 ("CompleteLifecycleAction" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "LifecycleHookName" =: _claLifecycleHookName,
               "AutoScalingGroupName" =: _claAutoScalingGroupName,
               "LifecycleActionToken" =: _claLifecycleActionToken,
               "LifecycleActionResult" =: _claLifecycleActionResult]

-- | /See:/ 'completeLifecycleActionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clarsStatus'
newtype CompleteLifecycleActionResponse = CompleteLifecycleActionResponse'
    { _clarsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompleteLifecycleActionResponse' smart constructor.
completeLifecycleActionResponse :: Int -> CompleteLifecycleActionResponse
completeLifecycleActionResponse pStatus_ =
    CompleteLifecycleActionResponse'
    { _clarsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
clarsStatus :: Lens' CompleteLifecycleActionResponse Int
clarsStatus = lens _clarsStatus (\ s a -> s{_clarsStatus = a});
