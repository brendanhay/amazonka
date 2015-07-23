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
    , clarqLifecycleHookName
    , clarqAutoScalingGroupName
    , clarqLifecycleActionToken
    , clarqLifecycleActionResult

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
-- * 'clarqLifecycleHookName'
--
-- * 'clarqAutoScalingGroupName'
--
-- * 'clarqLifecycleActionToken'
--
-- * 'clarqLifecycleActionResult'
data CompleteLifecycleAction = CompleteLifecycleAction'
    { _clarqLifecycleHookName     :: !Text
    , _clarqAutoScalingGroupName  :: !Text
    , _clarqLifecycleActionToken  :: !Text
    , _clarqLifecycleActionResult :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompleteLifecycleAction' smart constructor.
completeLifecycleAction :: Text -> Text -> Text -> Text -> CompleteLifecycleAction
completeLifecycleAction pLifecycleHookName_ pAutoScalingGroupName_ pLifecycleActionToken_ pLifecycleActionResult_ =
    CompleteLifecycleAction'
    { _clarqLifecycleHookName = pLifecycleHookName_
    , _clarqAutoScalingGroupName = pAutoScalingGroupName_
    , _clarqLifecycleActionToken = pLifecycleActionToken_
    , _clarqLifecycleActionResult = pLifecycleActionResult_
    }

-- | The name of the lifecycle hook.
clarqLifecycleHookName :: Lens' CompleteLifecycleAction Text
clarqLifecycleHookName = lens _clarqLifecycleHookName (\ s a -> s{_clarqLifecycleHookName = a});

-- | The name of the group for the lifecycle hook.
clarqAutoScalingGroupName :: Lens' CompleteLifecycleAction Text
clarqAutoScalingGroupName = lens _clarqAutoScalingGroupName (\ s a -> s{_clarqAutoScalingGroupName = a});

-- | A universally unique identifier (UUID) that identifies a specific
-- lifecycle action associated with an instance. Auto Scaling sends this
-- token to the notification target you specified when you created the
-- lifecycle hook.
clarqLifecycleActionToken :: Lens' CompleteLifecycleAction Text
clarqLifecycleActionToken = lens _clarqLifecycleActionToken (\ s a -> s{_clarqLifecycleActionToken = a});

-- | The action for the group to take. This parameter can be either
-- @CONTINUE@ or @ABANDON@.
clarqLifecycleActionResult :: Lens' CompleteLifecycleAction Text
clarqLifecycleActionResult = lens _clarqLifecycleActionResult (\ s a -> s{_clarqLifecycleActionResult = a});

instance AWSRequest CompleteLifecycleAction where
        type Sv CompleteLifecycleAction = AutoScaling
        type Rs CompleteLifecycleAction =
             CompleteLifecycleActionResponse
        request = post
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
               "LifecycleHookName" =: _clarqLifecycleHookName,
               "AutoScalingGroupName" =: _clarqAutoScalingGroupName,
               "LifecycleActionToken" =: _clarqLifecycleActionToken,
               "LifecycleActionResult" =:
                 _clarqLifecycleActionResult]

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
