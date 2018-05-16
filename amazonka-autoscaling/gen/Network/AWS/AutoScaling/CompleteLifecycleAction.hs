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
-- Module      : Network.AWS.AutoScaling.CompleteLifecycleAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes the lifecycle action for the specified token or instance with the specified result.
--
--
-- This step is a part of the procedure for adding a lifecycle hook to an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Auto Scaling launches or terminates instances.
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Auto Scaling to publish lifecycle notifications to the target.
--
--     * Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.
--
--     * If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.
--
--     * __If you finish before the timeout period ends, complete the lifecycle action.__
--
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/AutoScalingGroupLifecycle.html Auto Scaling Lifecycle> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.CompleteLifecycleAction
    (
    -- * Creating a Request
      completeLifecycleAction
    , CompleteLifecycleAction
    -- * Request Lenses
    , claInstanceId
    , claLifecycleActionToken
    , claLifecycleHookName
    , claAutoScalingGroupName
    , claLifecycleActionResult

    -- * Destructuring the Response
    , completeLifecycleActionResponse
    , CompleteLifecycleActionResponse
    -- * Response Lenses
    , clarsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'completeLifecycleAction' smart constructor.
data CompleteLifecycleAction = CompleteLifecycleAction'
  { _claInstanceId            :: !(Maybe Text)
  , _claLifecycleActionToken  :: !(Maybe Text)
  , _claLifecycleHookName     :: !Text
  , _claAutoScalingGroupName  :: !Text
  , _claLifecycleActionResult :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompleteLifecycleAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'claInstanceId' - The ID of the instance.
--
-- * 'claLifecycleActionToken' - A universally unique identifier (UUID) that identifies a specific lifecycle action associated with an instance. Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
--
-- * 'claLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'claAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'claLifecycleActionResult' - The action for the group to take. This parameter can be either @CONTINUE@ or @ABANDON@ .
completeLifecycleAction
    :: Text -- ^ 'claLifecycleHookName'
    -> Text -- ^ 'claAutoScalingGroupName'
    -> Text -- ^ 'claLifecycleActionResult'
    -> CompleteLifecycleAction
completeLifecycleAction pLifecycleHookName_ pAutoScalingGroupName_ pLifecycleActionResult_ =
  CompleteLifecycleAction'
    { _claInstanceId = Nothing
    , _claLifecycleActionToken = Nothing
    , _claLifecycleHookName = pLifecycleHookName_
    , _claAutoScalingGroupName = pAutoScalingGroupName_
    , _claLifecycleActionResult = pLifecycleActionResult_
    }


-- | The ID of the instance.
claInstanceId :: Lens' CompleteLifecycleAction (Maybe Text)
claInstanceId = lens _claInstanceId (\ s a -> s{_claInstanceId = a})

-- | A universally unique identifier (UUID) that identifies a specific lifecycle action associated with an instance. Auto Scaling sends this token to the notification target you specified when you created the lifecycle hook.
claLifecycleActionToken :: Lens' CompleteLifecycleAction (Maybe Text)
claLifecycleActionToken = lens _claLifecycleActionToken (\ s a -> s{_claLifecycleActionToken = a})

-- | The name of the lifecycle hook.
claLifecycleHookName :: Lens' CompleteLifecycleAction Text
claLifecycleHookName = lens _claLifecycleHookName (\ s a -> s{_claLifecycleHookName = a})

-- | The name of the Auto Scaling group.
claAutoScalingGroupName :: Lens' CompleteLifecycleAction Text
claAutoScalingGroupName = lens _claAutoScalingGroupName (\ s a -> s{_claAutoScalingGroupName = a})

-- | The action for the group to take. This parameter can be either @CONTINUE@ or @ABANDON@ .
claLifecycleActionResult :: Lens' CompleteLifecycleAction Text
claLifecycleActionResult = lens _claLifecycleActionResult (\ s a -> s{_claLifecycleActionResult = a})

instance AWSRequest CompleteLifecycleAction where
        type Rs CompleteLifecycleAction =
             CompleteLifecycleActionResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "CompleteLifecycleActionResult"
              (\ s h x ->
                 CompleteLifecycleActionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CompleteLifecycleAction where

instance NFData CompleteLifecycleAction where

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
               "InstanceId" =: _claInstanceId,
               "LifecycleActionToken" =: _claLifecycleActionToken,
               "LifecycleHookName" =: _claLifecycleHookName,
               "AutoScalingGroupName" =: _claAutoScalingGroupName,
               "LifecycleActionResult" =: _claLifecycleActionResult]

-- | /See:/ 'completeLifecycleActionResponse' smart constructor.
newtype CompleteLifecycleActionResponse = CompleteLifecycleActionResponse'
  { _clarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompleteLifecycleActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clarsResponseStatus' - -- | The response status code.
completeLifecycleActionResponse
    :: Int -- ^ 'clarsResponseStatus'
    -> CompleteLifecycleActionResponse
completeLifecycleActionResponse pResponseStatus_ =
  CompleteLifecycleActionResponse' {_clarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
clarsResponseStatus :: Lens' CompleteLifecycleActionResponse Int
clarsResponseStatus = lens _clarsResponseStatus (\ s a -> s{_clarsResponseStatus = a})

instance NFData CompleteLifecycleActionResponse where
