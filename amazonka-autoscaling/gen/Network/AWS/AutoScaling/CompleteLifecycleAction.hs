{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Completes the lifecycle action for the associated token initiated under the
-- given lifecycle hook with the specified result. This operation is a part of
-- the basic sequence for adding a lifecycle hook to an Auto Scaling group:
-- Create a notification target. A target can be either an Amazon SQS queue or
-- an Amazon SNS topic. Create an IAM role. This role allows Auto Scaling to
-- publish lifecycle notifications to the designated SQS queue or SNS topic.
-- Create the lifecycle hook. You can create a hook that acts when instances
-- launch or when instances terminate. If necessary, record the lifecycle
-- action heartbeat to keep the instance in a pending state. Complete the
-- lifecycle action. To learn more, see Auto Scaling Pending State and Auto
-- Scaling Terminating State.
module Network.AWS.AutoScaling
    (
    -- * Request
      CompleteLifecycleAction
    -- ** Request constructor
    , mkCompleteLifecycleAction
    -- ** Request lenses
    , claLifecycleHookName
    , claAutoScalingGroupName
    , claLifecycleActionToken
    , claLifecycleActionResult

    -- * Response
    , CompleteLifecycleActionResponse
    -- ** Response constructor
    , mkCompleteLifecycleActionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data CompleteLifecycleAction = CompleteLifecycleAction
    { _claLifecycleHookName :: !Text
    , _claAutoScalingGroupName :: !Text
    , _claLifecycleActionToken :: !Text
    , _claLifecycleActionResult :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CompleteLifecycleAction' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LifecycleHookName ::@ @Text@
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @LifecycleActionToken ::@ @Text@
--
-- * @LifecycleActionResult ::@ @Text@
--
mkCompleteLifecycleAction :: Text -- ^ 'claLifecycleHookName'
                          -> Text -- ^ 'claAutoScalingGroupName'
                          -> Text -- ^ 'claLifecycleActionToken'
                          -> Text -- ^ 'claLifecycleActionResult'
                          -> CompleteLifecycleAction
mkCompleteLifecycleAction p1 p2 p3 p4 = CompleteLifecycleAction
    { _claLifecycleHookName = p1
    , _claAutoScalingGroupName = p2
    , _claLifecycleActionToken = p3
    , _claLifecycleActionResult = p4
    }

-- | The name of the lifecycle hook.
claLifecycleHookName :: Lens' CompleteLifecycleAction Text
claLifecycleHookName =
    lens _claLifecycleHookName (\s a -> s { _claLifecycleHookName = a })

-- | The name of the Auto Scaling group to which the lifecycle hook belongs.
claAutoScalingGroupName :: Lens' CompleteLifecycleAction Text
claAutoScalingGroupName =
    lens _claAutoScalingGroupName
         (\s a -> s { _claAutoScalingGroupName = a })

-- | A universally unique identifier (UUID) that identifies a specific lifecycle
-- action associated with an instance. Auto Scaling sends this token to the
-- notification target you specified when you created the lifecycle hook.
claLifecycleActionToken :: Lens' CompleteLifecycleAction Text
claLifecycleActionToken =
    lens _claLifecycleActionToken
         (\s a -> s { _claLifecycleActionToken = a })

-- | The action the Auto Scaling group should take. The value for this parameter
-- can be either CONTINUE or ABANDON.
claLifecycleActionResult :: Lens' CompleteLifecycleAction Text
claLifecycleActionResult =
    lens _claLifecycleActionResult
         (\s a -> s { _claLifecycleActionResult = a })

instance ToQuery CompleteLifecycleAction where
    toQuery = genericQuery def

-- | The output of the CompleteLifecycleAction.
data CompleteLifecycleActionResponse = CompleteLifecycleActionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CompleteLifecycleActionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCompleteLifecycleActionResponse :: CompleteLifecycleActionResponse
mkCompleteLifecycleActionResponse = CompleteLifecycleActionResponse

instance AWSRequest CompleteLifecycleAction where
    type Sv CompleteLifecycleAction = AutoScaling
    type Rs CompleteLifecycleAction = CompleteLifecycleActionResponse

    request = post "CompleteLifecycleAction"
    response _ = nullaryResponse CompleteLifecycleActionResponse
