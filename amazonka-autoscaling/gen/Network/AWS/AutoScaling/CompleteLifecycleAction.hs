{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.AutoScaling.CompleteLifecycleAction
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
module Network.AWS.AutoScaling.CompleteLifecycleAction
    (
    -- * Request
      CompleteLifecycleActionType
    -- ** Request constructor
    , completeLifecycleActionType
    -- ** Request lenses
    , clatAutoScalingGroupName
    , clatLifecycleActionResult
    , clatLifecycleActionToken
    , clatLifecycleHookName

    -- * Response
    , CompleteLifecycleActionResponse
    -- ** Response constructor
    , completeLifecycleActionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data CompleteLifecycleActionType = CompleteLifecycleActionType
    { _clatAutoScalingGroupName  :: Text
    , _clatLifecycleActionResult :: Text
    , _clatLifecycleActionToken  :: Text
    , _clatLifecycleHookName     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CompleteLifecycleActionType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clatAutoScalingGroupName' @::@ 'Text'
--
-- * 'clatLifecycleActionResult' @::@ 'Text'
--
-- * 'clatLifecycleActionToken' @::@ 'Text'
--
-- * 'clatLifecycleHookName' @::@ 'Text'
--
completeLifecycleActionType :: Text -- ^ 'clatLifecycleHookName'
                            -> Text -- ^ 'clatAutoScalingGroupName'
                            -> Text -- ^ 'clatLifecycleActionToken'
                            -> Text -- ^ 'clatLifecycleActionResult'
                            -> CompleteLifecycleActionType
completeLifecycleActionType p1 p2 p3 p4 = CompleteLifecycleActionType
    { _clatLifecycleHookName     = p1
    , _clatAutoScalingGroupName  = p2
    , _clatLifecycleActionToken  = p3
    , _clatLifecycleActionResult = p4
    }

-- | The name of the Auto Scaling group to which the lifecycle hook belongs.
clatAutoScalingGroupName :: Lens' CompleteLifecycleActionType Text
clatAutoScalingGroupName =
    lens _clatAutoScalingGroupName
        (\s a -> s { _clatAutoScalingGroupName = a })

-- | The action the Auto Scaling group should take. The value for this
-- parameter can be either CONTINUE or ABANDON.
clatLifecycleActionResult :: Lens' CompleteLifecycleActionType Text
clatLifecycleActionResult =
    lens _clatLifecycleActionResult
        (\s a -> s { _clatLifecycleActionResult = a })

-- | A universally unique identifier (UUID) that identifies a specific
-- lifecycle action associated with an instance. Auto Scaling sends this
-- token to the notification target you specified when you created the
-- lifecycle hook.
clatLifecycleActionToken :: Lens' CompleteLifecycleActionType Text
clatLifecycleActionToken =
    lens _clatLifecycleActionToken
        (\s a -> s { _clatLifecycleActionToken = a })

-- | The name of the lifecycle hook.
clatLifecycleHookName :: Lens' CompleteLifecycleActionType Text
clatLifecycleHookName =
    lens _clatLifecycleHookName (\s a -> s { _clatLifecycleHookName = a })

instance ToPath CompleteLifecycleActionType where
    toPath = const "/"

instance ToQuery CompleteLifecycleActionType

data CompleteLifecycleActionResponse = CompleteLifecycleActionResponse

-- | 'CompleteLifecycleActionResponse' constructor.
completeLifecycleActionResponse :: CompleteLifecycleActionResponse
completeLifecycleActionResponse = CompleteLifecycleActionResponse

instance AWSRequest CompleteLifecycleActionType where
    type Sv CompleteLifecycleActionType = AutoScaling
    type Rs CompleteLifecycleActionType = CompleteLifecycleActionResponse

    request  = post "CompleteLifecycleAction"
    response = const (nullaryResponse CompleteLifecycleActionResponse)
