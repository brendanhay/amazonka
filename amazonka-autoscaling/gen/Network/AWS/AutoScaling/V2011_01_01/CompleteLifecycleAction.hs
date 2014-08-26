{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.CompleteLifecycleAction
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
module Network.AWS.AutoScaling.V2011_01_01.CompleteLifecycleAction where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data CompleteLifecycleAction = CompleteLifecycleAction
    { _clatLifecycleHookName :: Text
      -- ^ The name of the lifecycle hook.
    , _clatLifecycleActionResult :: Text
      -- ^ The action the Auto Scaling group should take. The value for this
      -- parameter can be either CONTINUE or ABANDON.
    , _clatLifecycleActionToken :: Text
      -- ^ A universally unique identifier (UUID) that identifies a specific
      -- lifecycle action associated with an instance. Auto Scaling sends
      -- this token to the notification target you specified when you
      -- created the lifecycle hook.
    , _clatAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which the lifecycle hook
      -- belongs.
    } deriving (Show, Generic)

makeLenses ''CompleteLifecycleAction

instance ToQuery CompleteLifecycleAction where
    toQuery = genericQuery def

data CompleteLifecycleActionResponse = CompleteLifecycleActionResponse
    deriving (Eq, Show, Generic)

makeLenses ''CompleteLifecycleActionResponse

instance AWSRequest CompleteLifecycleAction where
    type Sv CompleteLifecycleAction = AutoScaling
    type Rs CompleteLifecycleAction = CompleteLifecycleActionResponse

    request = post "CompleteLifecycleAction"
    response _ = nullaryResponse CompleteLifecycleActionResponse
