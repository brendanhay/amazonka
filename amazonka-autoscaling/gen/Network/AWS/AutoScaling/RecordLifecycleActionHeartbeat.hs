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

-- Module      : Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Records a heartbeat for the lifecycle action associated with a specific
-- token. This extends the timeout by the length of time defined by the
-- HeartbeatTimeout parameter of the PutLifecycleHook operation. This
-- operation is a part of the basic sequence for adding a lifecycle hook to an
-- Auto Scaling group: Create a notification target. A target can be either an
-- Amazon SQS queue or an Amazon SNS topic. Create an IAM role. This role
-- allows Auto Scaling to publish lifecycle notifications to the designated
-- SQS queue or SNS topic. Create the lifecycle hook. You can create a hook
-- that acts when instances launch or when instances terminate. If necessary,
-- record the lifecycle action heartbeat to keep the instance in a pending
-- state. Complete the lifecycle action. To learn more, see Auto Scaling
-- Pending State and Auto Scaling Terminating State.
module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
    (
    -- * Request
      RecordLifecycleActionHeartbeatType
    -- ** Request constructor
    , recordLifecycleActionHeartbeatType
    -- ** Request lenses
    , rlahtAutoScalingGroupName
    , rlahtLifecycleActionToken
    , rlahtLifecycleHookName

    -- * Response
    , RecordLifecycleActionHeartbeatResponse
    -- ** Response constructor
    , recordLifecycleActionHeartbeatResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data RecordLifecycleActionHeartbeatType = RecordLifecycleActionHeartbeatType
    { _rlahtAutoScalingGroupName :: Text
    , _rlahtLifecycleActionToken :: Text
    , _rlahtLifecycleHookName    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RecordLifecycleActionHeartbeatType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rlahtAutoScalingGroupName' @::@ 'Text'
--
-- * 'rlahtLifecycleActionToken' @::@ 'Text'
--
-- * 'rlahtLifecycleHookName' @::@ 'Text'
--
recordLifecycleActionHeartbeatType :: Text -- ^ 'rlahtLifecycleHookName'
                                   -> Text -- ^ 'rlahtAutoScalingGroupName'
                                   -> Text -- ^ 'rlahtLifecycleActionToken'
                                   -> RecordLifecycleActionHeartbeatType
recordLifecycleActionHeartbeatType p1 p2 p3 = RecordLifecycleActionHeartbeatType
    { _rlahtLifecycleHookName    = p1
    , _rlahtAutoScalingGroupName = p2
    , _rlahtLifecycleActionToken = p3
    }

-- | The name of the Auto Scaling group to which the hook belongs.
rlahtAutoScalingGroupName :: Lens' RecordLifecycleActionHeartbeatType Text
rlahtAutoScalingGroupName =
    lens _rlahtAutoScalingGroupName
        (\s a -> s { _rlahtAutoScalingGroupName = a })

-- | A token that uniquely identifies a specific lifecycle action associated
-- with an instance. Auto Scaling sends this token to the notification
-- target you specified when you created the lifecycle hook.
rlahtLifecycleActionToken :: Lens' RecordLifecycleActionHeartbeatType Text
rlahtLifecycleActionToken =
    lens _rlahtLifecycleActionToken
        (\s a -> s { _rlahtLifecycleActionToken = a })

-- | The name of the lifecycle hook.
rlahtLifecycleHookName :: Lens' RecordLifecycleActionHeartbeatType Text
rlahtLifecycleHookName =
    lens _rlahtLifecycleHookName (\s a -> s { _rlahtLifecycleHookName = a })
instance ToQuery RecordLifecycleActionHeartbeatType

instance ToPath RecordLifecycleActionHeartbeatType where
    toPath = const "/"

data RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RecordLifecycleActionHeartbeatResponse' constructor.
recordLifecycleActionHeartbeatResponse :: RecordLifecycleActionHeartbeatResponse
recordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse
instance FromXML RecordLifecycleActionHeartbeatResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecordLifecycleActionHeartbeatResponse"

instance AWSRequest RecordLifecycleActionHeartbeatType where
    type Sv RecordLifecycleActionHeartbeatType = AutoScaling
    type Rs RecordLifecycleActionHeartbeatType = RecordLifecycleActionHeartbeatResponse

    request  = post "RecordLifecycleActionHeartbeat"
    response = nullaryResponse RecordLifecycleActionHeartbeatResponse
