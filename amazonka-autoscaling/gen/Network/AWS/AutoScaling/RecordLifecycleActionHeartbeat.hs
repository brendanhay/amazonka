{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
      RecordLifecycleActionHeartbeat
    -- ** Request constructor
    , mkRecordLifecycleActionHeartbeat
    -- ** Request lenses
    , rlahLifecycleHookName
    , rlahAutoScalingGroupName
    , rlahLifecycleActionToken

    -- * Response
    , RecordLifecycleActionHeartbeatResponse
    -- ** Response constructor
    , mkRecordLifecycleActionHeartbeatResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeat
    { _rlahLifecycleHookName :: !Text
    , _rlahAutoScalingGroupName :: !Text
    , _rlahLifecycleActionToken :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RecordLifecycleActionHeartbeat' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LifecycleHookName ::@ @Text@
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @LifecycleActionToken ::@ @Text@
--
mkRecordLifecycleActionHeartbeat :: Text -- ^ 'rlahLifecycleHookName'
                                 -> Text -- ^ 'rlahAutoScalingGroupName'
                                 -> Text -- ^ 'rlahLifecycleActionToken'
                                 -> RecordLifecycleActionHeartbeat
mkRecordLifecycleActionHeartbeat p1 p2 p3 = RecordLifecycleActionHeartbeat
    { _rlahLifecycleHookName = p1
    , _rlahAutoScalingGroupName = p2
    , _rlahLifecycleActionToken = p3
    }

-- | The name of the lifecycle hook.
rlahLifecycleHookName :: Lens' RecordLifecycleActionHeartbeat Text
rlahLifecycleHookName =
    lens _rlahLifecycleHookName (\s a -> s { _rlahLifecycleHookName = a })

-- | The name of the Auto Scaling group to which the hook belongs.
rlahAutoScalingGroupName :: Lens' RecordLifecycleActionHeartbeat Text
rlahAutoScalingGroupName =
    lens _rlahAutoScalingGroupName
         (\s a -> s { _rlahAutoScalingGroupName = a })

-- | A token that uniquely identifies a specific lifecycle action associated
-- with an instance. Auto Scaling sends this token to the notification target
-- you specified when you created the lifecycle hook.
rlahLifecycleActionToken :: Lens' RecordLifecycleActionHeartbeat Text
rlahLifecycleActionToken =
    lens _rlahLifecycleActionToken
         (\s a -> s { _rlahLifecycleActionToken = a })

instance ToQuery RecordLifecycleActionHeartbeat where
    toQuery = genericQuery def

-- | The output of the RecordLifecycleActionHeartbeat action.
data RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RecordLifecycleActionHeartbeatResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRecordLifecycleActionHeartbeatResponse :: RecordLifecycleActionHeartbeatResponse
mkRecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse

instance AWSRequest RecordLifecycleActionHeartbeat where
    type Sv RecordLifecycleActionHeartbeat = AutoScaling
    type Rs RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeatResponse

    request = post "RecordLifecycleActionHeartbeat"
    response _ = nullaryResponse RecordLifecycleActionHeartbeatResponse
