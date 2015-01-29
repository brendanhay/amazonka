{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.SuspendProcesses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Suspends the specified Auto Scaling processes for the specified Auto Scaling
-- group. To suspend specific processes, use the 'ScalingProcesses' parameter. To
-- suspend all processes, omit the 'ScalingProcesses' parameter.
--
-- Note that if you suspend either the 'Launch' or 'Terminate' process types, it
-- can prevent other process types from functioning properly.
--
-- To resume processes that have been suspended, use 'ResumeProcesses'.
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US_SuspendResume.html Suspend and Resume Auto Scaling Processes> in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SuspendProcesses.html>
module Network.AWS.AutoScaling.SuspendProcesses
    (
    -- * Request
      SuspendProcesses
    -- ** Request constructor
    , suspendProcesses
    -- ** Request lenses
    , spAutoScalingGroupName
    , spScalingProcesses

    -- * Response
    , SuspendProcessesResponse
    -- ** Response constructor
    , suspendProcessesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data SuspendProcesses = SuspendProcesses
    { _spAutoScalingGroupName :: Text
    , _spScalingProcesses     :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SuspendProcesses' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spAutoScalingGroupName' @::@ 'Text'
--
-- * 'spScalingProcesses' @::@ ['Text']
--
suspendProcesses :: Text -- ^ 'spAutoScalingGroupName'
                 -> SuspendProcesses
suspendProcesses p1 = SuspendProcesses
    { _spAutoScalingGroupName = p1
    , _spScalingProcesses     = mempty
    }

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
spAutoScalingGroupName :: Lens' SuspendProcesses Text
spAutoScalingGroupName =
    lens _spAutoScalingGroupName (\s a -> s { _spAutoScalingGroupName = a })

-- | One or more of the following processes:
--
-- Launch Terminate HealthCheck ReplaceUnhealthy AZRebalance AlarmNotification
-- ScheduledActions AddToLoadBalancer
spScalingProcesses :: Lens' SuspendProcesses [Text]
spScalingProcesses =
    lens _spScalingProcesses (\s a -> s { _spScalingProcesses = a })
        . _List

data SuspendProcessesResponse = SuspendProcessesResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SuspendProcessesResponse' constructor.
suspendProcessesResponse :: SuspendProcessesResponse
suspendProcessesResponse = SuspendProcessesResponse

instance ToPath SuspendProcesses where
    toPath = const "/"

instance ToQuery SuspendProcesses where
    toQuery SuspendProcesses{..} = mconcat
        [ "AutoScalingGroupName" =? _spAutoScalingGroupName
        , "ScalingProcesses"     =? _spScalingProcesses
        ]

instance ToHeaders SuspendProcesses

instance AWSRequest SuspendProcesses where
    type Sv SuspendProcesses = AutoScaling
    type Rs SuspendProcesses = SuspendProcessesResponse

    request  = post "SuspendProcesses"
    response = nullResponse SuspendProcessesResponse
