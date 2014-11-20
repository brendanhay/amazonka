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

-- | Suspends Auto Scaling processes for an Auto Scaling group. To suspend
-- specific process types, specify them by name with the
-- ScalingProcesses.member.N parameter. To suspend all process types, omit the
-- ScalingProcesses.member.N parameter. Suspending either of the two primary
-- process types, Launch or Terminate, can prevent other process types from
-- functioning properly. To resume processes that have been suspended, use
-- ResumeProcesses For more information on suspending and resuming Auto
-- Scaling process, see Suspend and Resume Auto Scaling Process.
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
    , _spScalingProcesses     :: List "ScalingProcesses" Text
    } deriving (Eq, Ord, Show)

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

-- | The processes that you want to suspend or resume, which can include one
-- or more of the following: Launch Terminate HealthCheck ReplaceUnhealthy
-- AZRebalance AlarmNotification ScheduledActions AddToLoadBalancer To
-- suspend all process types, omit this parameter.
spScalingProcesses :: Lens' SuspendProcesses [Text]
spScalingProcesses =
    lens _spScalingProcesses (\s a -> s { _spScalingProcesses = a })
        . _List

data SuspendProcessesResponse = SuspendProcessesResponse
    deriving (Eq, Ord, Show, Generic)

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


Some kind of operator / class to check the types whether to continue?
