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

-- | Suspends Auto Scaling processes for an Auto Scaling group. To suspend
-- specific process types, specify them by name with the
-- ScalingProcesses.member.N parameter. To suspend all process types, omit the
-- ScalingProcesses.member.N parameter. Suspending either of the two primary
-- process types, Launch or Terminate, can prevent other process types from
-- functioning properly. To resume processes that have been suspended, use
-- ResumeProcesses For more information on suspending and resuming Auto
-- Scaling process, see Suspend and Resume Auto Scaling Process.
module Network.AWS.AutoScaling
    (
    -- * Request
      SuspendProcesses
    -- ** Request constructor
    , mkSuspendProcesses
    -- ** Request lenses
    , sp1AutoScalingGroupName
    , sp1ScalingProcesses

    -- * Response
    , SuspendProcessesResponse
    -- ** Response constructor
    , mkSuspendProcessesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data SuspendProcesses = SuspendProcesses
    { _sp1AutoScalingGroupName :: !Text
    , _sp1ScalingProcesses :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SuspendProcesses' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @ScalingProcesses ::@ @[Text]@
--
mkSuspendProcesses :: Text -- ^ 'sp1AutoScalingGroupName'
                   -> SuspendProcesses
mkSuspendProcesses p1 = SuspendProcesses
    { _sp1AutoScalingGroupName = p1
    , _sp1ScalingProcesses = mempty
    }

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
sp1AutoScalingGroupName :: Lens' SuspendProcesses Text
sp1AutoScalingGroupName =
    lens _sp1AutoScalingGroupName
         (\s a -> s { _sp1AutoScalingGroupName = a })

-- | The processes that you want to suspend or resume, which can include one or
-- more of the following: Launch Terminate HealthCheck ReplaceUnhealthy
-- AZRebalance AlarmNotification ScheduledActions AddToLoadBalancer To suspend
-- all process types, omit this parameter.
sp1ScalingProcesses :: Lens' SuspendProcesses [Text]
sp1ScalingProcesses =
    lens _sp1ScalingProcesses (\s a -> s { _sp1ScalingProcesses = a })

instance ToQuery SuspendProcesses where
    toQuery = genericQuery def

data SuspendProcessesResponse = SuspendProcessesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SuspendProcessesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSuspendProcessesResponse :: SuspendProcessesResponse
mkSuspendProcessesResponse = SuspendProcessesResponse

instance AWSRequest SuspendProcesses where
    type Sv SuspendProcesses = AutoScaling
    type Rs SuspendProcesses = SuspendProcessesResponse

    request = post "SuspendProcesses"
    response _ = nullaryResponse SuspendProcessesResponse
