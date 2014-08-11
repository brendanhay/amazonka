{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.SuspendProcesses
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
module Network.AWS.AutoScaling.V2011_01_01.SuspendProcesses where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SuspendProcesses' request.
suspendProcesses :: Text -- ^ '_spqAutoScalingGroupName'
                 -> SuspendProcesses
suspendProcesses p1 = SuspendProcesses
    { _spqAutoScalingGroupName = p1
    , _spqScalingProcesses = mempty
    }

data SuspendProcesses = SuspendProcesses
    { _spqAutoScalingGroupName :: Text
      -- ^ The name or Amazon Resource Name (ARN) of the Auto Scaling group.
    , _spqScalingProcesses :: [Text]
      -- ^ The processes that you want to suspend or resume, which can
      -- include one or more of the following: Launch Terminate
      -- HealthCheck ReplaceUnhealthy AZRebalance AlarmNotification
      -- ScheduledActions AddToLoadBalancer To suspend all process types,
      -- omit this parameter.
    } deriving (Show, Generic)

makeLenses ''SuspendProcesses

instance ToQuery SuspendProcesses where
    toQuery = genericToQuery def

data SuspendProcessesResponse = SuspendProcessesResponse
    deriving (Eq, Show, Generic)

makeLenses ''SuspendProcessesResponse

instance AWSRequest SuspendProcesses where
    type Sv SuspendProcesses = AutoScaling
    type Rs SuspendProcesses = SuspendProcessesResponse

    request = post "SuspendProcesses"
    response _ = nullaryResponse SuspendProcessesResponse
