{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.AutoScaling.V2011_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

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
    } deriving (Generic)

instance ToQuery SuspendProcesses where
    toQuery = genericToQuery def

instance AWSRequest SuspendProcesses where
    type Sv SuspendProcesses = AutoScaling
    type Rs SuspendProcesses = SuspendProcessesResponse

    request = post "SuspendProcesses"
    response _ _ = return (Right SuspendProcessesResponse)

data SuspendProcessesResponse = SuspendProcessesResponse
    deriving (Eq, Show, Generic)
