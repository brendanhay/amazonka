{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.ResumeProcesses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resumes all suspended Auto Scaling processes for an Auto Scaling group. For
-- information on suspending and resuming Auto Scaling process, see Suspend
-- and Resume Auto Scaling Process.
module Network.AWS.AutoScaling.V2011_01_01.ResumeProcesses where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ResumeProcesses' request.
resumeProcesses :: Text -- ^ '_sprAutoScalingGroupName'
                -> ResumeProcesses
resumeProcesses p1 = ResumeProcesses
    { _sprAutoScalingGroupName = p1
    , _sprScalingProcesses = mempty
    }

data ResumeProcesses = ResumeProcesses
    { _sprAutoScalingGroupName :: Text
      -- ^ The name or Amazon Resource Name (ARN) of the Auto Scaling group.
    , _sprScalingProcesses :: [Text]
      -- ^ The processes that you want to suspend or resume, which can
      -- include one or more of the following: Launch Terminate
      -- HealthCheck ReplaceUnhealthy AZRebalance AlarmNotification
      -- ScheduledActions AddToLoadBalancer To suspend all process types,
      -- omit this parameter.
    } deriving (Show, Generic)

makeLenses ''ResumeProcesses

instance ToQuery ResumeProcesses where
    toQuery = genericQuery def

data ResumeProcessesResponse = ResumeProcessesResponse
    deriving (Eq, Show, Generic)

makeLenses ''ResumeProcessesResponse

instance AWSRequest ResumeProcesses where
    type Sv ResumeProcesses = AutoScaling
    type Rs ResumeProcesses = ResumeProcessesResponse

    request = post "ResumeProcesses"
    response _ = nullaryResponse ResumeProcessesResponse
