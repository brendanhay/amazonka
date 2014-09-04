{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.AutoScaling.V2011_01_01.SuspendProcesses
    (
    -- * Request
      SuspendProcesses
    -- ** Request constructor
    , mkScalingProcessQuery
    -- ** Request lenses
    , sprAutoScalingGroupName
    , sprScalingProcesses

    -- * Response
    , SuspendProcessesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SuspendProcesses' request.
mkScalingProcessQuery :: Text -- ^ 'sprAutoScalingGroupName'
                      -> SuspendProcesses
mkScalingProcessQuery p1 = SuspendProcesses
    { _sprAutoScalingGroupName = p1
    , _sprScalingProcesses = mempty
    }
{-# INLINE mkScalingProcessQuery #-}

data SuspendProcesses = SuspendProcesses
    { _sprAutoScalingGroupName :: Text
      -- ^ The name or Amazon Resource Name (ARN) of the Auto Scaling group.
    , _sprScalingProcesses :: [Text]
      -- ^ The processes that you want to suspend or resume, which can
      -- include one or more of the following: Launch Terminate
      -- HealthCheck ReplaceUnhealthy AZRebalance AlarmNotification
      -- ScheduledActions AddToLoadBalancer To suspend all process types,
      -- omit this parameter.
    } deriving (Show, Generic)

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
sprAutoScalingGroupName :: Lens' SuspendProcesses (Text)
sprAutoScalingGroupName = lens _sprAutoScalingGroupName (\s a -> s { _sprAutoScalingGroupName = a })
{-# INLINE sprAutoScalingGroupName #-}

-- | The processes that you want to suspend or resume, which can include one or
-- more of the following: Launch Terminate HealthCheck ReplaceUnhealthy
-- AZRebalance AlarmNotification ScheduledActions AddToLoadBalancer To suspend
-- all process types, omit this parameter.
sprScalingProcesses :: Lens' SuspendProcesses ([Text])
sprScalingProcesses = lens _sprScalingProcesses (\s a -> s { _sprScalingProcesses = a })
{-# INLINE sprScalingProcesses #-}

instance ToQuery SuspendProcesses where
    toQuery = genericQuery def

data SuspendProcessesResponse = SuspendProcessesResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SuspendProcesses where
    type Sv SuspendProcesses = AutoScaling
    type Rs SuspendProcesses = SuspendProcessesResponse

    request = post "SuspendProcesses"
    response _ = nullaryResponse SuspendProcessesResponse
