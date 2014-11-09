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

-- Module      : Network.AWS.AutoScaling.ResumeProcesses
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
module Network.AWS.AutoScaling.ResumeProcesses
    (
    -- * Request
      ScalingProcessQuery
    -- ** Request constructor
    , scalingProcessQuery
    -- ** Request lenses
    , spqAutoScalingGroupName
    , spqScalingProcesses

    -- * Response
    , ResumeProcessesResponse
    -- ** Response constructor
    , resumeProcessesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data ScalingProcessQuery = ScalingProcessQuery
    { _spqAutoScalingGroupName :: Text
    , _spqScalingProcesses     :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ScalingProcessQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spqAutoScalingGroupName' @::@ 'Text'
--
-- * 'spqScalingProcesses' @::@ ['Text']
--
scalingProcessQuery :: Text -- ^ 'spqAutoScalingGroupName'
                    -> ScalingProcessQuery
scalingProcessQuery p1 = ScalingProcessQuery
    { _spqAutoScalingGroupName = p1
    , _spqScalingProcesses     = mempty
    }

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
spqAutoScalingGroupName :: Lens' ScalingProcessQuery Text
spqAutoScalingGroupName =
    lens _spqAutoScalingGroupName (\s a -> s { _spqAutoScalingGroupName = a })

-- | The processes that you want to suspend or resume, which can include one
-- or more of the following: Launch Terminate HealthCheck ReplaceUnhealthy
-- AZRebalance AlarmNotification ScheduledActions AddToLoadBalancer To
-- suspend all process types, omit this parameter.
spqScalingProcesses :: Lens' ScalingProcessQuery [Text]
spqScalingProcesses =
    lens _spqScalingProcesses (\s a -> s { _spqScalingProcesses = a })

instance ToPath ScalingProcessQuery where
    toPath = const "/"

instance ToQuery ScalingProcessQuery

data ResumeProcessesResponse = ResumeProcessesResponse

-- | 'ResumeProcessesResponse' constructor.
resumeProcessesResponse :: ResumeProcessesResponse
resumeProcessesResponse = ResumeProcessesResponse

instance AWSRequest ScalingProcessQuery where
    type Sv ScalingProcessQuery = AutoScaling
    type Rs ScalingProcessQuery = ResumeProcessesResponse

    request  = post "ResumeProcesses"
    response = const (nullaryResponse ResumeProcessesResponse)
