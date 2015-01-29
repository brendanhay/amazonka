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

-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Sets the health status of the specified instance.
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/healthcheck.html Health Checks> in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetInstanceHealth.html>
module Network.AWS.AutoScaling.SetInstanceHealth
    (
    -- * Request
      SetInstanceHealth
    -- ** Request constructor
    , setInstanceHealth
    -- ** Request lenses
    , sihHealthStatus
    , sihInstanceId
    , sihShouldRespectGracePeriod

    -- * Response
    , SetInstanceHealthResponse
    -- ** Response constructor
    , setInstanceHealthResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data SetInstanceHealth = SetInstanceHealth
    { _sihHealthStatus             :: Text
    , _sihInstanceId               :: Text
    , _sihShouldRespectGracePeriod :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'SetInstanceHealth' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sihHealthStatus' @::@ 'Text'
--
-- * 'sihInstanceId' @::@ 'Text'
--
-- * 'sihShouldRespectGracePeriod' @::@ 'Maybe' 'Bool'
--
setInstanceHealth :: Text -- ^ 'sihInstanceId'
                  -> Text -- ^ 'sihHealthStatus'
                  -> SetInstanceHealth
setInstanceHealth p1 p2 = SetInstanceHealth
    { _sihInstanceId               = p1
    , _sihHealthStatus             = p2
    , _sihShouldRespectGracePeriod = Nothing
    }

-- | The health status of the instance. Set to 'Healthy' if you want the instance
-- to remain in service. Set to 'Unhealthy' if you want the instance to be out of
-- service. Auto Scaling will terminate and replace the unhealthy instance.
sihHealthStatus :: Lens' SetInstanceHealth Text
sihHealthStatus = lens _sihHealthStatus (\s a -> s { _sihHealthStatus = a })

-- | The ID of the EC2 instance.
sihInstanceId :: Lens' SetInstanceHealth Text
sihInstanceId = lens _sihInstanceId (\s a -> s { _sihInstanceId = a })

-- | If the Auto Scaling group of the specified instance has a 'HealthCheckGracePeriod' specified for the group, by default, this call will respect the grace
-- period. Set this to 'False', if you do not want the call to respect the grace
-- period associated with the group.
--
-- For more information, see the 'HealthCheckGracePeriod' parameter description
-- for 'CreateAutoScalingGroup'.
sihShouldRespectGracePeriod :: Lens' SetInstanceHealth (Maybe Bool)
sihShouldRespectGracePeriod =
    lens _sihShouldRespectGracePeriod
        (\s a -> s { _sihShouldRespectGracePeriod = a })

data SetInstanceHealthResponse = SetInstanceHealthResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetInstanceHealthResponse' constructor.
setInstanceHealthResponse :: SetInstanceHealthResponse
setInstanceHealthResponse = SetInstanceHealthResponse

instance ToPath SetInstanceHealth where
    toPath = const "/"

instance ToQuery SetInstanceHealth where
    toQuery SetInstanceHealth{..} = mconcat
        [ "HealthStatus"             =? _sihHealthStatus
        , "InstanceId"               =? _sihInstanceId
        , "ShouldRespectGracePeriod" =? _sihShouldRespectGracePeriod
        ]

instance ToHeaders SetInstanceHealth

instance AWSRequest SetInstanceHealth where
    type Sv SetInstanceHealth = AutoScaling
    type Rs SetInstanceHealth = SetInstanceHealthResponse

    request  = post "SetInstanceHealth"
    response = nullResponse SetInstanceHealthResponse
