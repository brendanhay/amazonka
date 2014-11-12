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

-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the health status of a specified instance that belongs to any of your
-- Auto Scaling groups. For more information, see Configure Health Checks for
-- Your Auto Scaling group.
module Network.AWS.AutoScaling.SetInstanceHealth
    (
    -- * Request
      SetInstanceHealthQuery
    -- ** Request constructor
    , setInstanceHealth
    -- ** Request lenses
    , sihqHealthStatus
    , sihqInstanceId
    , sihqShouldRespectGracePeriod

    -- * Response
    , SetInstanceHealthResponse
    -- ** Response constructor
    , setInstanceHealthResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data SetInstanceHealthQuery = SetInstanceHealthQuery
    { _sihqHealthStatus             :: Text
    , _sihqInstanceId               :: Text
    , _sihqShouldRespectGracePeriod :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetInstanceHealthQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sihqHealthStatus' @::@ 'Text'
--
-- * 'sihqInstanceId' @::@ 'Text'
--
-- * 'sihqShouldRespectGracePeriod' @::@ 'Maybe' 'Bool'
--
setInstanceHealth :: Text -- ^ 'sihqInstanceId'
                  -> Text -- ^ 'sihqHealthStatus'
                  -> SetInstanceHealthQuery
setInstanceHealth p1 p2 = SetInstanceHealthQuery
    { _sihqInstanceId               = p1
    , _sihqHealthStatus             = p2
    , _sihqShouldRespectGracePeriod = Nothing
    }

-- | The health status of the instance. Set to Healthy if you want the
-- instance to remain in service. Set to Unhealthy if you want the instance
-- to be out of service. Auto Scaling will terminate and replace the
-- unhealthy instance.
sihqHealthStatus :: Lens' SetInstanceHealthQuery Text
sihqHealthStatus = lens _sihqHealthStatus (\s a -> s { _sihqHealthStatus = a })

-- | The identifier of the Amazon EC2 instance.
sihqInstanceId :: Lens' SetInstanceHealthQuery Text
sihqInstanceId = lens _sihqInstanceId (\s a -> s { _sihqInstanceId = a })

-- | If the Auto Scaling group of the specified instance has a
-- HealthCheckGracePeriod specified for the group, by default, this call
-- will respect the grace period. Set this to False, if you do not want the
-- call to respect the grace period associated with the group. For more
-- information, see the HealthCheckGracePeriod parameter description in the
-- CreateAutoScalingGroup action.
sihqShouldRespectGracePeriod :: Lens' SetInstanceHealthQuery (Maybe Bool)
sihqShouldRespectGracePeriod =
    lens _sihqShouldRespectGracePeriod
        (\s a -> s { _sihqShouldRespectGracePeriod = a })

instance ToQuery SetInstanceHealthQuery

instance ToPath SetInstanceHealthQuery where
    toPath = const "/"

data SetInstanceHealthResponse = SetInstanceHealthResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetInstanceHealthResponse' constructor.
setInstanceHealthResponse :: SetInstanceHealthResponse
setInstanceHealthResponse = SetInstanceHealthResponse

instance FromXML SetInstanceHealthResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetInstanceHealthResponse"

instance AWSRequest SetInstanceHealthQuery where
    type Sv SetInstanceHealthQuery = AutoScaling
    type Rs SetInstanceHealthQuery = SetInstanceHealthResponse

    request  = post "SetInstanceHealth"
    response = nullaryResponse SetInstanceHealthResponse
