{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.SetInstanceHealth
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
module Network.AWS.AutoScaling.V2011_01_01.SetInstanceHealth
    (
    -- * Request
      SetInstanceHealth
    -- ** Request constructor
    , mkSetInstanceHealthQuery
    -- ** Request lenses
    , sihqInstanceId
    , sihqHealthStatus
    , sihqShouldRespectGracePeriod

    -- * Response
    , SetInstanceHealthResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetInstanceHealth' request.
mkSetInstanceHealthQuery :: Text -- ^ 'sihqInstanceId'
                         -> Text -- ^ 'sihqHealthStatus'
                         -> SetInstanceHealth
mkSetInstanceHealthQuery p1 p2 = SetInstanceHealth
    { _sihqInstanceId = p1
    , _sihqHealthStatus = p2
    , _sihqShouldRespectGracePeriod = Nothing
    }
{-# INLINE mkSetInstanceHealthQuery #-}

data SetInstanceHealth = SetInstanceHealth
    { _sihqInstanceId :: Text
      -- ^ The identifier of the Amazon EC2 instance.
    , _sihqHealthStatus :: Text
      -- ^ The health status of the instance. Set to Healthy if you want the
      -- instance to remain in service. Set to Unhealthy if you want the
      -- instance to be out of service. Auto Scaling will terminate and
      -- replace the unhealthy instance.
    , _sihqShouldRespectGracePeriod :: Maybe Bool
      -- ^ If the Auto Scaling group of the specified instance has a
      -- HealthCheckGracePeriod specified for the group, by default, this
      -- call will respect the grace period. Set this to False, if you do
      -- not want the call to respect the grace period associated with the
      -- group. For more information, see the HealthCheckGracePeriod
      -- parameter description in the CreateAutoScalingGroup action.
    } deriving (Show, Generic)

-- | The identifier of the Amazon EC2 instance.
sihqInstanceId :: Lens' SetInstanceHealth (Text)
sihqInstanceId = lens _sihqInstanceId (\s a -> s { _sihqInstanceId = a })
{-# INLINE sihqInstanceId #-}

-- | The health status of the instance. Set to Healthy if you want the instance
-- to remain in service. Set to Unhealthy if you want the instance to be out
-- of service. Auto Scaling will terminate and replace the unhealthy instance.
sihqHealthStatus :: Lens' SetInstanceHealth (Text)
sihqHealthStatus = lens _sihqHealthStatus (\s a -> s { _sihqHealthStatus = a })
{-# INLINE sihqHealthStatus #-}

-- | If the Auto Scaling group of the specified instance has a
-- HealthCheckGracePeriod specified for the group, by default, this call will
-- respect the grace period. Set this to False, if you do not want the call to
-- respect the grace period associated with the group. For more information,
-- see the HealthCheckGracePeriod parameter description in the
-- CreateAutoScalingGroup action.
sihqShouldRespectGracePeriod :: Lens' SetInstanceHealth (Maybe Bool)
sihqShouldRespectGracePeriod = lens _sihqShouldRespectGracePeriod (\s a -> s { _sihqShouldRespectGracePeriod = a })
{-# INLINE sihqShouldRespectGracePeriod #-}

instance ToQuery SetInstanceHealth where
    toQuery = genericQuery def

data SetInstanceHealthResponse = SetInstanceHealthResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetInstanceHealth where
    type Sv SetInstanceHealth = AutoScaling
    type Rs SetInstanceHealth = SetInstanceHealthResponse

    request = post "SetInstanceHealth"
    response _ = nullaryResponse SetInstanceHealthResponse
