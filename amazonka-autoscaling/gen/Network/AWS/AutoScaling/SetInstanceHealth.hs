{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
      SetInstanceHealth
    -- ** Request constructor
    , mkSetInstanceHealth
    -- ** Request lenses
    , sihInstanceId
    , sihHealthStatus
    , sihShouldRespectGracePeriod

    -- * Response
    , SetInstanceHealthResponse
    -- ** Response constructor
    , mkSetInstanceHealthResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data SetInstanceHealth = SetInstanceHealth
    { _sihInstanceId :: Text
    , _sihHealthStatus :: Text
    , _sihShouldRespectGracePeriod :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetInstanceHealth' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @HealthStatus ::@ @Text@
--
-- * @ShouldRespectGracePeriod ::@ @Maybe Bool@
--
mkSetInstanceHealth :: Text -- ^ 'sihInstanceId'
                    -> Text -- ^ 'sihHealthStatus'
                    -> SetInstanceHealth
mkSetInstanceHealth p1 p2 = SetInstanceHealth
    { _sihInstanceId = p1
    , _sihHealthStatus = p2
    , _sihShouldRespectGracePeriod = Nothing
    }

-- | The identifier of the Amazon EC2 instance.
sihInstanceId :: Lens' SetInstanceHealth Text
sihInstanceId = lens _sihInstanceId (\s a -> s { _sihInstanceId = a })

-- | The health status of the instance. Set to Healthy if you want the instance
-- to remain in service. Set to Unhealthy if you want the instance to be out
-- of service. Auto Scaling will terminate and replace the unhealthy instance.
sihHealthStatus :: Lens' SetInstanceHealth Text
sihHealthStatus = lens _sihHealthStatus (\s a -> s { _sihHealthStatus = a })

-- | If the Auto Scaling group of the specified instance has a
-- HealthCheckGracePeriod specified for the group, by default, this call will
-- respect the grace period. Set this to False, if you do not want the call to
-- respect the grace period associated with the group. For more information,
-- see the HealthCheckGracePeriod parameter description in the
-- CreateAutoScalingGroup action.
sihShouldRespectGracePeriod :: Lens' SetInstanceHealth (Maybe Bool)
sihShouldRespectGracePeriod =
    lens _sihShouldRespectGracePeriod
         (\s a -> s { _sihShouldRespectGracePeriod = a })

instance ToQuery SetInstanceHealth where
    toQuery = genericQuery def

data SetInstanceHealthResponse = SetInstanceHealthResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetInstanceHealthResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetInstanceHealthResponse :: SetInstanceHealthResponse
mkSetInstanceHealthResponse = SetInstanceHealthResponse

instance AWSRequest SetInstanceHealth where
    type Sv SetInstanceHealth = AutoScaling
    type Rs SetInstanceHealth = SetInstanceHealthResponse

    request = post "SetInstanceHealth"
    response _ = nullaryResponse SetInstanceHealthResponse
