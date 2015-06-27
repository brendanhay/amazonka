{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/healthcheck.html Health Checks>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetInstanceHealth.html>
module Network.AWS.AutoScaling.SetInstanceHealth
    (
    -- * Request
      SetInstanceHealth
    -- ** Request constructor
    , setInstanceHealth
    -- ** Request lenses
    , sihShouldRespectGracePeriod
    , sihInstanceId
    , sihHealthStatus

    -- * Response
    , SetInstanceHealthResponse
    -- ** Response constructor
    , setInstanceHealthResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setInstanceHealth' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sihShouldRespectGracePeriod'
--
-- * 'sihInstanceId'
--
-- * 'sihHealthStatus'
data SetInstanceHealth = SetInstanceHealth'
    { _sihShouldRespectGracePeriod :: !(Maybe Bool)
    , _sihInstanceId               :: !Text
    , _sihHealthStatus             :: !Text
    } deriving (Eq,Read,Show)

-- | 'SetInstanceHealth' smart constructor.
setInstanceHealth :: Text -> Text -> SetInstanceHealth
setInstanceHealth pInstanceId pHealthStatus =
    SetInstanceHealth'
    { _sihShouldRespectGracePeriod = Nothing
    , _sihInstanceId = pInstanceId
    , _sihHealthStatus = pHealthStatus
    }

-- | If the Auto Scaling group of the specified instance has a
-- @HealthCheckGracePeriod@ specified for the group, by default, this call
-- will respect the grace period. Set this to @False@, if you do not want
-- the call to respect the grace period associated with the group.
--
-- For more information, see the @HealthCheckGracePeriod@ parameter
-- description for CreateAutoScalingGroup.
sihShouldRespectGracePeriod :: Lens' SetInstanceHealth (Maybe Bool)
sihShouldRespectGracePeriod = lens _sihShouldRespectGracePeriod (\ s a -> s{_sihShouldRespectGracePeriod = a});

-- | The ID of the EC2 instance.
sihInstanceId :: Lens' SetInstanceHealth Text
sihInstanceId = lens _sihInstanceId (\ s a -> s{_sihInstanceId = a});

-- | The health status of the instance. Set to @Healthy@ if you want the
-- instance to remain in service. Set to @Unhealthy@ if you want the
-- instance to be out of service. Auto Scaling will terminate and replace
-- the unhealthy instance.
sihHealthStatus :: Lens' SetInstanceHealth Text
sihHealthStatus = lens _sihHealthStatus (\ s a -> s{_sihHealthStatus = a});

instance AWSRequest SetInstanceHealth where
        type Sv SetInstanceHealth = AutoScaling
        type Rs SetInstanceHealth = SetInstanceHealthResponse
        request = post
        response = receiveNull SetInstanceHealthResponse'

instance ToHeaders SetInstanceHealth where
        toHeaders = const mempty

instance ToPath SetInstanceHealth where
        toPath = const "/"

instance ToQuery SetInstanceHealth where
        toQuery SetInstanceHealth'{..}
          = mconcat
              ["Action" =: ("SetInstanceHealth" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "ShouldRespectGracePeriod" =:
                 _sihShouldRespectGracePeriod,
               "InstanceId" =: _sihInstanceId,
               "HealthStatus" =: _sihHealthStatus]

-- | /See:/ 'setInstanceHealthResponse' smart constructor.
data SetInstanceHealthResponse =
    SetInstanceHealthResponse'
    deriving (Eq,Read,Show)

-- | 'SetInstanceHealthResponse' smart constructor.
setInstanceHealthResponse :: SetInstanceHealthResponse
setInstanceHealthResponse = SetInstanceHealthResponse'
