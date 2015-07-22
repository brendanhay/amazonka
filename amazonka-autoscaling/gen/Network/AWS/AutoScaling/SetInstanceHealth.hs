{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the health status of the specified instance.
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
    , sihrqShouldRespectGracePeriod
    , sihrqInstanceId
    , sihrqHealthStatus

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
-- * 'sihrqShouldRespectGracePeriod'
--
-- * 'sihrqInstanceId'
--
-- * 'sihrqHealthStatus'
data SetInstanceHealth = SetInstanceHealth'
    { _sihrqShouldRespectGracePeriod :: !(Maybe Bool)
    , _sihrqInstanceId               :: !Text
    , _sihrqHealthStatus             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetInstanceHealth' smart constructor.
setInstanceHealth :: Text -> Text -> SetInstanceHealth
setInstanceHealth pInstanceId pHealthStatus =
    SetInstanceHealth'
    { _sihrqShouldRespectGracePeriod = Nothing
    , _sihrqInstanceId = pInstanceId
    , _sihrqHealthStatus = pHealthStatus
    }

-- | If the Auto Scaling group of the specified instance has a
-- @HealthCheckGracePeriod@ specified for the group, by default, this call
-- will respect the grace period. Set this to @False@, if you do not want
-- the call to respect the grace period associated with the group.
--
-- For more information, see the @HealthCheckGracePeriod@ parameter
-- description for CreateAutoScalingGroup.
sihrqShouldRespectGracePeriod :: Lens' SetInstanceHealth (Maybe Bool)
sihrqShouldRespectGracePeriod = lens _sihrqShouldRespectGracePeriod (\ s a -> s{_sihrqShouldRespectGracePeriod = a});

-- | The ID of the EC2 instance.
sihrqInstanceId :: Lens' SetInstanceHealth Text
sihrqInstanceId = lens _sihrqInstanceId (\ s a -> s{_sihrqInstanceId = a});

-- | The health status of the instance. Set to @Healthy@ if you want the
-- instance to remain in service. Set to @Unhealthy@ if you want the
-- instance to be out of service. Auto Scaling will terminate and replace
-- the unhealthy instance.
sihrqHealthStatus :: Lens' SetInstanceHealth Text
sihrqHealthStatus = lens _sihrqHealthStatus (\ s a -> s{_sihrqHealthStatus = a});

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
                 _sihrqShouldRespectGracePeriod,
               "InstanceId" =: _sihrqInstanceId,
               "HealthStatus" =: _sihrqHealthStatus]

-- | /See:/ 'setInstanceHealthResponse' smart constructor.
data SetInstanceHealthResponse =
    SetInstanceHealthResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetInstanceHealthResponse' smart constructor.
setInstanceHealthResponse :: SetInstanceHealthResponse
setInstanceHealthResponse = SetInstanceHealthResponse'
