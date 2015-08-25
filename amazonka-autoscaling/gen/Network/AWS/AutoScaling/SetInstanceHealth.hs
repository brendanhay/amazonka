{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the health status of the specified instance.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/healthcheck.html Health Checks>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetInstanceHealth.html AWS API Reference> for SetInstanceHealth.
module Network.AWS.AutoScaling.SetInstanceHealth
    (
    -- * Creating a Request
      setInstanceHealth
    , SetInstanceHealth
    -- * Request Lenses
    , sihShouldRespectGracePeriod
    , sihInstanceId
    , sihHealthStatus

    -- * Destructuring the Response
    , setInstanceHealthResponse
    , SetInstanceHealthResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setInstanceHealth' smart constructor.
data SetInstanceHealth = SetInstanceHealth'
    { _sihShouldRespectGracePeriod :: !(Maybe Bool)
    , _sihInstanceId               :: !Text
    , _sihHealthStatus             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetInstanceHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sihShouldRespectGracePeriod'
--
-- * 'sihInstanceId'
--
-- * 'sihHealthStatus'
setInstanceHealth
    :: Text -- ^ 'sihInstanceId'
    -> Text -- ^ 'sihHealthStatus'
    -> SetInstanceHealth
setInstanceHealth pInstanceId_ pHealthStatus_ =
    SetInstanceHealth'
    { _sihShouldRespectGracePeriod = Nothing
    , _sihInstanceId = pInstanceId_
    , _sihHealthStatus = pHealthStatus_
    }

-- | If the Auto Scaling group of the specified instance has a
-- 'HealthCheckGracePeriod' specified for the group, by default, this call
-- will respect the grace period. Set this to 'False', if you do not want
-- the call to respect the grace period associated with the group.
--
-- For more information, see the 'HealthCheckGracePeriod' parameter
-- description for CreateAutoScalingGroup.
sihShouldRespectGracePeriod :: Lens' SetInstanceHealth (Maybe Bool)
sihShouldRespectGracePeriod = lens _sihShouldRespectGracePeriod (\ s a -> s{_sihShouldRespectGracePeriod = a});

-- | The ID of the EC2 instance.
sihInstanceId :: Lens' SetInstanceHealth Text
sihInstanceId = lens _sihInstanceId (\ s a -> s{_sihInstanceId = a});

-- | The health status of the instance. Set to 'Healthy' if you want the
-- instance to remain in service. Set to 'Unhealthy' if you want the
-- instance to be out of service. Auto Scaling will terminate and replace
-- the unhealthy instance.
sihHealthStatus :: Lens' SetInstanceHealth Text
sihHealthStatus = lens _sihHealthStatus (\ s a -> s{_sihHealthStatus = a});

instance AWSRequest SetInstanceHealth where
        type Rs SetInstanceHealth = SetInstanceHealthResponse
        request = postQuery autoScaling
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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetInstanceHealthResponse' with the minimum fields required to make a request.
--
setInstanceHealthResponse
    :: SetInstanceHealthResponse
setInstanceHealthResponse = SetInstanceHealthResponse'
