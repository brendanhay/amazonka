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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the health status of the specified instance.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health Checks for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
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

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setInstanceHealth' smart constructor.
data SetInstanceHealth = SetInstanceHealth'
  { _sihShouldRespectGracePeriod :: !(Maybe Bool)
  , _sihInstanceId               :: !Text
  , _sihHealthStatus             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetInstanceHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sihShouldRespectGracePeriod' - If the Auto Scaling group of the specified instance has a @HealthCheckGracePeriod@ specified for the group, by default, this call respects the grace period. Set this to @False@ , to have the call not respect the grace period associated with the group. For more information about the health check grace period, see 'CreateAutoScalingGroup' .
--
-- * 'sihInstanceId' - The ID of the instance.
--
-- * 'sihHealthStatus' - The health status of the instance. Set to @Healthy@ to have the instance remain in service. Set to @Unhealthy@ to have the instance be out of service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy instance.
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


-- | If the Auto Scaling group of the specified instance has a @HealthCheckGracePeriod@ specified for the group, by default, this call respects the grace period. Set this to @False@ , to have the call not respect the grace period associated with the group. For more information about the health check grace period, see 'CreateAutoScalingGroup' .
sihShouldRespectGracePeriod :: Lens' SetInstanceHealth (Maybe Bool)
sihShouldRespectGracePeriod = lens _sihShouldRespectGracePeriod (\ s a -> s{_sihShouldRespectGracePeriod = a})

-- | The ID of the instance.
sihInstanceId :: Lens' SetInstanceHealth Text
sihInstanceId = lens _sihInstanceId (\ s a -> s{_sihInstanceId = a})

-- | The health status of the instance. Set to @Healthy@ to have the instance remain in service. Set to @Unhealthy@ to have the instance be out of service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy instance.
sihHealthStatus :: Lens' SetInstanceHealth Text
sihHealthStatus = lens _sihHealthStatus (\ s a -> s{_sihHealthStatus = a})

instance AWSRequest SetInstanceHealth where
        type Rs SetInstanceHealth = SetInstanceHealthResponse
        request = postQuery autoScaling
        response = receiveNull SetInstanceHealthResponse'

instance Hashable SetInstanceHealth where

instance NFData SetInstanceHealth where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetInstanceHealthResponse' with the minimum fields required to make a request.
--
setInstanceHealthResponse
    :: SetInstanceHealthResponse
setInstanceHealthResponse = SetInstanceHealthResponse'


instance NFData SetInstanceHealthResponse where
