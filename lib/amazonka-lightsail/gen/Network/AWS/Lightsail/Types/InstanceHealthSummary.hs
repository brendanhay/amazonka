{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHealthSummary where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.InstanceHealthReason
import Network.AWS.Lightsail.Types.InstanceHealthState
import Network.AWS.Prelude

-- | Describes information about the health of the instance.
--
--
--
-- /See:/ 'instanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { _ihsInstanceHealth ::
      !(Maybe InstanceHealthState),
    _ihsInstanceName :: !(Maybe Text),
    _ihsInstanceHealthReason ::
      !(Maybe InstanceHealthReason)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceHealthSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ihsInstanceHealth' - Describes the overall instance health. Valid values are below.
--
-- * 'ihsInstanceName' - The name of the Lightsail instance for which you are requesting health check data.
--
-- * 'ihsInstanceHealthReason' - More information about the instance health. If the @instanceHealth@ is @healthy@ , then an @instanceHealthReason@ value is not provided. If __@instanceHealth@ __ is @initial@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Lb.RegistrationInProgress@ __ - The target instance is in the process of being registered with the load balancer.     * __@Lb.InitialHealthChecking@ __ - The Lightsail load balancer is still sending the target instance the minimum number of health checks required to determine its health status. If __@instanceHealth@ __ is @unhealthy@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Instance.ResponseCodeMismatch@ __ - The health checks did not return an expected HTTP code.     * __@Instance.Timeout@ __ - The health check requests timed out.     * __@Instance.FailedHealthChecks@ __ - The health checks failed because the connection to the target instance timed out, the target instance response was malformed, or the target instance failed the health check for an unknown reason.     * __@Lb.InternalError@ __ - The health checks failed due to an internal error. If __@instanceHealth@ __ is @unused@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Instance.NotRegistered@ __ - The target instance is not registered with the target group.     * __@Instance.NotInUse@ __ - The target group is not used by any load balancer, or the target instance is in an Availability Zone that is not enabled for its load balancer.     * __@Instance.IpUnusable@ __ - The target IP address is reserved for use by a Lightsail load balancer.     * __@Instance.InvalidState@ __ - The target is in the stopped or terminated state. If __@instanceHealth@ __ is @draining@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Instance.DeregistrationInProgress@ __ - The target instance is in the process of being deregistered and the deregistration delay period has not expired.
instanceHealthSummary ::
  InstanceHealthSummary
instanceHealthSummary =
  InstanceHealthSummary'
    { _ihsInstanceHealth = Nothing,
      _ihsInstanceName = Nothing,
      _ihsInstanceHealthReason = Nothing
    }

-- | Describes the overall instance health. Valid values are below.
ihsInstanceHealth :: Lens' InstanceHealthSummary (Maybe InstanceHealthState)
ihsInstanceHealth = lens _ihsInstanceHealth (\s a -> s {_ihsInstanceHealth = a})

-- | The name of the Lightsail instance for which you are requesting health check data.
ihsInstanceName :: Lens' InstanceHealthSummary (Maybe Text)
ihsInstanceName = lens _ihsInstanceName (\s a -> s {_ihsInstanceName = a})

-- | More information about the instance health. If the @instanceHealth@ is @healthy@ , then an @instanceHealthReason@ value is not provided. If __@instanceHealth@ __ is @initial@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Lb.RegistrationInProgress@ __ - The target instance is in the process of being registered with the load balancer.     * __@Lb.InitialHealthChecking@ __ - The Lightsail load balancer is still sending the target instance the minimum number of health checks required to determine its health status. If __@instanceHealth@ __ is @unhealthy@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Instance.ResponseCodeMismatch@ __ - The health checks did not return an expected HTTP code.     * __@Instance.Timeout@ __ - The health check requests timed out.     * __@Instance.FailedHealthChecks@ __ - The health checks failed because the connection to the target instance timed out, the target instance response was malformed, or the target instance failed the health check for an unknown reason.     * __@Lb.InternalError@ __ - The health checks failed due to an internal error. If __@instanceHealth@ __ is @unused@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Instance.NotRegistered@ __ - The target instance is not registered with the target group.     * __@Instance.NotInUse@ __ - The target group is not used by any load balancer, or the target instance is in an Availability Zone that is not enabled for its load balancer.     * __@Instance.IpUnusable@ __ - The target IP address is reserved for use by a Lightsail load balancer.     * __@Instance.InvalidState@ __ - The target is in the stopped or terminated state. If __@instanceHealth@ __ is @draining@ , the __@instanceHealthReason@ __ value can be one of the following:     * __@Instance.DeregistrationInProgress@ __ - The target instance is in the process of being deregistered and the deregistration delay period has not expired.
ihsInstanceHealthReason :: Lens' InstanceHealthSummary (Maybe InstanceHealthReason)
ihsInstanceHealthReason = lens _ihsInstanceHealthReason (\s a -> s {_ihsInstanceHealthReason = a})

instance FromJSON InstanceHealthSummary where
  parseJSON =
    withObject
      "InstanceHealthSummary"
      ( \x ->
          InstanceHealthSummary'
            <$> (x .:? "instanceHealth")
            <*> (x .:? "instanceName")
            <*> (x .:? "instanceHealthReason")
      )

instance Hashable InstanceHealthSummary

instance NFData InstanceHealthSummary
