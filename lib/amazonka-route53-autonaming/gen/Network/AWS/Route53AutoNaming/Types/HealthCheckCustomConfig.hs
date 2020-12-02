{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about an optional custom health check. A custom health check, which requires that you use a third-party health checker to evaluate the health of your resources, is useful in the following circumstances:
--
--
--     * You can't use a health check that is defined by @HealthCheckConfig@ because the resource isn't available over the internet. For example, you can use a custom health check when the instance is in an Amazon VPC. (To check the health of resources in a VPC, the health checker must also be in the VPC.)
--
--     * You want to use a third-party health checker regardless of where your resources are.
--
--
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- To change the status of a custom health check, submit an @UpdateInstanceCustomHealthStatus@ request. AWS Cloud Map doesn't monitor the status of the resource, it just keeps a record of the status specified in the most recent @UpdateInstanceCustomHealthStatus@ request.
--
-- Here's how custom health checks work:
--
--     * You create a service and specify a value for @FailureThreshold@ .
--
-- The failure threshold indicates the number of 30-second intervals you want AWS Cloud Map to wait between the time that your application sends an <https://docs.aws.amazon.com/cloud-map/latest/api/API_UpdateInstanceCustomHealthStatus.html UpdateInstanceCustomHealthStatus> request and the time that AWS Cloud Map stops routing internet traffic to the corresponding resource.
--
--     * You register an instance.
--
--     * You configure a third-party health checker to monitor the resource that is associated with the new instance.
--
--     * The third-party health-checker determines that the resource is unhealthy and notifies your application.
--
--     * Your application submits an @UpdateInstanceCustomHealthStatus@ request.
--
--     * AWS Cloud Map waits for (@FailureThreshold@ x 30) seconds.
--
--     * If another @UpdateInstanceCustomHealthStatus@ request doesn't arrive during that time to change the status back to healthy, AWS Cloud Map stops routing traffic to the resource.
--
--
--
--
-- /See:/ 'healthCheckCustomConfig' smart constructor.
newtype HealthCheckCustomConfig = HealthCheckCustomConfig'
  { _hcccFailureThreshold ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HealthCheckCustomConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcccFailureThreshold' - /Important:/ This parameter has been deprecated and is always set to 1. AWS Cloud Map waits for approximately 30 seconds after receiving an @UpdateInstanceCustomHealthStatus@ request before changing the status of the service instance. The number of 30-second intervals that you want AWS Cloud Map to wait after receiving an @UpdateInstanceCustomHealthStatus@ request before it changes the health status of a service instance. Sending a second or subsequent @UpdateInstanceCustomHealthStatus@ request with the same value before 30 seconds has passed doesn't accelerate the change. AWS Cloud Map still waits @30@ seconds after the first request to make the change.
healthCheckCustomConfig ::
  HealthCheckCustomConfig
healthCheckCustomConfig =
  HealthCheckCustomConfig' {_hcccFailureThreshold = Nothing}

-- | /Important:/ This parameter has been deprecated and is always set to 1. AWS Cloud Map waits for approximately 30 seconds after receiving an @UpdateInstanceCustomHealthStatus@ request before changing the status of the service instance. The number of 30-second intervals that you want AWS Cloud Map to wait after receiving an @UpdateInstanceCustomHealthStatus@ request before it changes the health status of a service instance. Sending a second or subsequent @UpdateInstanceCustomHealthStatus@ request with the same value before 30 seconds has passed doesn't accelerate the change. AWS Cloud Map still waits @30@ seconds after the first request to make the change.
hcccFailureThreshold :: Lens' HealthCheckCustomConfig (Maybe Natural)
hcccFailureThreshold = lens _hcccFailureThreshold (\s a -> s {_hcccFailureThreshold = a}) . mapping _Nat

instance FromJSON HealthCheckCustomConfig where
  parseJSON =
    withObject
      "HealthCheckCustomConfig"
      (\x -> HealthCheckCustomConfig' <$> (x .:? "FailureThreshold"))

instance Hashable HealthCheckCustomConfig

instance NFData HealthCheckCustomConfig

instance ToJSON HealthCheckCustomConfig where
  toJSON HealthCheckCustomConfig' {..} =
    object
      (catMaybes [("FailureThreshold" .=) <$> _hcccFailureThreshold])
