{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.HealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.HealthCheck where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a health check.
--
--
--
-- /See:/ 'healthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { _hcTarget :: !Text,
    _hcInterval :: !Nat,
    _hcTimeout :: !Nat,
    _hcUnhealthyThreshold :: !Nat,
    _hcHealthyThreshold :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcTarget' - The instance being checked. The protocol is either TCP, HTTP, HTTPS, or SSL. The range of valid ports is one (1) through 65535. TCP is the default, specified as a TCP: port pair, for example "TCP:5000". In this case, a health check simply attempts to open a TCP connection to the instance on the specified port. Failure to connect within the configured timeout is considered unhealthy. SSL is also specified as SSL: port pair, for example, SSL:5000. For HTTP/HTTPS, you must include a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing; grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued to the instance on the given port and path. Any answer other than "200 OK" within the timeout period is considered unhealthy. The total length of the HTTP ping target must be 1024 16-bit Unicode characters or less.
--
-- * 'hcInterval' - The approximate interval, in seconds, between health checks of an individual instance.
--
-- * 'hcTimeout' - The amount of time, in seconds, during which no response means a failed health check. This value must be less than the @Interval@ value.
--
-- * 'hcUnhealthyThreshold' - The number of consecutive health check failures required before moving the instance to the @Unhealthy@ state.
--
-- * 'hcHealthyThreshold' - The number of consecutive health checks successes required before moving the instance to the @Healthy@ state.
healthCheck ::
  -- | 'hcTarget'
  Text ->
  -- | 'hcInterval'
  Natural ->
  -- | 'hcTimeout'
  Natural ->
  -- | 'hcUnhealthyThreshold'
  Natural ->
  -- | 'hcHealthyThreshold'
  Natural ->
  HealthCheck
healthCheck
  pTarget_
  pInterval_
  pTimeout_
  pUnhealthyThreshold_
  pHealthyThreshold_ =
    HealthCheck'
      { _hcTarget = pTarget_,
        _hcInterval = _Nat # pInterval_,
        _hcTimeout = _Nat # pTimeout_,
        _hcUnhealthyThreshold = _Nat # pUnhealthyThreshold_,
        _hcHealthyThreshold = _Nat # pHealthyThreshold_
      }

-- | The instance being checked. The protocol is either TCP, HTTP, HTTPS, or SSL. The range of valid ports is one (1) through 65535. TCP is the default, specified as a TCP: port pair, for example "TCP:5000". In this case, a health check simply attempts to open a TCP connection to the instance on the specified port. Failure to connect within the configured timeout is considered unhealthy. SSL is also specified as SSL: port pair, for example, SSL:5000. For HTTP/HTTPS, you must include a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing; grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued to the instance on the given port and path. Any answer other than "200 OK" within the timeout period is considered unhealthy. The total length of the HTTP ping target must be 1024 16-bit Unicode characters or less.
hcTarget :: Lens' HealthCheck Text
hcTarget = lens _hcTarget (\s a -> s {_hcTarget = a})

-- | The approximate interval, in seconds, between health checks of an individual instance.
hcInterval :: Lens' HealthCheck Natural
hcInterval = lens _hcInterval (\s a -> s {_hcInterval = a}) . _Nat

-- | The amount of time, in seconds, during which no response means a failed health check. This value must be less than the @Interval@ value.
hcTimeout :: Lens' HealthCheck Natural
hcTimeout = lens _hcTimeout (\s a -> s {_hcTimeout = a}) . _Nat

-- | The number of consecutive health check failures required before moving the instance to the @Unhealthy@ state.
hcUnhealthyThreshold :: Lens' HealthCheck Natural
hcUnhealthyThreshold = lens _hcUnhealthyThreshold (\s a -> s {_hcUnhealthyThreshold = a}) . _Nat

-- | The number of consecutive health checks successes required before moving the instance to the @Healthy@ state.
hcHealthyThreshold :: Lens' HealthCheck Natural
hcHealthyThreshold = lens _hcHealthyThreshold (\s a -> s {_hcHealthyThreshold = a}) . _Nat

instance FromXML HealthCheck where
  parseXML x =
    HealthCheck'
      <$> (x .@ "Target")
      <*> (x .@ "Interval")
      <*> (x .@ "Timeout")
      <*> (x .@ "UnhealthyThreshold")
      <*> (x .@ "HealthyThreshold")

instance Hashable HealthCheck

instance NFData HealthCheck

instance ToQuery HealthCheck where
  toQuery HealthCheck' {..} =
    mconcat
      [ "Target" =: _hcTarget,
        "Interval" =: _hcInterval,
        "Timeout" =: _hcTimeout,
        "UnhealthyThreshold" =: _hcUnhealthyThreshold,
        "HealthyThreshold" =: _hcHealthyThreshold
      ]
