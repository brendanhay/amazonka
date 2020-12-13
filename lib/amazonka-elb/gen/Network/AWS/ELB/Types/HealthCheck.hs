{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.HealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.HealthCheck
  ( HealthCheck (..),

    -- * Smart constructor
    mkHealthCheck,

    -- * Lenses
    hcHealthyThreshold,
    hcInterval,
    hcTimeout,
    hcUnhealthyThreshold,
    hcTarget,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a health check.
--
-- /See:/ 'mkHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { -- | The number of consecutive health checks successes required before moving the instance to the @Healthy@ state.
    healthyThreshold :: Lude.Natural,
    -- | The approximate interval, in seconds, between health checks of an individual instance.
    interval :: Lude.Natural,
    -- | The amount of time, in seconds, during which no response means a failed health check.
    --
    -- This value must be less than the @Interval@ value.
    timeout :: Lude.Natural,
    -- | The number of consecutive health check failures required before moving the instance to the @Unhealthy@ state.
    unhealthyThreshold :: Lude.Natural,
    -- | The instance being checked. The protocol is either TCP, HTTP, HTTPS, or SSL. The range of valid ports is one (1) through 65535.
    --
    -- TCP is the default, specified as a TCP: port pair, for example "TCP:5000". In this case, a health check simply attempts to open a TCP connection to the instance on the specified port. Failure to connect within the configured timeout is considered unhealthy.
    -- SSL is also specified as SSL: port pair, for example, SSL:5000.
    -- For HTTP/HTTPS, you must include a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing; grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued to the instance on the given port and path. Any answer other than "200 OK" within the timeout period is considered unhealthy.
    -- The total length of the HTTP ping target must be 1024 16-bit Unicode characters or less.
    target :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- * 'healthyThreshold' - The number of consecutive health checks successes required before moving the instance to the @Healthy@ state.
-- * 'interval' - The approximate interval, in seconds, between health checks of an individual instance.
-- * 'timeout' - The amount of time, in seconds, during which no response means a failed health check.
--
-- This value must be less than the @Interval@ value.
-- * 'unhealthyThreshold' - The number of consecutive health check failures required before moving the instance to the @Unhealthy@ state.
-- * 'target' - The instance being checked. The protocol is either TCP, HTTP, HTTPS, or SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example "TCP:5000". In this case, a health check simply attempts to open a TCP connection to the instance on the specified port. Failure to connect within the configured timeout is considered unhealthy.
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
-- For HTTP/HTTPS, you must include a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing; grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued to the instance on the given port and path. Any answer other than "200 OK" within the timeout period is considered unhealthy.
-- The total length of the HTTP ping target must be 1024 16-bit Unicode characters or less.
mkHealthCheck ::
  -- | 'healthyThreshold'
  Lude.Natural ->
  -- | 'interval'
  Lude.Natural ->
  -- | 'timeout'
  Lude.Natural ->
  -- | 'unhealthyThreshold'
  Lude.Natural ->
  -- | 'target'
  Lude.Text ->
  HealthCheck
mkHealthCheck
  pHealthyThreshold_
  pInterval_
  pTimeout_
  pUnhealthyThreshold_
  pTarget_ =
    HealthCheck'
      { healthyThreshold = pHealthyThreshold_,
        interval = pInterval_,
        timeout = pTimeout_,
        unhealthyThreshold = pUnhealthyThreshold_,
        target = pTarget_
      }

-- | The number of consecutive health checks successes required before moving the instance to the @Healthy@ state.
--
-- /Note:/ Consider using 'healthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHealthyThreshold :: Lens.Lens' HealthCheck Lude.Natural
hcHealthyThreshold = Lens.lens (healthyThreshold :: HealthCheck -> Lude.Natural) (\s a -> s {healthyThreshold = a} :: HealthCheck)
{-# DEPRECATED hcHealthyThreshold "Use generic-lens or generic-optics with 'healthyThreshold' instead." #-}

-- | The approximate interval, in seconds, between health checks of an individual instance.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcInterval :: Lens.Lens' HealthCheck Lude.Natural
hcInterval = Lens.lens (interval :: HealthCheck -> Lude.Natural) (\s a -> s {interval = a} :: HealthCheck)
{-# DEPRECATED hcInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | The amount of time, in seconds, during which no response means a failed health check.
--
-- This value must be less than the @Interval@ value.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcTimeout :: Lens.Lens' HealthCheck Lude.Natural
hcTimeout = Lens.lens (timeout :: HealthCheck -> Lude.Natural) (\s a -> s {timeout = a} :: HealthCheck)
{-# DEPRECATED hcTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The number of consecutive health check failures required before moving the instance to the @Unhealthy@ state.
--
-- /Note:/ Consider using 'unhealthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcUnhealthyThreshold :: Lens.Lens' HealthCheck Lude.Natural
hcUnhealthyThreshold = Lens.lens (unhealthyThreshold :: HealthCheck -> Lude.Natural) (\s a -> s {unhealthyThreshold = a} :: HealthCheck)
{-# DEPRECATED hcUnhealthyThreshold "Use generic-lens or generic-optics with 'unhealthyThreshold' instead." #-}

-- | The instance being checked. The protocol is either TCP, HTTP, HTTPS, or SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example "TCP:5000". In this case, a health check simply attempts to open a TCP connection to the instance on the specified port. Failure to connect within the configured timeout is considered unhealthy.
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
-- For HTTP/HTTPS, you must include a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing; grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued to the instance on the given port and path. Any answer other than "200 OK" within the timeout period is considered unhealthy.
-- The total length of the HTTP ping target must be 1024 16-bit Unicode characters or less.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcTarget :: Lens.Lens' HealthCheck Lude.Text
hcTarget = Lens.lens (target :: HealthCheck -> Lude.Text) (\s a -> s {target = a} :: HealthCheck)
{-# DEPRECATED hcTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromXML HealthCheck where
  parseXML x =
    HealthCheck'
      Lude.<$> (x Lude..@ "HealthyThreshold")
      Lude.<*> (x Lude..@ "Interval")
      Lude.<*> (x Lude..@ "Timeout")
      Lude.<*> (x Lude..@ "UnhealthyThreshold")
      Lude.<*> (x Lude..@ "Target")

instance Lude.ToQuery HealthCheck where
  toQuery HealthCheck' {..} =
    Lude.mconcat
      [ "HealthyThreshold" Lude.=: healthyThreshold,
        "Interval" Lude.=: interval,
        "Timeout" Lude.=: timeout,
        "UnhealthyThreshold" Lude.=: unhealthyThreshold,
        "Target" Lude.=: target
      ]
