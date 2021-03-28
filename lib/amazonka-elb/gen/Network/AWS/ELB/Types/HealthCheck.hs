{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.HealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.HealthCheck
  ( HealthCheck (..)
  -- * Smart constructor
  , mkHealthCheck
  -- * Lenses
  , hcTarget
  , hcInterval
  , hcTimeout
  , hcUnhealthyThreshold
  , hcHealthyThreshold
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.Target as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a health check.
--
-- /See:/ 'mkHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { target :: Types.Target
    -- ^ The instance being checked. The protocol is either TCP, HTTP, HTTPS, or SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example "TCP:5000". In this case, a health check simply attempts to open a TCP connection to the instance on the specified port. Failure to connect within the configured timeout is considered unhealthy.
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
-- For HTTP/HTTPS, you must include a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing; grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued to the instance on the given port and path. Any answer other than "200 OK" within the timeout period is considered unhealthy.
-- The total length of the HTTP ping target must be 1024 16-bit Unicode characters or less.
  , interval :: Core.Natural
    -- ^ The approximate interval, in seconds, between health checks of an individual instance.
  , timeout :: Core.Natural
    -- ^ The amount of time, in seconds, during which no response means a failed health check.
--
-- This value must be less than the @Interval@ value.
  , unhealthyThreshold :: Core.Natural
    -- ^ The number of consecutive health check failures required before moving the instance to the @Unhealthy@ state.
  , healthyThreshold :: Core.Natural
    -- ^ The number of consecutive health checks successes required before moving the instance to the @Healthy@ state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HealthCheck' value with any optional fields omitted.
mkHealthCheck
    :: Types.Target -- ^ 'target'
    -> Core.Natural -- ^ 'interval'
    -> Core.Natural -- ^ 'timeout'
    -> Core.Natural -- ^ 'unhealthyThreshold'
    -> Core.Natural -- ^ 'healthyThreshold'
    -> HealthCheck
mkHealthCheck target interval timeout unhealthyThreshold
  healthyThreshold
  = HealthCheck'{target, interval, timeout, unhealthyThreshold,
                 healthyThreshold}

-- | The instance being checked. The protocol is either TCP, HTTP, HTTPS, or SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example "TCP:5000". In this case, a health check simply attempts to open a TCP connection to the instance on the specified port. Failure to connect within the configured timeout is considered unhealthy.
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
-- For HTTP/HTTPS, you must include a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing; grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued to the instance on the given port and path. Any answer other than "200 OK" within the timeout period is considered unhealthy.
-- The total length of the HTTP ping target must be 1024 16-bit Unicode characters or less.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcTarget :: Lens.Lens' HealthCheck Types.Target
hcTarget = Lens.field @"target"
{-# INLINEABLE hcTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

-- | The approximate interval, in seconds, between health checks of an individual instance.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcInterval :: Lens.Lens' HealthCheck Core.Natural
hcInterval = Lens.field @"interval"
{-# INLINEABLE hcInterval #-}
{-# DEPRECATED interval "Use generic-lens or generic-optics with 'interval' instead"  #-}

-- | The amount of time, in seconds, during which no response means a failed health check.
--
-- This value must be less than the @Interval@ value.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcTimeout :: Lens.Lens' HealthCheck Core.Natural
hcTimeout = Lens.field @"timeout"
{-# INLINEABLE hcTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

-- | The number of consecutive health check failures required before moving the instance to the @Unhealthy@ state.
--
-- /Note:/ Consider using 'unhealthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcUnhealthyThreshold :: Lens.Lens' HealthCheck Core.Natural
hcUnhealthyThreshold = Lens.field @"unhealthyThreshold"
{-# INLINEABLE hcUnhealthyThreshold #-}
{-# DEPRECATED unhealthyThreshold "Use generic-lens or generic-optics with 'unhealthyThreshold' instead"  #-}

-- | The number of consecutive health checks successes required before moving the instance to the @Healthy@ state.
--
-- /Note:/ Consider using 'healthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHealthyThreshold :: Lens.Lens' HealthCheck Core.Natural
hcHealthyThreshold = Lens.field @"healthyThreshold"
{-# INLINEABLE hcHealthyThreshold #-}
{-# DEPRECATED healthyThreshold "Use generic-lens or generic-optics with 'healthyThreshold' instead"  #-}

instance Core.ToQuery HealthCheck where
        toQuery HealthCheck{..}
          = Core.toQueryPair "Target" target Core.<>
              Core.toQueryPair "Interval" interval
              Core.<> Core.toQueryPair "Timeout" timeout
              Core.<> Core.toQueryPair "UnhealthyThreshold" unhealthyThreshold
              Core.<> Core.toQueryPair "HealthyThreshold" healthyThreshold

instance Core.FromXML HealthCheck where
        parseXML x
          = HealthCheck' Core.<$>
              (x Core..@ "Target") Core.<*> x Core..@ "Interval" Core.<*>
                x Core..@ "Timeout"
                Core.<*> x Core..@ "UnhealthyThreshold"
                Core.<*> x Core..@ "HealthyThreshold"
