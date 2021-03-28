{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckObservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.HealthCheckObservation
  ( HealthCheckObservation (..)
  -- * Smart constructor
  , mkHealthCheckObservation
  -- * Lenses
  , hcoIPAddress
  , hcoRegion
  , hcoStatusReport
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.HealthCheckRegion as Types
import qualified Network.AWS.Route53.Types.IPAddress as Types
import qualified Network.AWS.Route53.Types.StatusReport as Types

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker.
--
-- /See:/ 'mkHealthCheckObservation' smart constructor.
data HealthCheckObservation = HealthCheckObservation'
  { iPAddress :: Core.Maybe Types.IPAddress
    -- ^ The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
  , region :: Core.Maybe Types.HealthCheckRegion
    -- ^ The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
  , statusReport :: Core.Maybe Types.StatusReport
    -- ^ A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'HealthCheckObservation' value with any optional fields omitted.
mkHealthCheckObservation
    :: HealthCheckObservation
mkHealthCheckObservation
  = HealthCheckObservation'{iPAddress = Core.Nothing,
                            region = Core.Nothing, statusReport = Core.Nothing}

-- | The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
--
-- /Note:/ Consider using 'iPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcoIPAddress :: Lens.Lens' HealthCheckObservation (Core.Maybe Types.IPAddress)
hcoIPAddress = Lens.field @"iPAddress"
{-# INLINEABLE hcoIPAddress #-}
{-# DEPRECATED iPAddress "Use generic-lens or generic-optics with 'iPAddress' instead"  #-}

-- | The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcoRegion :: Lens.Lens' HealthCheckObservation (Core.Maybe Types.HealthCheckRegion)
hcoRegion = Lens.field @"region"
{-# INLINEABLE hcoRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
--
-- /Note:/ Consider using 'statusReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcoStatusReport :: Lens.Lens' HealthCheckObservation (Core.Maybe Types.StatusReport)
hcoStatusReport = Lens.field @"statusReport"
{-# INLINEABLE hcoStatusReport #-}
{-# DEPRECATED statusReport "Use generic-lens or generic-optics with 'statusReport' instead"  #-}

instance Core.FromXML HealthCheckObservation where
        parseXML x
          = HealthCheckObservation' Core.<$>
              (x Core..@? "IPAddress") Core.<*> x Core..@? "Region" Core.<*>
                x Core..@? "StatusReport"
