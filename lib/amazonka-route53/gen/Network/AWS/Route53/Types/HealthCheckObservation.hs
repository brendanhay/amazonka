{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckObservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckObservation
  ( HealthCheckObservation (..),

    -- * Smart constructor
    mkHealthCheckObservation,

    -- * Lenses
    hcoIPAddress,
    hcoStatusReport,
    hcoRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HealthCheckRegion
import Network.AWS.Route53.Types.StatusReport

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker.
--
-- /See:/ 'mkHealthCheckObservation' smart constructor.
data HealthCheckObservation = HealthCheckObservation'
  { -- | The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
    ipAddress :: Lude.Maybe Lude.Text,
    -- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
    statusReport :: Lude.Maybe StatusReport,
    -- | The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
    region :: Lude.Maybe HealthCheckRegion
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HealthCheckObservation' with the minimum fields required to make a request.
--
-- * 'ipAddress' - The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
-- * 'statusReport' - A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
-- * 'region' - The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
mkHealthCheckObservation ::
  HealthCheckObservation
mkHealthCheckObservation =
  HealthCheckObservation'
    { ipAddress = Lude.Nothing,
      statusReport = Lude.Nothing,
      region = Lude.Nothing
    }

-- | The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcoIPAddress :: Lens.Lens' HealthCheckObservation (Lude.Maybe Lude.Text)
hcoIPAddress = Lens.lens (ipAddress :: HealthCheckObservation -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: HealthCheckObservation)
{-# DEPRECATED hcoIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
--
-- /Note:/ Consider using 'statusReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcoStatusReport :: Lens.Lens' HealthCheckObservation (Lude.Maybe StatusReport)
hcoStatusReport = Lens.lens (statusReport :: HealthCheckObservation -> Lude.Maybe StatusReport) (\s a -> s {statusReport = a} :: HealthCheckObservation)
{-# DEPRECATED hcoStatusReport "Use generic-lens or generic-optics with 'statusReport' instead." #-}

-- | The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcoRegion :: Lens.Lens' HealthCheckObservation (Lude.Maybe HealthCheckRegion)
hcoRegion = Lens.lens (region :: HealthCheckObservation -> Lude.Maybe HealthCheckRegion) (\s a -> s {region = a} :: HealthCheckObservation)
{-# DEPRECATED hcoRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromXML HealthCheckObservation where
  parseXML x =
    HealthCheckObservation'
      Lude.<$> (x Lude..@? "IPAddress")
      Lude.<*> (x Lude..@? "StatusReport")
      Lude.<*> (x Lude..@? "Region")
