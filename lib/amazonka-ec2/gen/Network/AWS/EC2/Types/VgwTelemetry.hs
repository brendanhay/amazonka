{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VgwTelemetry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VgwTelemetry
  ( VgwTelemetry (..)
  -- * Smart constructor
  , mkVgwTelemetry
  -- * Lenses
  , vtAcceptedRouteCount
  , vtCertificateArn
  , vtLastStatusChange
  , vtOutsideIpAddress
  , vtStatus
  , vtStatusMessage
  ) where

import qualified Network.AWS.EC2.Types.TelemetryStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes telemetry for a VPN tunnel.
--
-- /See:/ 'mkVgwTelemetry' smart constructor.
data VgwTelemetry = VgwTelemetry'
  { acceptedRouteCount :: Core.Maybe Core.Int
    -- ^ The number of accepted routes.
  , certificateArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
  , lastStatusChange :: Core.Maybe Core.UTCTime
    -- ^ The date and time of the last change in status.
  , outsideIpAddress :: Core.Maybe Core.Text
    -- ^ The Internet-routable IP address of the virtual private gateway's outside interface.
  , status :: Core.Maybe Types.TelemetryStatus
    -- ^ The status of the VPN tunnel.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ If an error occurs, a description of the error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VgwTelemetry' value with any optional fields omitted.
mkVgwTelemetry
    :: VgwTelemetry
mkVgwTelemetry
  = VgwTelemetry'{acceptedRouteCount = Core.Nothing,
                  certificateArn = Core.Nothing, lastStatusChange = Core.Nothing,
                  outsideIpAddress = Core.Nothing, status = Core.Nothing,
                  statusMessage = Core.Nothing}

-- | The number of accepted routes.
--
-- /Note:/ Consider using 'acceptedRouteCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtAcceptedRouteCount :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Int)
vtAcceptedRouteCount = Lens.field @"acceptedRouteCount"
{-# INLINEABLE vtAcceptedRouteCount #-}
{-# DEPRECATED acceptedRouteCount "Use generic-lens or generic-optics with 'acceptedRouteCount' instead"  #-}

-- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtCertificateArn :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Text)
vtCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE vtCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The date and time of the last change in status.
--
-- /Note:/ Consider using 'lastStatusChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtLastStatusChange :: Lens.Lens' VgwTelemetry (Core.Maybe Core.UTCTime)
vtLastStatusChange = Lens.field @"lastStatusChange"
{-# INLINEABLE vtLastStatusChange #-}
{-# DEPRECATED lastStatusChange "Use generic-lens or generic-optics with 'lastStatusChange' instead"  #-}

-- | The Internet-routable IP address of the virtual private gateway's outside interface.
--
-- /Note:/ Consider using 'outsideIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtOutsideIpAddress :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Text)
vtOutsideIpAddress = Lens.field @"outsideIpAddress"
{-# INLINEABLE vtOutsideIpAddress #-}
{-# DEPRECATED outsideIpAddress "Use generic-lens or generic-optics with 'outsideIpAddress' instead"  #-}

-- | The status of the VPN tunnel.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtStatus :: Lens.Lens' VgwTelemetry (Core.Maybe Types.TelemetryStatus)
vtStatus = Lens.field @"status"
{-# INLINEABLE vtStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | If an error occurs, a description of the error.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtStatusMessage :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Text)
vtStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE vtStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

instance Core.FromXML VgwTelemetry where
        parseXML x
          = VgwTelemetry' Core.<$>
              (x Core..@? "acceptedRouteCount") Core.<*>
                x Core..@? "certificateArn"
                Core.<*> x Core..@? "lastStatusChange"
                Core.<*> x Core..@? "outsideIpAddress"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "statusMessage"
