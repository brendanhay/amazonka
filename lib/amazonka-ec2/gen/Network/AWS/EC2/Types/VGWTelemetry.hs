{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VGWTelemetry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VGWTelemetry
  ( VGWTelemetry (..),

    -- * Smart constructor
    mkVGWTelemetry,

    -- * Lenses
    vtStatus,
    vtOutsideIPAddress,
    vtCertificateARN,
    vtLastStatusChange,
    vtAcceptedRouteCount,
    vtStatusMessage,
  )
where

import Network.AWS.EC2.Types.TelemetryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes telemetry for a VPN tunnel.
--
-- /See:/ 'mkVGWTelemetry' smart constructor.
data VGWTelemetry = VGWTelemetry'
  { -- | The status of the VPN tunnel.
    status :: Lude.Maybe TelemetryStatus,
    -- | The Internet-routable IP address of the virtual private gateway's outside interface.
    outsideIPAddress :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The date and time of the last change in status.
    lastStatusChange :: Lude.Maybe Lude.DateTime,
    -- | The number of accepted routes.
    acceptedRouteCount :: Lude.Maybe Lude.Int,
    -- | If an error occurs, a description of the error.
    statusMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VGWTelemetry' with the minimum fields required to make a request.
--
-- * 'status' - The status of the VPN tunnel.
-- * 'outsideIPAddress' - The Internet-routable IP address of the virtual private gateway's outside interface.
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
-- * 'lastStatusChange' - The date and time of the last change in status.
-- * 'acceptedRouteCount' - The number of accepted routes.
-- * 'statusMessage' - If an error occurs, a description of the error.
mkVGWTelemetry ::
  VGWTelemetry
mkVGWTelemetry =
  VGWTelemetry'
    { status = Lude.Nothing,
      outsideIPAddress = Lude.Nothing,
      certificateARN = Lude.Nothing,
      lastStatusChange = Lude.Nothing,
      acceptedRouteCount = Lude.Nothing,
      statusMessage = Lude.Nothing
    }

-- | The status of the VPN tunnel.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtStatus :: Lens.Lens' VGWTelemetry (Lude.Maybe TelemetryStatus)
vtStatus = Lens.lens (status :: VGWTelemetry -> Lude.Maybe TelemetryStatus) (\s a -> s {status = a} :: VGWTelemetry)
{-# DEPRECATED vtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Internet-routable IP address of the virtual private gateway's outside interface.
--
-- /Note:/ Consider using 'outsideIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtOutsideIPAddress :: Lens.Lens' VGWTelemetry (Lude.Maybe Lude.Text)
vtOutsideIPAddress = Lens.lens (outsideIPAddress :: VGWTelemetry -> Lude.Maybe Lude.Text) (\s a -> s {outsideIPAddress = a} :: VGWTelemetry)
{-# DEPRECATED vtOutsideIPAddress "Use generic-lens or generic-optics with 'outsideIPAddress' instead." #-}

-- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtCertificateARN :: Lens.Lens' VGWTelemetry (Lude.Maybe Lude.Text)
vtCertificateARN = Lens.lens (certificateARN :: VGWTelemetry -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: VGWTelemetry)
{-# DEPRECATED vtCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The date and time of the last change in status.
--
-- /Note:/ Consider using 'lastStatusChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtLastStatusChange :: Lens.Lens' VGWTelemetry (Lude.Maybe Lude.DateTime)
vtLastStatusChange = Lens.lens (lastStatusChange :: VGWTelemetry -> Lude.Maybe Lude.DateTime) (\s a -> s {lastStatusChange = a} :: VGWTelemetry)
{-# DEPRECATED vtLastStatusChange "Use generic-lens or generic-optics with 'lastStatusChange' instead." #-}

-- | The number of accepted routes.
--
-- /Note:/ Consider using 'acceptedRouteCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtAcceptedRouteCount :: Lens.Lens' VGWTelemetry (Lude.Maybe Lude.Int)
vtAcceptedRouteCount = Lens.lens (acceptedRouteCount :: VGWTelemetry -> Lude.Maybe Lude.Int) (\s a -> s {acceptedRouteCount = a} :: VGWTelemetry)
{-# DEPRECATED vtAcceptedRouteCount "Use generic-lens or generic-optics with 'acceptedRouteCount' instead." #-}

-- | If an error occurs, a description of the error.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtStatusMessage :: Lens.Lens' VGWTelemetry (Lude.Maybe Lude.Text)
vtStatusMessage = Lens.lens (statusMessage :: VGWTelemetry -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: VGWTelemetry)
{-# DEPRECATED vtStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Lude.FromXML VGWTelemetry where
  parseXML x =
    VGWTelemetry'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "outsideIpAddress")
      Lude.<*> (x Lude..@? "certificateArn")
      Lude.<*> (x Lude..@? "lastStatusChange")
      Lude.<*> (x Lude..@? "acceptedRouteCount")
      Lude.<*> (x Lude..@? "statusMessage")
