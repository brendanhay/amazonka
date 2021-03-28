{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.VpnDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.VpnDetails
  ( VpnDetails (..)
  -- * Smart constructor
  , mkVpnDetails
  -- * Lenses
  , vdVpnGatewayIp
  , vdVpnId
  , vdVpnName
  , vdVpnPSK
  , vdVpnState
  , vdVpnTunnelOriginatorIP
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.VpnPSK as Types

-- | Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ > 
--
-- /See:/ 'mkVpnDetails' smart constructor.
data VpnDetails = VpnDetails'
  { vpnGatewayIp :: Core.Maybe Core.Text
    -- ^ The IP address of network traffic from AWS to your on-premises data center.
  , vpnId :: Core.Maybe Core.Text
    -- ^ The ID of the VPN.
  , vpnName :: Core.Maybe Core.Text
    -- ^ The name of the VPN.
  , vpnPSK :: Core.Maybe Types.VpnPSK
    -- ^ The preshared key (PSK) for the VPN.
  , vpnState :: Core.Maybe Core.Text
    -- ^ The state of the VPN.
  , vpnTunnelOriginatorIP :: Core.Maybe Core.Text
    -- ^ The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpnDetails' value with any optional fields omitted.
mkVpnDetails
    :: VpnDetails
mkVpnDetails
  = VpnDetails'{vpnGatewayIp = Core.Nothing, vpnId = Core.Nothing,
                vpnName = Core.Nothing, vpnPSK = Core.Nothing,
                vpnState = Core.Nothing, vpnTunnelOriginatorIP = Core.Nothing}

-- | The IP address of network traffic from AWS to your on-premises data center.
--
-- /Note:/ Consider using 'vpnGatewayIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVpnGatewayIp :: Lens.Lens' VpnDetails (Core.Maybe Core.Text)
vdVpnGatewayIp = Lens.field @"vpnGatewayIp"
{-# INLINEABLE vdVpnGatewayIp #-}
{-# DEPRECATED vpnGatewayIp "Use generic-lens or generic-optics with 'vpnGatewayIp' instead"  #-}

-- | The ID of the VPN.
--
-- /Note:/ Consider using 'vpnId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVpnId :: Lens.Lens' VpnDetails (Core.Maybe Core.Text)
vdVpnId = Lens.field @"vpnId"
{-# INLINEABLE vdVpnId #-}
{-# DEPRECATED vpnId "Use generic-lens or generic-optics with 'vpnId' instead"  #-}

-- | The name of the VPN.
--
-- /Note:/ Consider using 'vpnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVpnName :: Lens.Lens' VpnDetails (Core.Maybe Core.Text)
vdVpnName = Lens.field @"vpnName"
{-# INLINEABLE vdVpnName #-}
{-# DEPRECATED vpnName "Use generic-lens or generic-optics with 'vpnName' instead"  #-}

-- | The preshared key (PSK) for the VPN.
--
-- /Note:/ Consider using 'vpnPSK' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVpnPSK :: Lens.Lens' VpnDetails (Core.Maybe Types.VpnPSK)
vdVpnPSK = Lens.field @"vpnPSK"
{-# INLINEABLE vdVpnPSK #-}
{-# DEPRECATED vpnPSK "Use generic-lens or generic-optics with 'vpnPSK' instead"  #-}

-- | The state of the VPN.
--
-- /Note:/ Consider using 'vpnState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVpnState :: Lens.Lens' VpnDetails (Core.Maybe Core.Text)
vdVpnState = Lens.field @"vpnState"
{-# INLINEABLE vdVpnState #-}
{-# DEPRECATED vpnState "Use generic-lens or generic-optics with 'vpnState' instead"  #-}

-- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- /Note:/ Consider using 'vpnTunnelOriginatorIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVpnTunnelOriginatorIP :: Lens.Lens' VpnDetails (Core.Maybe Core.Text)
vdVpnTunnelOriginatorIP = Lens.field @"vpnTunnelOriginatorIP"
{-# INLINEABLE vdVpnTunnelOriginatorIP #-}
{-# DEPRECATED vpnTunnelOriginatorIP "Use generic-lens or generic-optics with 'vpnTunnelOriginatorIP' instead"  #-}

instance Core.FromXML VpnDetails where
        parseXML x
          = VpnDetails' Core.<$>
              (x Core..@? "VpnGatewayIp") Core.<*> x Core..@? "VpnId" Core.<*>
                x Core..@? "VpnName"
                Core.<*> x Core..@? "VpnPSK"
                Core.<*> x Core..@? "VpnState"
                Core.<*> x Core..@? "VpnTunnelOriginatorIP"
