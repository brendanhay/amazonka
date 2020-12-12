{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.VPNDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.VPNDetails
  ( VPNDetails (..),

    -- * Smart constructor
    mkVPNDetails,

    -- * Lenses
    vdVPNName,
    vdVPNTunnelOriginatorIP,
    vdVPNId,
    vdVPNState,
    vdVPNPSK,
    vdVPNGatewayIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
--
-- /See:/ 'mkVPNDetails' smart constructor.
data VPNDetails = VPNDetails'
  { vpnName :: Lude.Maybe Lude.Text,
    vpnTunnelOriginatorIP :: Lude.Maybe Lude.Text,
    vpnId :: Lude.Maybe Lude.Text,
    vpnState :: Lude.Maybe Lude.Text,
    vpnPSK :: Lude.Maybe (Lude.Sensitive Lude.Text),
    vpnGatewayIP :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPNDetails' with the minimum fields required to make a request.
--
-- * 'vpnGatewayIP' - The IP address of network traffic from AWS to your on-premises data center.
-- * 'vpnId' - The ID of the VPN.
-- * 'vpnName' - The name of the VPN.
-- * 'vpnPSK' - The preshared key (PSK) for the VPN.
-- * 'vpnState' - The state of the VPN.
-- * 'vpnTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
mkVPNDetails ::
  VPNDetails
mkVPNDetails =
  VPNDetails'
    { vpnName = Lude.Nothing,
      vpnTunnelOriginatorIP = Lude.Nothing,
      vpnId = Lude.Nothing,
      vpnState = Lude.Nothing,
      vpnPSK = Lude.Nothing,
      vpnGatewayIP = Lude.Nothing
    }

-- | The name of the VPN.
--
-- /Note:/ Consider using 'vpnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVPNName :: Lens.Lens' VPNDetails (Lude.Maybe Lude.Text)
vdVPNName = Lens.lens (vpnName :: VPNDetails -> Lude.Maybe Lude.Text) (\s a -> s {vpnName = a} :: VPNDetails)
{-# DEPRECATED vdVPNName "Use generic-lens or generic-optics with 'vpnName' instead." #-}

-- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- /Note:/ Consider using 'vpnTunnelOriginatorIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVPNTunnelOriginatorIP :: Lens.Lens' VPNDetails (Lude.Maybe Lude.Text)
vdVPNTunnelOriginatorIP = Lens.lens (vpnTunnelOriginatorIP :: VPNDetails -> Lude.Maybe Lude.Text) (\s a -> s {vpnTunnelOriginatorIP = a} :: VPNDetails)
{-# DEPRECATED vdVPNTunnelOriginatorIP "Use generic-lens or generic-optics with 'vpnTunnelOriginatorIP' instead." #-}

-- | The ID of the VPN.
--
-- /Note:/ Consider using 'vpnId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVPNId :: Lens.Lens' VPNDetails (Lude.Maybe Lude.Text)
vdVPNId = Lens.lens (vpnId :: VPNDetails -> Lude.Maybe Lude.Text) (\s a -> s {vpnId = a} :: VPNDetails)
{-# DEPRECATED vdVPNId "Use generic-lens or generic-optics with 'vpnId' instead." #-}

-- | The state of the VPN.
--
-- /Note:/ Consider using 'vpnState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVPNState :: Lens.Lens' VPNDetails (Lude.Maybe Lude.Text)
vdVPNState = Lens.lens (vpnState :: VPNDetails -> Lude.Maybe Lude.Text) (\s a -> s {vpnState = a} :: VPNDetails)
{-# DEPRECATED vdVPNState "Use generic-lens or generic-optics with 'vpnState' instead." #-}

-- | The preshared key (PSK) for the VPN.
--
-- /Note:/ Consider using 'vpnPSK' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVPNPSK :: Lens.Lens' VPNDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
vdVPNPSK = Lens.lens (vpnPSK :: VPNDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {vpnPSK = a} :: VPNDetails)
{-# DEPRECATED vdVPNPSK "Use generic-lens or generic-optics with 'vpnPSK' instead." #-}

-- | The IP address of network traffic from AWS to your on-premises data center.
--
-- /Note:/ Consider using 'vpnGatewayIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVPNGatewayIP :: Lens.Lens' VPNDetails (Lude.Maybe Lude.Text)
vdVPNGatewayIP = Lens.lens (vpnGatewayIP :: VPNDetails -> Lude.Maybe Lude.Text) (\s a -> s {vpnGatewayIP = a} :: VPNDetails)
{-# DEPRECATED vdVPNGatewayIP "Use generic-lens or generic-optics with 'vpnGatewayIP' instead." #-}

instance Lude.FromXML VPNDetails where
  parseXML x =
    VPNDetails'
      Lude.<$> (x Lude..@? "VpnName")
      Lude.<*> (x Lude..@? "VpnTunnelOriginatorIP")
      Lude.<*> (x Lude..@? "VpnId")
      Lude.<*> (x Lude..@? "VpnState")
      Lude.<*> (x Lude..@? "VpnPSK")
      Lude.<*> (x Lude..@? "VpnGatewayIp")
