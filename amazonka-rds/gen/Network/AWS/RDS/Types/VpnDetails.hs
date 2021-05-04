{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.VpnDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.VpnDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the virtual private network (VPN) between the VMware
-- vSphere cluster and the AWS website.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
--
-- /See:/ 'newVpnDetails' smart constructor.
data VpnDetails = VpnDetails'
  { -- | The IP address of network traffic from your on-premises data center. A
    -- custom AZ receives the network traffic.
    vpnTunnelOriginatorIP :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPN.
    vpnId :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPN.
    vpnName :: Prelude.Maybe Prelude.Text,
    -- | The state of the VPN.
    vpnState :: Prelude.Maybe Prelude.Text,
    -- | The IP address of network traffic from AWS to your on-premises data
    -- center.
    vpnGatewayIp :: Prelude.Maybe Prelude.Text,
    -- | The preshared key (PSK) for the VPN.
    vpnPSK :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpnDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnTunnelOriginatorIP', 'vpnDetails_vpnTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
--
-- 'vpnId', 'vpnDetails_vpnId' - The ID of the VPN.
--
-- 'vpnName', 'vpnDetails_vpnName' - The name of the VPN.
--
-- 'vpnState', 'vpnDetails_vpnState' - The state of the VPN.
--
-- 'vpnGatewayIp', 'vpnDetails_vpnGatewayIp' - The IP address of network traffic from AWS to your on-premises data
-- center.
--
-- 'vpnPSK', 'vpnDetails_vpnPSK' - The preshared key (PSK) for the VPN.
newVpnDetails ::
  VpnDetails
newVpnDetails =
  VpnDetails'
    { vpnTunnelOriginatorIP =
        Prelude.Nothing,
      vpnId = Prelude.Nothing,
      vpnName = Prelude.Nothing,
      vpnState = Prelude.Nothing,
      vpnGatewayIp = Prelude.Nothing,
      vpnPSK = Prelude.Nothing
    }

-- | The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
vpnDetails_vpnTunnelOriginatorIP :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnTunnelOriginatorIP = Lens.lens (\VpnDetails' {vpnTunnelOriginatorIP} -> vpnTunnelOriginatorIP) (\s@VpnDetails' {} a -> s {vpnTunnelOriginatorIP = a} :: VpnDetails)

-- | The ID of the VPN.
vpnDetails_vpnId :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnId = Lens.lens (\VpnDetails' {vpnId} -> vpnId) (\s@VpnDetails' {} a -> s {vpnId = a} :: VpnDetails)

-- | The name of the VPN.
vpnDetails_vpnName :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnName = Lens.lens (\VpnDetails' {vpnName} -> vpnName) (\s@VpnDetails' {} a -> s {vpnName = a} :: VpnDetails)

-- | The state of the VPN.
vpnDetails_vpnState :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnState = Lens.lens (\VpnDetails' {vpnState} -> vpnState) (\s@VpnDetails' {} a -> s {vpnState = a} :: VpnDetails)

-- | The IP address of network traffic from AWS to your on-premises data
-- center.
vpnDetails_vpnGatewayIp :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnGatewayIp = Lens.lens (\VpnDetails' {vpnGatewayIp} -> vpnGatewayIp) (\s@VpnDetails' {} a -> s {vpnGatewayIp = a} :: VpnDetails)

-- | The preshared key (PSK) for the VPN.
vpnDetails_vpnPSK :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnPSK = Lens.lens (\VpnDetails' {vpnPSK} -> vpnPSK) (\s@VpnDetails' {} a -> s {vpnPSK = a} :: VpnDetails) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.FromXML VpnDetails where
  parseXML x =
    VpnDetails'
      Prelude.<$> (x Prelude..@? "VpnTunnelOriginatorIP")
      Prelude.<*> (x Prelude..@? "VpnId")
      Prelude.<*> (x Prelude..@? "VpnName")
      Prelude.<*> (x Prelude..@? "VpnState")
      Prelude.<*> (x Prelude..@? "VpnGatewayIp")
      Prelude.<*> (x Prelude..@? "VpnPSK")

instance Prelude.Hashable VpnDetails

instance Prelude.NFData VpnDetails
