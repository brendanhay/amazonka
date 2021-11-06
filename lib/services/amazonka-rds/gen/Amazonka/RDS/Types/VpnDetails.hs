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
-- Module      : Amazonka.RDS.Types.VpnDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.VpnDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the virtual private network (VPN) between the VMware
-- vSphere cluster and the Amazon Web Services website.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
--
-- /See:/ 'newVpnDetails' smart constructor.
data VpnDetails = VpnDetails'
  { -- | The name of the VPN.
    vpnName :: Prelude.Maybe Prelude.Text,
    -- | The IP address of network traffic from your on-premises data center. A
    -- custom AZ receives the network traffic.
    vpnTunnelOriginatorIP :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPN.
    vpnId :: Prelude.Maybe Prelude.Text,
    -- | The state of the VPN.
    vpnState :: Prelude.Maybe Prelude.Text,
    -- | The preshared key (PSK) for the VPN.
    vpnPSK :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The IP address of network traffic from Amazon Web Services to your
    -- on-premises data center.
    vpnGatewayIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnName', 'vpnDetails_vpnName' - The name of the VPN.
--
-- 'vpnTunnelOriginatorIP', 'vpnDetails_vpnTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
--
-- 'vpnId', 'vpnDetails_vpnId' - The ID of the VPN.
--
-- 'vpnState', 'vpnDetails_vpnState' - The state of the VPN.
--
-- 'vpnPSK', 'vpnDetails_vpnPSK' - The preshared key (PSK) for the VPN.
--
-- 'vpnGatewayIp', 'vpnDetails_vpnGatewayIp' - The IP address of network traffic from Amazon Web Services to your
-- on-premises data center.
newVpnDetails ::
  VpnDetails
newVpnDetails =
  VpnDetails'
    { vpnName = Prelude.Nothing,
      vpnTunnelOriginatorIP = Prelude.Nothing,
      vpnId = Prelude.Nothing,
      vpnState = Prelude.Nothing,
      vpnPSK = Prelude.Nothing,
      vpnGatewayIp = Prelude.Nothing
    }

-- | The name of the VPN.
vpnDetails_vpnName :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnName = Lens.lens (\VpnDetails' {vpnName} -> vpnName) (\s@VpnDetails' {} a -> s {vpnName = a} :: VpnDetails)

-- | The IP address of network traffic from your on-premises data center. A
-- custom AZ receives the network traffic.
vpnDetails_vpnTunnelOriginatorIP :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnTunnelOriginatorIP = Lens.lens (\VpnDetails' {vpnTunnelOriginatorIP} -> vpnTunnelOriginatorIP) (\s@VpnDetails' {} a -> s {vpnTunnelOriginatorIP = a} :: VpnDetails)

-- | The ID of the VPN.
vpnDetails_vpnId :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnId = Lens.lens (\VpnDetails' {vpnId} -> vpnId) (\s@VpnDetails' {} a -> s {vpnId = a} :: VpnDetails)

-- | The state of the VPN.
vpnDetails_vpnState :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnState = Lens.lens (\VpnDetails' {vpnState} -> vpnState) (\s@VpnDetails' {} a -> s {vpnState = a} :: VpnDetails)

-- | The preshared key (PSK) for the VPN.
vpnDetails_vpnPSK :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnPSK = Lens.lens (\VpnDetails' {vpnPSK} -> vpnPSK) (\s@VpnDetails' {} a -> s {vpnPSK = a} :: VpnDetails) Prelude.. Lens.mapping Core._Sensitive

-- | The IP address of network traffic from Amazon Web Services to your
-- on-premises data center.
vpnDetails_vpnGatewayIp :: Lens.Lens' VpnDetails (Prelude.Maybe Prelude.Text)
vpnDetails_vpnGatewayIp = Lens.lens (\VpnDetails' {vpnGatewayIp} -> vpnGatewayIp) (\s@VpnDetails' {} a -> s {vpnGatewayIp = a} :: VpnDetails)

instance Core.FromXML VpnDetails where
  parseXML x =
    VpnDetails'
      Prelude.<$> (x Core..@? "VpnName")
      Prelude.<*> (x Core..@? "VpnTunnelOriginatorIP")
      Prelude.<*> (x Core..@? "VpnId")
      Prelude.<*> (x Core..@? "VpnState")
      Prelude.<*> (x Core..@? "VpnPSK")
      Prelude.<*> (x Core..@? "VpnGatewayIp")

instance Prelude.Hashable VpnDetails

instance Prelude.NFData VpnDetails
