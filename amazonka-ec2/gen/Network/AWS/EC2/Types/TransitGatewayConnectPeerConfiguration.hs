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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ProtocolValue
import Network.AWS.EC2.Types.TransitGatewayAttachmentBgpConfiguration
import qualified Network.AWS.Lens as Lens

-- | Describes the Connect peer details.
--
-- /See:/ 'newTransitGatewayConnectPeerConfiguration' smart constructor.
data TransitGatewayConnectPeerConfiguration = TransitGatewayConnectPeerConfiguration'
  { -- | The Connect peer IP address on the appliance side of the tunnel.
    peerAddress :: Core.Maybe Core.Text,
    -- | The Connect peer IP address on the transit gateway side of the tunnel.
    transitGatewayAddress :: Core.Maybe Core.Text,
    -- | The BGP configuration details.
    bgpConfigurations :: Core.Maybe [TransitGatewayAttachmentBgpConfiguration],
    -- | The tunnel protocol.
    protocol :: Core.Maybe ProtocolValue,
    -- | The range of interior BGP peer IP addresses.
    insideCidrBlocks :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayConnectPeerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peerAddress', 'transitGatewayConnectPeerConfiguration_peerAddress' - The Connect peer IP address on the appliance side of the tunnel.
--
-- 'transitGatewayAddress', 'transitGatewayConnectPeerConfiguration_transitGatewayAddress' - The Connect peer IP address on the transit gateway side of the tunnel.
--
-- 'bgpConfigurations', 'transitGatewayConnectPeerConfiguration_bgpConfigurations' - The BGP configuration details.
--
-- 'protocol', 'transitGatewayConnectPeerConfiguration_protocol' - The tunnel protocol.
--
-- 'insideCidrBlocks', 'transitGatewayConnectPeerConfiguration_insideCidrBlocks' - The range of interior BGP peer IP addresses.
newTransitGatewayConnectPeerConfiguration ::
  TransitGatewayConnectPeerConfiguration
newTransitGatewayConnectPeerConfiguration =
  TransitGatewayConnectPeerConfiguration'
    { peerAddress =
        Core.Nothing,
      transitGatewayAddress =
        Core.Nothing,
      bgpConfigurations = Core.Nothing,
      protocol = Core.Nothing,
      insideCidrBlocks = Core.Nothing
    }

-- | The Connect peer IP address on the appliance side of the tunnel.
transitGatewayConnectPeerConfiguration_peerAddress :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Core.Maybe Core.Text)
transitGatewayConnectPeerConfiguration_peerAddress = Lens.lens (\TransitGatewayConnectPeerConfiguration' {peerAddress} -> peerAddress) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {peerAddress = a} :: TransitGatewayConnectPeerConfiguration)

-- | The Connect peer IP address on the transit gateway side of the tunnel.
transitGatewayConnectPeerConfiguration_transitGatewayAddress :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Core.Maybe Core.Text)
transitGatewayConnectPeerConfiguration_transitGatewayAddress = Lens.lens (\TransitGatewayConnectPeerConfiguration' {transitGatewayAddress} -> transitGatewayAddress) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {transitGatewayAddress = a} :: TransitGatewayConnectPeerConfiguration)

-- | The BGP configuration details.
transitGatewayConnectPeerConfiguration_bgpConfigurations :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Core.Maybe [TransitGatewayAttachmentBgpConfiguration])
transitGatewayConnectPeerConfiguration_bgpConfigurations = Lens.lens (\TransitGatewayConnectPeerConfiguration' {bgpConfigurations} -> bgpConfigurations) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {bgpConfigurations = a} :: TransitGatewayConnectPeerConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The tunnel protocol.
transitGatewayConnectPeerConfiguration_protocol :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Core.Maybe ProtocolValue)
transitGatewayConnectPeerConfiguration_protocol = Lens.lens (\TransitGatewayConnectPeerConfiguration' {protocol} -> protocol) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {protocol = a} :: TransitGatewayConnectPeerConfiguration)

-- | The range of interior BGP peer IP addresses.
transitGatewayConnectPeerConfiguration_insideCidrBlocks :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Core.Maybe [Core.Text])
transitGatewayConnectPeerConfiguration_insideCidrBlocks = Lens.lens (\TransitGatewayConnectPeerConfiguration' {insideCidrBlocks} -> insideCidrBlocks) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {insideCidrBlocks = a} :: TransitGatewayConnectPeerConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromXML
    TransitGatewayConnectPeerConfiguration
  where
  parseXML x =
    TransitGatewayConnectPeerConfiguration'
      Core.<$> (x Core..@? "peerAddress")
      Core.<*> (x Core..@? "transitGatewayAddress")
      Core.<*> ( x Core..@? "bgpConfigurations" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "protocol")
      Core.<*> ( x Core..@? "insideCidrBlocks" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance
  Core.Hashable
    TransitGatewayConnectPeerConfiguration

instance
  Core.NFData
    TransitGatewayConnectPeerConfiguration
