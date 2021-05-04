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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ProtocolValue
import Network.AWS.EC2.Types.TransitGatewayAttachmentBgpConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Connect peer details.
--
-- /See:/ 'newTransitGatewayConnectPeerConfiguration' smart constructor.
data TransitGatewayConnectPeerConfiguration = TransitGatewayConnectPeerConfiguration'
  { -- | The Connect peer IP address on the appliance side of the tunnel.
    peerAddress :: Prelude.Maybe Prelude.Text,
    -- | The Connect peer IP address on the transit gateway side of the tunnel.
    transitGatewayAddress :: Prelude.Maybe Prelude.Text,
    -- | The BGP configuration details.
    bgpConfigurations :: Prelude.Maybe [TransitGatewayAttachmentBgpConfiguration],
    -- | The tunnel protocol.
    protocol :: Prelude.Maybe ProtocolValue,
    -- | The range of interior BGP peer IP addresses.
    insideCidrBlocks :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      transitGatewayAddress =
        Prelude.Nothing,
      bgpConfigurations = Prelude.Nothing,
      protocol = Prelude.Nothing,
      insideCidrBlocks = Prelude.Nothing
    }

-- | The Connect peer IP address on the appliance side of the tunnel.
transitGatewayConnectPeerConfiguration_peerAddress :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerConfiguration_peerAddress = Lens.lens (\TransitGatewayConnectPeerConfiguration' {peerAddress} -> peerAddress) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {peerAddress = a} :: TransitGatewayConnectPeerConfiguration)

-- | The Connect peer IP address on the transit gateway side of the tunnel.
transitGatewayConnectPeerConfiguration_transitGatewayAddress :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerConfiguration_transitGatewayAddress = Lens.lens (\TransitGatewayConnectPeerConfiguration' {transitGatewayAddress} -> transitGatewayAddress) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {transitGatewayAddress = a} :: TransitGatewayConnectPeerConfiguration)

-- | The BGP configuration details.
transitGatewayConnectPeerConfiguration_bgpConfigurations :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe [TransitGatewayAttachmentBgpConfiguration])
transitGatewayConnectPeerConfiguration_bgpConfigurations = Lens.lens (\TransitGatewayConnectPeerConfiguration' {bgpConfigurations} -> bgpConfigurations) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {bgpConfigurations = a} :: TransitGatewayConnectPeerConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The tunnel protocol.
transitGatewayConnectPeerConfiguration_protocol :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe ProtocolValue)
transitGatewayConnectPeerConfiguration_protocol = Lens.lens (\TransitGatewayConnectPeerConfiguration' {protocol} -> protocol) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {protocol = a} :: TransitGatewayConnectPeerConfiguration)

-- | The range of interior BGP peer IP addresses.
transitGatewayConnectPeerConfiguration_insideCidrBlocks :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe [Prelude.Text])
transitGatewayConnectPeerConfiguration_insideCidrBlocks = Lens.lens (\TransitGatewayConnectPeerConfiguration' {insideCidrBlocks} -> insideCidrBlocks) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {insideCidrBlocks = a} :: TransitGatewayConnectPeerConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    TransitGatewayConnectPeerConfiguration
  where
  parseXML x =
    TransitGatewayConnectPeerConfiguration'
      Prelude.<$> (x Prelude..@? "peerAddress")
      Prelude.<*> (x Prelude..@? "transitGatewayAddress")
      Prelude.<*> ( x Prelude..@? "bgpConfigurations"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "protocol")
      Prelude.<*> ( x Prelude..@? "insideCidrBlocks"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    TransitGatewayConnectPeerConfiguration

instance
  Prelude.NFData
    TransitGatewayConnectPeerConfiguration
