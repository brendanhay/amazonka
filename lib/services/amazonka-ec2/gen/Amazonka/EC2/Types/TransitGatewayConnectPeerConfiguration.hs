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
-- Module      : Amazonka.EC2.Types.TransitGatewayConnectPeerConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayConnectPeerConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ProtocolValue
import Amazonka.EC2.Types.TransitGatewayAttachmentBgpConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the Connect peer details.
--
-- /See:/ 'newTransitGatewayConnectPeerConfiguration' smart constructor.
data TransitGatewayConnectPeerConfiguration = TransitGatewayConnectPeerConfiguration'
  { -- | The Connect peer IP address on the transit gateway side of the tunnel.
    transitGatewayAddress :: Prelude.Maybe Prelude.Text,
    -- | The BGP configuration details.
    bgpConfigurations :: Prelude.Maybe [TransitGatewayAttachmentBgpConfiguration],
    -- | The range of interior BGP peer IP addresses.
    insideCidrBlocks :: Prelude.Maybe [Prelude.Text],
    -- | The Connect peer IP address on the appliance side of the tunnel.
    peerAddress :: Prelude.Maybe Prelude.Text,
    -- | The tunnel protocol.
    protocol :: Prelude.Maybe ProtocolValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayConnectPeerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayAddress', 'transitGatewayConnectPeerConfiguration_transitGatewayAddress' - The Connect peer IP address on the transit gateway side of the tunnel.
--
-- 'bgpConfigurations', 'transitGatewayConnectPeerConfiguration_bgpConfigurations' - The BGP configuration details.
--
-- 'insideCidrBlocks', 'transitGatewayConnectPeerConfiguration_insideCidrBlocks' - The range of interior BGP peer IP addresses.
--
-- 'peerAddress', 'transitGatewayConnectPeerConfiguration_peerAddress' - The Connect peer IP address on the appliance side of the tunnel.
--
-- 'protocol', 'transitGatewayConnectPeerConfiguration_protocol' - The tunnel protocol.
newTransitGatewayConnectPeerConfiguration ::
  TransitGatewayConnectPeerConfiguration
newTransitGatewayConnectPeerConfiguration =
  TransitGatewayConnectPeerConfiguration'
    { transitGatewayAddress =
        Prelude.Nothing,
      bgpConfigurations = Prelude.Nothing,
      insideCidrBlocks = Prelude.Nothing,
      peerAddress = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The Connect peer IP address on the transit gateway side of the tunnel.
transitGatewayConnectPeerConfiguration_transitGatewayAddress :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerConfiguration_transitGatewayAddress = Lens.lens (\TransitGatewayConnectPeerConfiguration' {transitGatewayAddress} -> transitGatewayAddress) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {transitGatewayAddress = a} :: TransitGatewayConnectPeerConfiguration)

-- | The BGP configuration details.
transitGatewayConnectPeerConfiguration_bgpConfigurations :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe [TransitGatewayAttachmentBgpConfiguration])
transitGatewayConnectPeerConfiguration_bgpConfigurations = Lens.lens (\TransitGatewayConnectPeerConfiguration' {bgpConfigurations} -> bgpConfigurations) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {bgpConfigurations = a} :: TransitGatewayConnectPeerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The range of interior BGP peer IP addresses.
transitGatewayConnectPeerConfiguration_insideCidrBlocks :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe [Prelude.Text])
transitGatewayConnectPeerConfiguration_insideCidrBlocks = Lens.lens (\TransitGatewayConnectPeerConfiguration' {insideCidrBlocks} -> insideCidrBlocks) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {insideCidrBlocks = a} :: TransitGatewayConnectPeerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Connect peer IP address on the appliance side of the tunnel.
transitGatewayConnectPeerConfiguration_peerAddress :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerConfiguration_peerAddress = Lens.lens (\TransitGatewayConnectPeerConfiguration' {peerAddress} -> peerAddress) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {peerAddress = a} :: TransitGatewayConnectPeerConfiguration)

-- | The tunnel protocol.
transitGatewayConnectPeerConfiguration_protocol :: Lens.Lens' TransitGatewayConnectPeerConfiguration (Prelude.Maybe ProtocolValue)
transitGatewayConnectPeerConfiguration_protocol = Lens.lens (\TransitGatewayConnectPeerConfiguration' {protocol} -> protocol) (\s@TransitGatewayConnectPeerConfiguration' {} a -> s {protocol = a} :: TransitGatewayConnectPeerConfiguration)

instance
  Core.FromXML
    TransitGatewayConnectPeerConfiguration
  where
  parseXML x =
    TransitGatewayConnectPeerConfiguration'
      Prelude.<$> (x Core..@? "transitGatewayAddress")
      Prelude.<*> ( x Core..@? "bgpConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "insideCidrBlocks"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "peerAddress")
      Prelude.<*> (x Core..@? "protocol")

instance
  Prelude.Hashable
    TransitGatewayConnectPeerConfiguration
  where
  hashWithSalt
    _salt
    TransitGatewayConnectPeerConfiguration' {..} =
      _salt `Prelude.hashWithSalt` transitGatewayAddress
        `Prelude.hashWithSalt` bgpConfigurations
        `Prelude.hashWithSalt` insideCidrBlocks
        `Prelude.hashWithSalt` peerAddress
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    TransitGatewayConnectPeerConfiguration
  where
  rnf TransitGatewayConnectPeerConfiguration' {..} =
    Prelude.rnf transitGatewayAddress
      `Prelude.seq` Prelude.rnf bgpConfigurations
      `Prelude.seq` Prelude.rnf insideCidrBlocks
      `Prelude.seq` Prelude.rnf peerAddress
      `Prelude.seq` Prelude.rnf protocol
