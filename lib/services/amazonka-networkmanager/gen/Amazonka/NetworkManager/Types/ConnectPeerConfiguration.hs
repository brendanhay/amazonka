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
-- Module      : Amazonka.NetworkManager.Types.ConnectPeerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectPeerConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ConnectPeerBgpConfiguration
import Amazonka.NetworkManager.Types.TunnelProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network Connect peer configuration.
--
-- /See:/ 'newConnectPeerConfiguration' smart constructor.
data ConnectPeerConfiguration = ConnectPeerConfiguration'
  { -- | The Connect peer BGP configurations.
    bgpConfigurations :: Prelude.Maybe [ConnectPeerBgpConfiguration],
    -- | The IP address of a core network.
    coreNetworkAddress :: Prelude.Maybe Prelude.Text,
    -- | The inside IP addresses used for a Connect peer configuration.
    insideCidrBlocks :: Prelude.Maybe [Prelude.Text],
    -- | The IP address of the Connect peer.
    peerAddress :: Prelude.Maybe Prelude.Text,
    -- | The protocol used for a Connect peer configuration.
    protocol :: Prelude.Maybe TunnelProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectPeerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bgpConfigurations', 'connectPeerConfiguration_bgpConfigurations' - The Connect peer BGP configurations.
--
-- 'coreNetworkAddress', 'connectPeerConfiguration_coreNetworkAddress' - The IP address of a core network.
--
-- 'insideCidrBlocks', 'connectPeerConfiguration_insideCidrBlocks' - The inside IP addresses used for a Connect peer configuration.
--
-- 'peerAddress', 'connectPeerConfiguration_peerAddress' - The IP address of the Connect peer.
--
-- 'protocol', 'connectPeerConfiguration_protocol' - The protocol used for a Connect peer configuration.
newConnectPeerConfiguration ::
  ConnectPeerConfiguration
newConnectPeerConfiguration =
  ConnectPeerConfiguration'
    { bgpConfigurations =
        Prelude.Nothing,
      coreNetworkAddress = Prelude.Nothing,
      insideCidrBlocks = Prelude.Nothing,
      peerAddress = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The Connect peer BGP configurations.
connectPeerConfiguration_bgpConfigurations :: Lens.Lens' ConnectPeerConfiguration (Prelude.Maybe [ConnectPeerBgpConfiguration])
connectPeerConfiguration_bgpConfigurations = Lens.lens (\ConnectPeerConfiguration' {bgpConfigurations} -> bgpConfigurations) (\s@ConnectPeerConfiguration' {} a -> s {bgpConfigurations = a} :: ConnectPeerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The IP address of a core network.
connectPeerConfiguration_coreNetworkAddress :: Lens.Lens' ConnectPeerConfiguration (Prelude.Maybe Prelude.Text)
connectPeerConfiguration_coreNetworkAddress = Lens.lens (\ConnectPeerConfiguration' {coreNetworkAddress} -> coreNetworkAddress) (\s@ConnectPeerConfiguration' {} a -> s {coreNetworkAddress = a} :: ConnectPeerConfiguration)

-- | The inside IP addresses used for a Connect peer configuration.
connectPeerConfiguration_insideCidrBlocks :: Lens.Lens' ConnectPeerConfiguration (Prelude.Maybe [Prelude.Text])
connectPeerConfiguration_insideCidrBlocks = Lens.lens (\ConnectPeerConfiguration' {insideCidrBlocks} -> insideCidrBlocks) (\s@ConnectPeerConfiguration' {} a -> s {insideCidrBlocks = a} :: ConnectPeerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The IP address of the Connect peer.
connectPeerConfiguration_peerAddress :: Lens.Lens' ConnectPeerConfiguration (Prelude.Maybe Prelude.Text)
connectPeerConfiguration_peerAddress = Lens.lens (\ConnectPeerConfiguration' {peerAddress} -> peerAddress) (\s@ConnectPeerConfiguration' {} a -> s {peerAddress = a} :: ConnectPeerConfiguration)

-- | The protocol used for a Connect peer configuration.
connectPeerConfiguration_protocol :: Lens.Lens' ConnectPeerConfiguration (Prelude.Maybe TunnelProtocol)
connectPeerConfiguration_protocol = Lens.lens (\ConnectPeerConfiguration' {protocol} -> protocol) (\s@ConnectPeerConfiguration' {} a -> s {protocol = a} :: ConnectPeerConfiguration)

instance Data.FromJSON ConnectPeerConfiguration where
  parseJSON =
    Data.withObject
      "ConnectPeerConfiguration"
      ( \x ->
          ConnectPeerConfiguration'
            Prelude.<$> ( x
                            Data..:? "BgpConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CoreNetworkAddress")
            Prelude.<*> ( x
                            Data..:? "InsideCidrBlocks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PeerAddress")
            Prelude.<*> (x Data..:? "Protocol")
      )

instance Prelude.Hashable ConnectPeerConfiguration where
  hashWithSalt _salt ConnectPeerConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` bgpConfigurations
      `Prelude.hashWithSalt` coreNetworkAddress
      `Prelude.hashWithSalt` insideCidrBlocks
      `Prelude.hashWithSalt` peerAddress
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData ConnectPeerConfiguration where
  rnf ConnectPeerConfiguration' {..} =
    Prelude.rnf bgpConfigurations
      `Prelude.seq` Prelude.rnf coreNetworkAddress
      `Prelude.seq` Prelude.rnf insideCidrBlocks
      `Prelude.seq` Prelude.rnf peerAddress
      `Prelude.seq` Prelude.rnf protocol
