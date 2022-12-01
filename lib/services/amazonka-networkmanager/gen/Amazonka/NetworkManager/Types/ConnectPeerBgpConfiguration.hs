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
-- Module      : Amazonka.NetworkManager.Types.ConnectPeerBgpConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectPeerBgpConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network BGP configuration.
--
-- /See:/ 'newConnectPeerBgpConfiguration' smart constructor.
data ConnectPeerBgpConfiguration = ConnectPeerBgpConfiguration'
  { -- | The ASN of the Connect peer.
    peerAsn :: Prelude.Maybe Prelude.Integer,
    -- | The address of a core network.
    coreNetworkAddress :: Prelude.Maybe Prelude.Text,
    -- | The ASN of the Coret Network.
    coreNetworkAsn :: Prelude.Maybe Prelude.Integer,
    -- | The address of a core network Connect peer.
    peerAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectPeerBgpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peerAsn', 'connectPeerBgpConfiguration_peerAsn' - The ASN of the Connect peer.
--
-- 'coreNetworkAddress', 'connectPeerBgpConfiguration_coreNetworkAddress' - The address of a core network.
--
-- 'coreNetworkAsn', 'connectPeerBgpConfiguration_coreNetworkAsn' - The ASN of the Coret Network.
--
-- 'peerAddress', 'connectPeerBgpConfiguration_peerAddress' - The address of a core network Connect peer.
newConnectPeerBgpConfiguration ::
  ConnectPeerBgpConfiguration
newConnectPeerBgpConfiguration =
  ConnectPeerBgpConfiguration'
    { peerAsn =
        Prelude.Nothing,
      coreNetworkAddress = Prelude.Nothing,
      coreNetworkAsn = Prelude.Nothing,
      peerAddress = Prelude.Nothing
    }

-- | The ASN of the Connect peer.
connectPeerBgpConfiguration_peerAsn :: Lens.Lens' ConnectPeerBgpConfiguration (Prelude.Maybe Prelude.Integer)
connectPeerBgpConfiguration_peerAsn = Lens.lens (\ConnectPeerBgpConfiguration' {peerAsn} -> peerAsn) (\s@ConnectPeerBgpConfiguration' {} a -> s {peerAsn = a} :: ConnectPeerBgpConfiguration)

-- | The address of a core network.
connectPeerBgpConfiguration_coreNetworkAddress :: Lens.Lens' ConnectPeerBgpConfiguration (Prelude.Maybe Prelude.Text)
connectPeerBgpConfiguration_coreNetworkAddress = Lens.lens (\ConnectPeerBgpConfiguration' {coreNetworkAddress} -> coreNetworkAddress) (\s@ConnectPeerBgpConfiguration' {} a -> s {coreNetworkAddress = a} :: ConnectPeerBgpConfiguration)

-- | The ASN of the Coret Network.
connectPeerBgpConfiguration_coreNetworkAsn :: Lens.Lens' ConnectPeerBgpConfiguration (Prelude.Maybe Prelude.Integer)
connectPeerBgpConfiguration_coreNetworkAsn = Lens.lens (\ConnectPeerBgpConfiguration' {coreNetworkAsn} -> coreNetworkAsn) (\s@ConnectPeerBgpConfiguration' {} a -> s {coreNetworkAsn = a} :: ConnectPeerBgpConfiguration)

-- | The address of a core network Connect peer.
connectPeerBgpConfiguration_peerAddress :: Lens.Lens' ConnectPeerBgpConfiguration (Prelude.Maybe Prelude.Text)
connectPeerBgpConfiguration_peerAddress = Lens.lens (\ConnectPeerBgpConfiguration' {peerAddress} -> peerAddress) (\s@ConnectPeerBgpConfiguration' {} a -> s {peerAddress = a} :: ConnectPeerBgpConfiguration)

instance Core.FromJSON ConnectPeerBgpConfiguration where
  parseJSON =
    Core.withObject
      "ConnectPeerBgpConfiguration"
      ( \x ->
          ConnectPeerBgpConfiguration'
            Prelude.<$> (x Core..:? "PeerAsn")
            Prelude.<*> (x Core..:? "CoreNetworkAddress")
            Prelude.<*> (x Core..:? "CoreNetworkAsn")
            Prelude.<*> (x Core..:? "PeerAddress")
      )

instance Prelude.Hashable ConnectPeerBgpConfiguration where
  hashWithSalt _salt ConnectPeerBgpConfiguration' {..} =
    _salt `Prelude.hashWithSalt` peerAsn
      `Prelude.hashWithSalt` coreNetworkAddress
      `Prelude.hashWithSalt` coreNetworkAsn
      `Prelude.hashWithSalt` peerAddress

instance Prelude.NFData ConnectPeerBgpConfiguration where
  rnf ConnectPeerBgpConfiguration' {..} =
    Prelude.rnf peerAsn
      `Prelude.seq` Prelude.rnf coreNetworkAddress
      `Prelude.seq` Prelude.rnf coreNetworkAsn
      `Prelude.seq` Prelude.rnf peerAddress
