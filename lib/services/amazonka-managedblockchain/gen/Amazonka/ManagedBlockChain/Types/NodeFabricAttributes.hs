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
-- Module      : Amazonka.ManagedBlockChain.Types.NodeFabricAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeFabricAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Attributes of Hyperledger Fabric for a peer node on a Hyperledger Fabric
-- network on Managed Blockchain.
--
-- /See:/ 'newNodeFabricAttributes' smart constructor.
data NodeFabricAttributes = NodeFabricAttributes'
  { -- | The endpoint that identifies the peer node for all services except peer
    -- channel-based event services.
    peerEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The endpoint that identifies the peer node for peer channel-based event
    -- services.
    peerEventEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeFabricAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peerEndpoint', 'nodeFabricAttributes_peerEndpoint' - The endpoint that identifies the peer node for all services except peer
-- channel-based event services.
--
-- 'peerEventEndpoint', 'nodeFabricAttributes_peerEventEndpoint' - The endpoint that identifies the peer node for peer channel-based event
-- services.
newNodeFabricAttributes ::
  NodeFabricAttributes
newNodeFabricAttributes =
  NodeFabricAttributes'
    { peerEndpoint =
        Prelude.Nothing,
      peerEventEndpoint = Prelude.Nothing
    }

-- | The endpoint that identifies the peer node for all services except peer
-- channel-based event services.
nodeFabricAttributes_peerEndpoint :: Lens.Lens' NodeFabricAttributes (Prelude.Maybe Prelude.Text)
nodeFabricAttributes_peerEndpoint = Lens.lens (\NodeFabricAttributes' {peerEndpoint} -> peerEndpoint) (\s@NodeFabricAttributes' {} a -> s {peerEndpoint = a} :: NodeFabricAttributes)

-- | The endpoint that identifies the peer node for peer channel-based event
-- services.
nodeFabricAttributes_peerEventEndpoint :: Lens.Lens' NodeFabricAttributes (Prelude.Maybe Prelude.Text)
nodeFabricAttributes_peerEventEndpoint = Lens.lens (\NodeFabricAttributes' {peerEventEndpoint} -> peerEventEndpoint) (\s@NodeFabricAttributes' {} a -> s {peerEventEndpoint = a} :: NodeFabricAttributes)

instance Data.FromJSON NodeFabricAttributes where
  parseJSON =
    Data.withObject
      "NodeFabricAttributes"
      ( \x ->
          NodeFabricAttributes'
            Prelude.<$> (x Data..:? "PeerEndpoint")
            Prelude.<*> (x Data..:? "PeerEventEndpoint")
      )

instance Prelude.Hashable NodeFabricAttributes where
  hashWithSalt _salt NodeFabricAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` peerEndpoint
      `Prelude.hashWithSalt` peerEventEndpoint

instance Prelude.NFData NodeFabricAttributes where
  rnf NodeFabricAttributes' {..} =
    Prelude.rnf peerEndpoint `Prelude.seq`
      Prelude.rnf peerEventEndpoint
