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
-- Module      : Amazonka.ManagedBlockChain.Types.NodeFabricLogPublishingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeFabricLogPublishingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.LogConfigurations
import qualified Amazonka.Prelude as Prelude

-- | Configuration properties for logging events associated with a peer node
-- owned by a member in a Managed Blockchain network.
--
-- /See:/ 'newNodeFabricLogPublishingConfiguration' smart constructor.
data NodeFabricLogPublishingConfiguration = NodeFabricLogPublishingConfiguration'
  { -- | Configuration properties for logging events associated with chaincode
    -- execution on a peer node. Chaincode logs contain the results of
    -- instantiating, invoking, and querying the chaincode. A peer can run
    -- multiple instances of chaincode. When enabled, a log stream is created
    -- for all chaincodes, with an individual log stream for each chaincode.
    chaincodeLogs :: Prelude.Maybe LogConfigurations,
    -- | Configuration properties for a peer node log. Peer node logs contain
    -- messages generated when your client submits transaction proposals to
    -- peer nodes, requests to join channels, enrolls an admin peer, and lists
    -- the chaincode instances on a peer node.
    peerLogs :: Prelude.Maybe LogConfigurations
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeFabricLogPublishingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chaincodeLogs', 'nodeFabricLogPublishingConfiguration_chaincodeLogs' - Configuration properties for logging events associated with chaincode
-- execution on a peer node. Chaincode logs contain the results of
-- instantiating, invoking, and querying the chaincode. A peer can run
-- multiple instances of chaincode. When enabled, a log stream is created
-- for all chaincodes, with an individual log stream for each chaincode.
--
-- 'peerLogs', 'nodeFabricLogPublishingConfiguration_peerLogs' - Configuration properties for a peer node log. Peer node logs contain
-- messages generated when your client submits transaction proposals to
-- peer nodes, requests to join channels, enrolls an admin peer, and lists
-- the chaincode instances on a peer node.
newNodeFabricLogPublishingConfiguration ::
  NodeFabricLogPublishingConfiguration
newNodeFabricLogPublishingConfiguration =
  NodeFabricLogPublishingConfiguration'
    { chaincodeLogs =
        Prelude.Nothing,
      peerLogs = Prelude.Nothing
    }

-- | Configuration properties for logging events associated with chaincode
-- execution on a peer node. Chaincode logs contain the results of
-- instantiating, invoking, and querying the chaincode. A peer can run
-- multiple instances of chaincode. When enabled, a log stream is created
-- for all chaincodes, with an individual log stream for each chaincode.
nodeFabricLogPublishingConfiguration_chaincodeLogs :: Lens.Lens' NodeFabricLogPublishingConfiguration (Prelude.Maybe LogConfigurations)
nodeFabricLogPublishingConfiguration_chaincodeLogs = Lens.lens (\NodeFabricLogPublishingConfiguration' {chaincodeLogs} -> chaincodeLogs) (\s@NodeFabricLogPublishingConfiguration' {} a -> s {chaincodeLogs = a} :: NodeFabricLogPublishingConfiguration)

-- | Configuration properties for a peer node log. Peer node logs contain
-- messages generated when your client submits transaction proposals to
-- peer nodes, requests to join channels, enrolls an admin peer, and lists
-- the chaincode instances on a peer node.
nodeFabricLogPublishingConfiguration_peerLogs :: Lens.Lens' NodeFabricLogPublishingConfiguration (Prelude.Maybe LogConfigurations)
nodeFabricLogPublishingConfiguration_peerLogs = Lens.lens (\NodeFabricLogPublishingConfiguration' {peerLogs} -> peerLogs) (\s@NodeFabricLogPublishingConfiguration' {} a -> s {peerLogs = a} :: NodeFabricLogPublishingConfiguration)

instance
  Data.FromJSON
    NodeFabricLogPublishingConfiguration
  where
  parseJSON =
    Data.withObject
      "NodeFabricLogPublishingConfiguration"
      ( \x ->
          NodeFabricLogPublishingConfiguration'
            Prelude.<$> (x Data..:? "ChaincodeLogs")
            Prelude.<*> (x Data..:? "PeerLogs")
      )

instance
  Prelude.Hashable
    NodeFabricLogPublishingConfiguration
  where
  hashWithSalt
    _salt
    NodeFabricLogPublishingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` chaincodeLogs
        `Prelude.hashWithSalt` peerLogs

instance
  Prelude.NFData
    NodeFabricLogPublishingConfiguration
  where
  rnf NodeFabricLogPublishingConfiguration' {..} =
    Prelude.rnf chaincodeLogs `Prelude.seq`
      Prelude.rnf peerLogs

instance
  Data.ToJSON
    NodeFabricLogPublishingConfiguration
  where
  toJSON NodeFabricLogPublishingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChaincodeLogs" Data..=) Prelude.<$> chaincodeLogs,
            ("PeerLogs" Data..=) Prelude.<$> peerLogs
          ]
      )
