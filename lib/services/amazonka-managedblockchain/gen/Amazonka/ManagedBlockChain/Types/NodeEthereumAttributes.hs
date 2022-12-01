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
-- Module      : Amazonka.ManagedBlockChain.Types.NodeEthereumAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeEthereumAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Attributes of an Ethereum node.
--
-- /See:/ 'newNodeEthereumAttributes' smart constructor.
data NodeEthereumAttributes = NodeEthereumAttributes'
  { -- | The endpoint on which the Ethereum node listens to run Ethereum API
    -- methods over HTTP connections from a client. Use this endpoint in client
    -- code for smart contracts when using an HTTP connection. Connections to
    -- this endpoint are authenticated using
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
    httpEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The endpoint on which the Ethereum node listens to run Ethereum JSON-RPC
    -- methods over WebSocket connections from a client. Use this endpoint in
    -- client code for smart contracts when using a WebSocket connection.
    -- Connections to this endpoint are authenticated using
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
    webSocketEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeEthereumAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'nodeEthereumAttributes_httpEndpoint' - The endpoint on which the Ethereum node listens to run Ethereum API
-- methods over HTTP connections from a client. Use this endpoint in client
-- code for smart contracts when using an HTTP connection. Connections to
-- this endpoint are authenticated using
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- 'webSocketEndpoint', 'nodeEthereumAttributes_webSocketEndpoint' - The endpoint on which the Ethereum node listens to run Ethereum JSON-RPC
-- methods over WebSocket connections from a client. Use this endpoint in
-- client code for smart contracts when using a WebSocket connection.
-- Connections to this endpoint are authenticated using
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
newNodeEthereumAttributes ::
  NodeEthereumAttributes
newNodeEthereumAttributes =
  NodeEthereumAttributes'
    { httpEndpoint =
        Prelude.Nothing,
      webSocketEndpoint = Prelude.Nothing
    }

-- | The endpoint on which the Ethereum node listens to run Ethereum API
-- methods over HTTP connections from a client. Use this endpoint in client
-- code for smart contracts when using an HTTP connection. Connections to
-- this endpoint are authenticated using
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
nodeEthereumAttributes_httpEndpoint :: Lens.Lens' NodeEthereumAttributes (Prelude.Maybe Prelude.Text)
nodeEthereumAttributes_httpEndpoint = Lens.lens (\NodeEthereumAttributes' {httpEndpoint} -> httpEndpoint) (\s@NodeEthereumAttributes' {} a -> s {httpEndpoint = a} :: NodeEthereumAttributes)

-- | The endpoint on which the Ethereum node listens to run Ethereum JSON-RPC
-- methods over WebSocket connections from a client. Use this endpoint in
-- client code for smart contracts when using a WebSocket connection.
-- Connections to this endpoint are authenticated using
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
nodeEthereumAttributes_webSocketEndpoint :: Lens.Lens' NodeEthereumAttributes (Prelude.Maybe Prelude.Text)
nodeEthereumAttributes_webSocketEndpoint = Lens.lens (\NodeEthereumAttributes' {webSocketEndpoint} -> webSocketEndpoint) (\s@NodeEthereumAttributes' {} a -> s {webSocketEndpoint = a} :: NodeEthereumAttributes)

instance Core.FromJSON NodeEthereumAttributes where
  parseJSON =
    Core.withObject
      "NodeEthereumAttributes"
      ( \x ->
          NodeEthereumAttributes'
            Prelude.<$> (x Core..:? "HttpEndpoint")
            Prelude.<*> (x Core..:? "WebSocketEndpoint")
      )

instance Prelude.Hashable NodeEthereumAttributes where
  hashWithSalt _salt NodeEthereumAttributes' {..} =
    _salt `Prelude.hashWithSalt` httpEndpoint
      `Prelude.hashWithSalt` webSocketEndpoint

instance Prelude.NFData NodeEthereumAttributes where
  rnf NodeEthereumAttributes' {..} =
    Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf webSocketEndpoint
