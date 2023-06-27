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
-- Module      : Amazonka.MediaConnect.Types.UpdateBridgeNetworkOutputRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateBridgeNetworkOutputRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Update an existing network output.
--
-- /See:/ 'newUpdateBridgeNetworkOutputRequest' smart constructor.
data UpdateBridgeNetworkOutputRequest = UpdateBridgeNetworkOutputRequest'
  { -- | The network output IP Address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The network output\'s gateway network name.
    networkName :: Prelude.Maybe Prelude.Text,
    -- | The network output port.
    port :: Prelude.Maybe Prelude.Int,
    -- | The network output protocol.
    protocol :: Prelude.Maybe Protocol,
    -- | The network output TTL.
    ttl :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeNetworkOutputRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'updateBridgeNetworkOutputRequest_ipAddress' - The network output IP Address.
--
-- 'networkName', 'updateBridgeNetworkOutputRequest_networkName' - The network output\'s gateway network name.
--
-- 'port', 'updateBridgeNetworkOutputRequest_port' - The network output port.
--
-- 'protocol', 'updateBridgeNetworkOutputRequest_protocol' - The network output protocol.
--
-- 'ttl', 'updateBridgeNetworkOutputRequest_ttl' - The network output TTL.
newUpdateBridgeNetworkOutputRequest ::
  UpdateBridgeNetworkOutputRequest
newUpdateBridgeNetworkOutputRequest =
  UpdateBridgeNetworkOutputRequest'
    { ipAddress =
        Prelude.Nothing,
      networkName = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      ttl = Prelude.Nothing
    }

-- | The network output IP Address.
updateBridgeNetworkOutputRequest_ipAddress :: Lens.Lens' UpdateBridgeNetworkOutputRequest (Prelude.Maybe Prelude.Text)
updateBridgeNetworkOutputRequest_ipAddress = Lens.lens (\UpdateBridgeNetworkOutputRequest' {ipAddress} -> ipAddress) (\s@UpdateBridgeNetworkOutputRequest' {} a -> s {ipAddress = a} :: UpdateBridgeNetworkOutputRequest)

-- | The network output\'s gateway network name.
updateBridgeNetworkOutputRequest_networkName :: Lens.Lens' UpdateBridgeNetworkOutputRequest (Prelude.Maybe Prelude.Text)
updateBridgeNetworkOutputRequest_networkName = Lens.lens (\UpdateBridgeNetworkOutputRequest' {networkName} -> networkName) (\s@UpdateBridgeNetworkOutputRequest' {} a -> s {networkName = a} :: UpdateBridgeNetworkOutputRequest)

-- | The network output port.
updateBridgeNetworkOutputRequest_port :: Lens.Lens' UpdateBridgeNetworkOutputRequest (Prelude.Maybe Prelude.Int)
updateBridgeNetworkOutputRequest_port = Lens.lens (\UpdateBridgeNetworkOutputRequest' {port} -> port) (\s@UpdateBridgeNetworkOutputRequest' {} a -> s {port = a} :: UpdateBridgeNetworkOutputRequest)

-- | The network output protocol.
updateBridgeNetworkOutputRequest_protocol :: Lens.Lens' UpdateBridgeNetworkOutputRequest (Prelude.Maybe Protocol)
updateBridgeNetworkOutputRequest_protocol = Lens.lens (\UpdateBridgeNetworkOutputRequest' {protocol} -> protocol) (\s@UpdateBridgeNetworkOutputRequest' {} a -> s {protocol = a} :: UpdateBridgeNetworkOutputRequest)

-- | The network output TTL.
updateBridgeNetworkOutputRequest_ttl :: Lens.Lens' UpdateBridgeNetworkOutputRequest (Prelude.Maybe Prelude.Int)
updateBridgeNetworkOutputRequest_ttl = Lens.lens (\UpdateBridgeNetworkOutputRequest' {ttl} -> ttl) (\s@UpdateBridgeNetworkOutputRequest' {} a -> s {ttl = a} :: UpdateBridgeNetworkOutputRequest)

instance
  Prelude.Hashable
    UpdateBridgeNetworkOutputRequest
  where
  hashWithSalt
    _salt
    UpdateBridgeNetworkOutputRequest' {..} =
      _salt
        `Prelude.hashWithSalt` ipAddress
        `Prelude.hashWithSalt` networkName
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` ttl

instance
  Prelude.NFData
    UpdateBridgeNetworkOutputRequest
  where
  rnf UpdateBridgeNetworkOutputRequest' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf networkName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf ttl

instance Data.ToJSON UpdateBridgeNetworkOutputRequest where
  toJSON UpdateBridgeNetworkOutputRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ipAddress" Data..=) Prelude.<$> ipAddress,
            ("networkName" Data..=) Prelude.<$> networkName,
            ("port" Data..=) Prelude.<$> port,
            ("protocol" Data..=) Prelude.<$> protocol,
            ("ttl" Data..=) Prelude.<$> ttl
          ]
      )
