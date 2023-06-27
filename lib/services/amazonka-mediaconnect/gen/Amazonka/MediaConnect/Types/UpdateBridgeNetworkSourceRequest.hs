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
-- Module      : Amazonka.MediaConnect.Types.UpdateBridgeNetworkSourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateBridgeNetworkSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Update the network source of the bridge.
--
-- /See:/ 'newUpdateBridgeNetworkSourceRequest' smart constructor.
data UpdateBridgeNetworkSourceRequest = UpdateBridgeNetworkSourceRequest'
  { -- | The network source multicast IP.
    multicastIp :: Prelude.Maybe Prelude.Text,
    -- | The network source\'s gateway network name.
    networkName :: Prelude.Maybe Prelude.Text,
    -- | The network source port.
    port :: Prelude.Maybe Prelude.Int,
    -- | The network source protocol.
    protocol :: Prelude.Maybe Protocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeNetworkSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multicastIp', 'updateBridgeNetworkSourceRequest_multicastIp' - The network source multicast IP.
--
-- 'networkName', 'updateBridgeNetworkSourceRequest_networkName' - The network source\'s gateway network name.
--
-- 'port', 'updateBridgeNetworkSourceRequest_port' - The network source port.
--
-- 'protocol', 'updateBridgeNetworkSourceRequest_protocol' - The network source protocol.
newUpdateBridgeNetworkSourceRequest ::
  UpdateBridgeNetworkSourceRequest
newUpdateBridgeNetworkSourceRequest =
  UpdateBridgeNetworkSourceRequest'
    { multicastIp =
        Prelude.Nothing,
      networkName = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The network source multicast IP.
updateBridgeNetworkSourceRequest_multicastIp :: Lens.Lens' UpdateBridgeNetworkSourceRequest (Prelude.Maybe Prelude.Text)
updateBridgeNetworkSourceRequest_multicastIp = Lens.lens (\UpdateBridgeNetworkSourceRequest' {multicastIp} -> multicastIp) (\s@UpdateBridgeNetworkSourceRequest' {} a -> s {multicastIp = a} :: UpdateBridgeNetworkSourceRequest)

-- | The network source\'s gateway network name.
updateBridgeNetworkSourceRequest_networkName :: Lens.Lens' UpdateBridgeNetworkSourceRequest (Prelude.Maybe Prelude.Text)
updateBridgeNetworkSourceRequest_networkName = Lens.lens (\UpdateBridgeNetworkSourceRequest' {networkName} -> networkName) (\s@UpdateBridgeNetworkSourceRequest' {} a -> s {networkName = a} :: UpdateBridgeNetworkSourceRequest)

-- | The network source port.
updateBridgeNetworkSourceRequest_port :: Lens.Lens' UpdateBridgeNetworkSourceRequest (Prelude.Maybe Prelude.Int)
updateBridgeNetworkSourceRequest_port = Lens.lens (\UpdateBridgeNetworkSourceRequest' {port} -> port) (\s@UpdateBridgeNetworkSourceRequest' {} a -> s {port = a} :: UpdateBridgeNetworkSourceRequest)

-- | The network source protocol.
updateBridgeNetworkSourceRequest_protocol :: Lens.Lens' UpdateBridgeNetworkSourceRequest (Prelude.Maybe Protocol)
updateBridgeNetworkSourceRequest_protocol = Lens.lens (\UpdateBridgeNetworkSourceRequest' {protocol} -> protocol) (\s@UpdateBridgeNetworkSourceRequest' {} a -> s {protocol = a} :: UpdateBridgeNetworkSourceRequest)

instance
  Prelude.Hashable
    UpdateBridgeNetworkSourceRequest
  where
  hashWithSalt
    _salt
    UpdateBridgeNetworkSourceRequest' {..} =
      _salt
        `Prelude.hashWithSalt` multicastIp
        `Prelude.hashWithSalt` networkName
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    UpdateBridgeNetworkSourceRequest
  where
  rnf UpdateBridgeNetworkSourceRequest' {..} =
    Prelude.rnf multicastIp
      `Prelude.seq` Prelude.rnf networkName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol

instance Data.ToJSON UpdateBridgeNetworkSourceRequest where
  toJSON UpdateBridgeNetworkSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("multicastIp" Data..=) Prelude.<$> multicastIp,
            ("networkName" Data..=) Prelude.<$> networkName,
            ("port" Data..=) Prelude.<$> port,
            ("protocol" Data..=) Prelude.<$> protocol
          ]
      )
