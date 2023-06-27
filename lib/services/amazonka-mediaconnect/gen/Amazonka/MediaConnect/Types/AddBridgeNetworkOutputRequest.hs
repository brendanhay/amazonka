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
-- Module      : Amazonka.MediaConnect.Types.AddBridgeNetworkOutputRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddBridgeNetworkOutputRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Add a network output to an existing bridge.
--
-- /See:/ 'newAddBridgeNetworkOutputRequest' smart constructor.
data AddBridgeNetworkOutputRequest = AddBridgeNetworkOutputRequest'
  { -- | The network output\'s gateway network name.
    networkName :: Prelude.Text,
    -- | The network output port.
    port :: Prelude.Int,
    -- | The network output IP Address.
    ipAddress :: Prelude.Text,
    -- | The network output protocol.
    protocol :: Protocol,
    -- | The network output TTL.
    ttl :: Prelude.Int,
    -- | The network output name. This name is used to reference the output and
    -- must be unique among outputs in this bridge.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeNetworkOutputRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkName', 'addBridgeNetworkOutputRequest_networkName' - The network output\'s gateway network name.
--
-- 'port', 'addBridgeNetworkOutputRequest_port' - The network output port.
--
-- 'ipAddress', 'addBridgeNetworkOutputRequest_ipAddress' - The network output IP Address.
--
-- 'protocol', 'addBridgeNetworkOutputRequest_protocol' - The network output protocol.
--
-- 'ttl', 'addBridgeNetworkOutputRequest_ttl' - The network output TTL.
--
-- 'name', 'addBridgeNetworkOutputRequest_name' - The network output name. This name is used to reference the output and
-- must be unique among outputs in this bridge.
newAddBridgeNetworkOutputRequest ::
  -- | 'networkName'
  Prelude.Text ->
  -- | 'port'
  Prelude.Int ->
  -- | 'ipAddress'
  Prelude.Text ->
  -- | 'protocol'
  Protocol ->
  -- | 'ttl'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  AddBridgeNetworkOutputRequest
newAddBridgeNetworkOutputRequest
  pNetworkName_
  pPort_
  pIpAddress_
  pProtocol_
  pTtl_
  pName_ =
    AddBridgeNetworkOutputRequest'
      { networkName =
          pNetworkName_,
        port = pPort_,
        ipAddress = pIpAddress_,
        protocol = pProtocol_,
        ttl = pTtl_,
        name = pName_
      }

-- | The network output\'s gateway network name.
addBridgeNetworkOutputRequest_networkName :: Lens.Lens' AddBridgeNetworkOutputRequest Prelude.Text
addBridgeNetworkOutputRequest_networkName = Lens.lens (\AddBridgeNetworkOutputRequest' {networkName} -> networkName) (\s@AddBridgeNetworkOutputRequest' {} a -> s {networkName = a} :: AddBridgeNetworkOutputRequest)

-- | The network output port.
addBridgeNetworkOutputRequest_port :: Lens.Lens' AddBridgeNetworkOutputRequest Prelude.Int
addBridgeNetworkOutputRequest_port = Lens.lens (\AddBridgeNetworkOutputRequest' {port} -> port) (\s@AddBridgeNetworkOutputRequest' {} a -> s {port = a} :: AddBridgeNetworkOutputRequest)

-- | The network output IP Address.
addBridgeNetworkOutputRequest_ipAddress :: Lens.Lens' AddBridgeNetworkOutputRequest Prelude.Text
addBridgeNetworkOutputRequest_ipAddress = Lens.lens (\AddBridgeNetworkOutputRequest' {ipAddress} -> ipAddress) (\s@AddBridgeNetworkOutputRequest' {} a -> s {ipAddress = a} :: AddBridgeNetworkOutputRequest)

-- | The network output protocol.
addBridgeNetworkOutputRequest_protocol :: Lens.Lens' AddBridgeNetworkOutputRequest Protocol
addBridgeNetworkOutputRequest_protocol = Lens.lens (\AddBridgeNetworkOutputRequest' {protocol} -> protocol) (\s@AddBridgeNetworkOutputRequest' {} a -> s {protocol = a} :: AddBridgeNetworkOutputRequest)

-- | The network output TTL.
addBridgeNetworkOutputRequest_ttl :: Lens.Lens' AddBridgeNetworkOutputRequest Prelude.Int
addBridgeNetworkOutputRequest_ttl = Lens.lens (\AddBridgeNetworkOutputRequest' {ttl} -> ttl) (\s@AddBridgeNetworkOutputRequest' {} a -> s {ttl = a} :: AddBridgeNetworkOutputRequest)

-- | The network output name. This name is used to reference the output and
-- must be unique among outputs in this bridge.
addBridgeNetworkOutputRequest_name :: Lens.Lens' AddBridgeNetworkOutputRequest Prelude.Text
addBridgeNetworkOutputRequest_name = Lens.lens (\AddBridgeNetworkOutputRequest' {name} -> name) (\s@AddBridgeNetworkOutputRequest' {} a -> s {name = a} :: AddBridgeNetworkOutputRequest)

instance
  Prelude.Hashable
    AddBridgeNetworkOutputRequest
  where
  hashWithSalt _salt AddBridgeNetworkOutputRequest' {..} =
    _salt
      `Prelude.hashWithSalt` networkName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` name

instance Prelude.NFData AddBridgeNetworkOutputRequest where
  rnf AddBridgeNetworkOutputRequest' {..} =
    Prelude.rnf networkName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AddBridgeNetworkOutputRequest where
  toJSON AddBridgeNetworkOutputRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("networkName" Data..= networkName),
            Prelude.Just ("port" Data..= port),
            Prelude.Just ("ipAddress" Data..= ipAddress),
            Prelude.Just ("protocol" Data..= protocol),
            Prelude.Just ("ttl" Data..= ttl),
            Prelude.Just ("name" Data..= name)
          ]
      )
