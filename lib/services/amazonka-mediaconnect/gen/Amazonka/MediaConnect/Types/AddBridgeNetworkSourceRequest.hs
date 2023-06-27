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
-- Module      : Amazonka.MediaConnect.Types.AddBridgeNetworkSourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddBridgeNetworkSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Add a network source to an existing bridge.
--
-- /See:/ 'newAddBridgeNetworkSourceRequest' smart constructor.
data AddBridgeNetworkSourceRequest = AddBridgeNetworkSourceRequest'
  { -- | The network source\'s gateway network name.
    networkName :: Prelude.Text,
    -- | The network source multicast IP.
    multicastIp :: Prelude.Text,
    -- | The network source port.
    port :: Prelude.Int,
    -- | The network source protocol.
    protocol :: Protocol,
    -- | The name of the network source. This name is used to reference the
    -- source and must be unique among sources in this bridge.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeNetworkSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkName', 'addBridgeNetworkSourceRequest_networkName' - The network source\'s gateway network name.
--
-- 'multicastIp', 'addBridgeNetworkSourceRequest_multicastIp' - The network source multicast IP.
--
-- 'port', 'addBridgeNetworkSourceRequest_port' - The network source port.
--
-- 'protocol', 'addBridgeNetworkSourceRequest_protocol' - The network source protocol.
--
-- 'name', 'addBridgeNetworkSourceRequest_name' - The name of the network source. This name is used to reference the
-- source and must be unique among sources in this bridge.
newAddBridgeNetworkSourceRequest ::
  -- | 'networkName'
  Prelude.Text ->
  -- | 'multicastIp'
  Prelude.Text ->
  -- | 'port'
  Prelude.Int ->
  -- | 'protocol'
  Protocol ->
  -- | 'name'
  Prelude.Text ->
  AddBridgeNetworkSourceRequest
newAddBridgeNetworkSourceRequest
  pNetworkName_
  pMulticastIp_
  pPort_
  pProtocol_
  pName_ =
    AddBridgeNetworkSourceRequest'
      { networkName =
          pNetworkName_,
        multicastIp = pMulticastIp_,
        port = pPort_,
        protocol = pProtocol_,
        name = pName_
      }

-- | The network source\'s gateway network name.
addBridgeNetworkSourceRequest_networkName :: Lens.Lens' AddBridgeNetworkSourceRequest Prelude.Text
addBridgeNetworkSourceRequest_networkName = Lens.lens (\AddBridgeNetworkSourceRequest' {networkName} -> networkName) (\s@AddBridgeNetworkSourceRequest' {} a -> s {networkName = a} :: AddBridgeNetworkSourceRequest)

-- | The network source multicast IP.
addBridgeNetworkSourceRequest_multicastIp :: Lens.Lens' AddBridgeNetworkSourceRequest Prelude.Text
addBridgeNetworkSourceRequest_multicastIp = Lens.lens (\AddBridgeNetworkSourceRequest' {multicastIp} -> multicastIp) (\s@AddBridgeNetworkSourceRequest' {} a -> s {multicastIp = a} :: AddBridgeNetworkSourceRequest)

-- | The network source port.
addBridgeNetworkSourceRequest_port :: Lens.Lens' AddBridgeNetworkSourceRequest Prelude.Int
addBridgeNetworkSourceRequest_port = Lens.lens (\AddBridgeNetworkSourceRequest' {port} -> port) (\s@AddBridgeNetworkSourceRequest' {} a -> s {port = a} :: AddBridgeNetworkSourceRequest)

-- | The network source protocol.
addBridgeNetworkSourceRequest_protocol :: Lens.Lens' AddBridgeNetworkSourceRequest Protocol
addBridgeNetworkSourceRequest_protocol = Lens.lens (\AddBridgeNetworkSourceRequest' {protocol} -> protocol) (\s@AddBridgeNetworkSourceRequest' {} a -> s {protocol = a} :: AddBridgeNetworkSourceRequest)

-- | The name of the network source. This name is used to reference the
-- source and must be unique among sources in this bridge.
addBridgeNetworkSourceRequest_name :: Lens.Lens' AddBridgeNetworkSourceRequest Prelude.Text
addBridgeNetworkSourceRequest_name = Lens.lens (\AddBridgeNetworkSourceRequest' {name} -> name) (\s@AddBridgeNetworkSourceRequest' {} a -> s {name = a} :: AddBridgeNetworkSourceRequest)

instance
  Prelude.Hashable
    AddBridgeNetworkSourceRequest
  where
  hashWithSalt _salt AddBridgeNetworkSourceRequest' {..} =
    _salt
      `Prelude.hashWithSalt` networkName
      `Prelude.hashWithSalt` multicastIp
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` name

instance Prelude.NFData AddBridgeNetworkSourceRequest where
  rnf AddBridgeNetworkSourceRequest' {..} =
    Prelude.rnf networkName
      `Prelude.seq` Prelude.rnf multicastIp
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AddBridgeNetworkSourceRequest where
  toJSON AddBridgeNetworkSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("networkName" Data..= networkName),
            Prelude.Just ("multicastIp" Data..= multicastIp),
            Prelude.Just ("port" Data..= port),
            Prelude.Just ("protocol" Data..= protocol),
            Prelude.Just ("name" Data..= name)
          ]
      )
