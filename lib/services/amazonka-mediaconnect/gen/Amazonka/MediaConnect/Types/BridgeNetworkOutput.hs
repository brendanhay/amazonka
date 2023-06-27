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
-- Module      : Amazonka.MediaConnect.Types.BridgeNetworkOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.BridgeNetworkOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | The output of the bridge. A network output is delivered to your
-- premises.
--
-- /See:/ 'newBridgeNetworkOutput' smart constructor.
data BridgeNetworkOutput = BridgeNetworkOutput'
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
    -- | The network output name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BridgeNetworkOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkName', 'bridgeNetworkOutput_networkName' - The network output\'s gateway network name.
--
-- 'port', 'bridgeNetworkOutput_port' - The network output port.
--
-- 'ipAddress', 'bridgeNetworkOutput_ipAddress' - The network output IP Address.
--
-- 'protocol', 'bridgeNetworkOutput_protocol' - The network output protocol.
--
-- 'ttl', 'bridgeNetworkOutput_ttl' - The network output TTL.
--
-- 'name', 'bridgeNetworkOutput_name' - The network output name.
newBridgeNetworkOutput ::
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
  BridgeNetworkOutput
newBridgeNetworkOutput
  pNetworkName_
  pPort_
  pIpAddress_
  pProtocol_
  pTtl_
  pName_ =
    BridgeNetworkOutput'
      { networkName = pNetworkName_,
        port = pPort_,
        ipAddress = pIpAddress_,
        protocol = pProtocol_,
        ttl = pTtl_,
        name = pName_
      }

-- | The network output\'s gateway network name.
bridgeNetworkOutput_networkName :: Lens.Lens' BridgeNetworkOutput Prelude.Text
bridgeNetworkOutput_networkName = Lens.lens (\BridgeNetworkOutput' {networkName} -> networkName) (\s@BridgeNetworkOutput' {} a -> s {networkName = a} :: BridgeNetworkOutput)

-- | The network output port.
bridgeNetworkOutput_port :: Lens.Lens' BridgeNetworkOutput Prelude.Int
bridgeNetworkOutput_port = Lens.lens (\BridgeNetworkOutput' {port} -> port) (\s@BridgeNetworkOutput' {} a -> s {port = a} :: BridgeNetworkOutput)

-- | The network output IP Address.
bridgeNetworkOutput_ipAddress :: Lens.Lens' BridgeNetworkOutput Prelude.Text
bridgeNetworkOutput_ipAddress = Lens.lens (\BridgeNetworkOutput' {ipAddress} -> ipAddress) (\s@BridgeNetworkOutput' {} a -> s {ipAddress = a} :: BridgeNetworkOutput)

-- | The network output protocol.
bridgeNetworkOutput_protocol :: Lens.Lens' BridgeNetworkOutput Protocol
bridgeNetworkOutput_protocol = Lens.lens (\BridgeNetworkOutput' {protocol} -> protocol) (\s@BridgeNetworkOutput' {} a -> s {protocol = a} :: BridgeNetworkOutput)

-- | The network output TTL.
bridgeNetworkOutput_ttl :: Lens.Lens' BridgeNetworkOutput Prelude.Int
bridgeNetworkOutput_ttl = Lens.lens (\BridgeNetworkOutput' {ttl} -> ttl) (\s@BridgeNetworkOutput' {} a -> s {ttl = a} :: BridgeNetworkOutput)

-- | The network output name.
bridgeNetworkOutput_name :: Lens.Lens' BridgeNetworkOutput Prelude.Text
bridgeNetworkOutput_name = Lens.lens (\BridgeNetworkOutput' {name} -> name) (\s@BridgeNetworkOutput' {} a -> s {name = a} :: BridgeNetworkOutput)

instance Data.FromJSON BridgeNetworkOutput where
  parseJSON =
    Data.withObject
      "BridgeNetworkOutput"
      ( \x ->
          BridgeNetworkOutput'
            Prelude.<$> (x Data..: "networkName")
            Prelude.<*> (x Data..: "port")
            Prelude.<*> (x Data..: "ipAddress")
            Prelude.<*> (x Data..: "protocol")
            Prelude.<*> (x Data..: "ttl")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable BridgeNetworkOutput where
  hashWithSalt _salt BridgeNetworkOutput' {..} =
    _salt
      `Prelude.hashWithSalt` networkName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` name

instance Prelude.NFData BridgeNetworkOutput where
  rnf BridgeNetworkOutput' {..} =
    Prelude.rnf networkName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf name
