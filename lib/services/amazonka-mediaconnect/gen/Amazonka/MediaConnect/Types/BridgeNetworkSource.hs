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
-- Module      : Amazonka.MediaConnect.Types.BridgeNetworkSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.BridgeNetworkSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | The source of the bridge. A network source originates at your premises.
--
-- /See:/ 'newBridgeNetworkSource' smart constructor.
data BridgeNetworkSource = BridgeNetworkSource'
  { -- | The network source\'s gateway network name.
    networkName :: Prelude.Text,
    -- | The network source multicast IP.
    multicastIp :: Prelude.Text,
    -- | The network source port.
    port :: Prelude.Int,
    -- | The network source protocol.
    protocol :: Protocol,
    -- | The name of the network source.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BridgeNetworkSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkName', 'bridgeNetworkSource_networkName' - The network source\'s gateway network name.
--
-- 'multicastIp', 'bridgeNetworkSource_multicastIp' - The network source multicast IP.
--
-- 'port', 'bridgeNetworkSource_port' - The network source port.
--
-- 'protocol', 'bridgeNetworkSource_protocol' - The network source protocol.
--
-- 'name', 'bridgeNetworkSource_name' - The name of the network source.
newBridgeNetworkSource ::
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
  BridgeNetworkSource
newBridgeNetworkSource
  pNetworkName_
  pMulticastIp_
  pPort_
  pProtocol_
  pName_ =
    BridgeNetworkSource'
      { networkName = pNetworkName_,
        multicastIp = pMulticastIp_,
        port = pPort_,
        protocol = pProtocol_,
        name = pName_
      }

-- | The network source\'s gateway network name.
bridgeNetworkSource_networkName :: Lens.Lens' BridgeNetworkSource Prelude.Text
bridgeNetworkSource_networkName = Lens.lens (\BridgeNetworkSource' {networkName} -> networkName) (\s@BridgeNetworkSource' {} a -> s {networkName = a} :: BridgeNetworkSource)

-- | The network source multicast IP.
bridgeNetworkSource_multicastIp :: Lens.Lens' BridgeNetworkSource Prelude.Text
bridgeNetworkSource_multicastIp = Lens.lens (\BridgeNetworkSource' {multicastIp} -> multicastIp) (\s@BridgeNetworkSource' {} a -> s {multicastIp = a} :: BridgeNetworkSource)

-- | The network source port.
bridgeNetworkSource_port :: Lens.Lens' BridgeNetworkSource Prelude.Int
bridgeNetworkSource_port = Lens.lens (\BridgeNetworkSource' {port} -> port) (\s@BridgeNetworkSource' {} a -> s {port = a} :: BridgeNetworkSource)

-- | The network source protocol.
bridgeNetworkSource_protocol :: Lens.Lens' BridgeNetworkSource Protocol
bridgeNetworkSource_protocol = Lens.lens (\BridgeNetworkSource' {protocol} -> protocol) (\s@BridgeNetworkSource' {} a -> s {protocol = a} :: BridgeNetworkSource)

-- | The name of the network source.
bridgeNetworkSource_name :: Lens.Lens' BridgeNetworkSource Prelude.Text
bridgeNetworkSource_name = Lens.lens (\BridgeNetworkSource' {name} -> name) (\s@BridgeNetworkSource' {} a -> s {name = a} :: BridgeNetworkSource)

instance Data.FromJSON BridgeNetworkSource where
  parseJSON =
    Data.withObject
      "BridgeNetworkSource"
      ( \x ->
          BridgeNetworkSource'
            Prelude.<$> (x Data..: "networkName")
            Prelude.<*> (x Data..: "multicastIp")
            Prelude.<*> (x Data..: "port")
            Prelude.<*> (x Data..: "protocol")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable BridgeNetworkSource where
  hashWithSalt _salt BridgeNetworkSource' {..} =
    _salt
      `Prelude.hashWithSalt` networkName
      `Prelude.hashWithSalt` multicastIp
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` name

instance Prelude.NFData BridgeNetworkSource where
  rnf BridgeNetworkSource' {..} =
    Prelude.rnf networkName
      `Prelude.seq` Prelude.rnf multicastIp
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf name
