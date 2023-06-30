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
-- Module      : Amazonka.Discovery.Types.NeighborConnectionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.NeighborConnectionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about neighboring servers.
--
-- /See:/ 'newNeighborConnectionDetail' smart constructor.
data NeighborConnectionDetail = NeighborConnectionDetail'
  { -- | The destination network port for the connection.
    destinationPort :: Prelude.Maybe Prelude.Int,
    -- | The network protocol used for the connection.
    transportProtocol :: Prelude.Maybe Prelude.Text,
    -- | The ID of the server that opened the network connection.
    sourceServerId :: Prelude.Text,
    -- | The ID of the server that accepted the network connection.
    destinationServerId :: Prelude.Text,
    -- | The number of open network connections with the neighboring server.
    connectionsCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NeighborConnectionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPort', 'neighborConnectionDetail_destinationPort' - The destination network port for the connection.
--
-- 'transportProtocol', 'neighborConnectionDetail_transportProtocol' - The network protocol used for the connection.
--
-- 'sourceServerId', 'neighborConnectionDetail_sourceServerId' - The ID of the server that opened the network connection.
--
-- 'destinationServerId', 'neighborConnectionDetail_destinationServerId' - The ID of the server that accepted the network connection.
--
-- 'connectionsCount', 'neighborConnectionDetail_connectionsCount' - The number of open network connections with the neighboring server.
newNeighborConnectionDetail ::
  -- | 'sourceServerId'
  Prelude.Text ->
  -- | 'destinationServerId'
  Prelude.Text ->
  -- | 'connectionsCount'
  Prelude.Integer ->
  NeighborConnectionDetail
newNeighborConnectionDetail
  pSourceServerId_
  pDestinationServerId_
  pConnectionsCount_ =
    NeighborConnectionDetail'
      { destinationPort =
          Prelude.Nothing,
        transportProtocol = Prelude.Nothing,
        sourceServerId = pSourceServerId_,
        destinationServerId = pDestinationServerId_,
        connectionsCount = pConnectionsCount_
      }

-- | The destination network port for the connection.
neighborConnectionDetail_destinationPort :: Lens.Lens' NeighborConnectionDetail (Prelude.Maybe Prelude.Int)
neighborConnectionDetail_destinationPort = Lens.lens (\NeighborConnectionDetail' {destinationPort} -> destinationPort) (\s@NeighborConnectionDetail' {} a -> s {destinationPort = a} :: NeighborConnectionDetail)

-- | The network protocol used for the connection.
neighborConnectionDetail_transportProtocol :: Lens.Lens' NeighborConnectionDetail (Prelude.Maybe Prelude.Text)
neighborConnectionDetail_transportProtocol = Lens.lens (\NeighborConnectionDetail' {transportProtocol} -> transportProtocol) (\s@NeighborConnectionDetail' {} a -> s {transportProtocol = a} :: NeighborConnectionDetail)

-- | The ID of the server that opened the network connection.
neighborConnectionDetail_sourceServerId :: Lens.Lens' NeighborConnectionDetail Prelude.Text
neighborConnectionDetail_sourceServerId = Lens.lens (\NeighborConnectionDetail' {sourceServerId} -> sourceServerId) (\s@NeighborConnectionDetail' {} a -> s {sourceServerId = a} :: NeighborConnectionDetail)

-- | The ID of the server that accepted the network connection.
neighborConnectionDetail_destinationServerId :: Lens.Lens' NeighborConnectionDetail Prelude.Text
neighborConnectionDetail_destinationServerId = Lens.lens (\NeighborConnectionDetail' {destinationServerId} -> destinationServerId) (\s@NeighborConnectionDetail' {} a -> s {destinationServerId = a} :: NeighborConnectionDetail)

-- | The number of open network connections with the neighboring server.
neighborConnectionDetail_connectionsCount :: Lens.Lens' NeighborConnectionDetail Prelude.Integer
neighborConnectionDetail_connectionsCount = Lens.lens (\NeighborConnectionDetail' {connectionsCount} -> connectionsCount) (\s@NeighborConnectionDetail' {} a -> s {connectionsCount = a} :: NeighborConnectionDetail)

instance Data.FromJSON NeighborConnectionDetail where
  parseJSON =
    Data.withObject
      "NeighborConnectionDetail"
      ( \x ->
          NeighborConnectionDetail'
            Prelude.<$> (x Data..:? "destinationPort")
            Prelude.<*> (x Data..:? "transportProtocol")
            Prelude.<*> (x Data..: "sourceServerId")
            Prelude.<*> (x Data..: "destinationServerId")
            Prelude.<*> (x Data..: "connectionsCount")
      )

instance Prelude.Hashable NeighborConnectionDetail where
  hashWithSalt _salt NeighborConnectionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` destinationPort
      `Prelude.hashWithSalt` transportProtocol
      `Prelude.hashWithSalt` sourceServerId
      `Prelude.hashWithSalt` destinationServerId
      `Prelude.hashWithSalt` connectionsCount

instance Prelude.NFData NeighborConnectionDetail where
  rnf NeighborConnectionDetail' {..} =
    Prelude.rnf destinationPort
      `Prelude.seq` Prelude.rnf transportProtocol
      `Prelude.seq` Prelude.rnf sourceServerId
      `Prelude.seq` Prelude.rnf destinationServerId
      `Prelude.seq` Prelude.rnf connectionsCount
