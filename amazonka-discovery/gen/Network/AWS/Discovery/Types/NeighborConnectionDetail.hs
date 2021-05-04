{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Discovery.Types.NeighborConnectionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.NeighborConnectionDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about neighboring servers.
--
-- /See:/ 'newNeighborConnectionDetail' smart constructor.
data NeighborConnectionDetail = NeighborConnectionDetail'
  { -- | The network protocol used for the connection.
    transportProtocol :: Prelude.Maybe Prelude.Text,
    -- | The destination network port for the connection.
    destinationPort :: Prelude.Maybe Prelude.Int,
    -- | The ID of the server that opened the network connection.
    sourceServerId :: Prelude.Text,
    -- | The ID of the server that accepted the network connection.
    destinationServerId :: Prelude.Text,
    -- | The number of open network connections with the neighboring server.
    connectionsCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NeighborConnectionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transportProtocol', 'neighborConnectionDetail_transportProtocol' - The network protocol used for the connection.
--
-- 'destinationPort', 'neighborConnectionDetail_destinationPort' - The destination network port for the connection.
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
      { transportProtocol =
          Prelude.Nothing,
        destinationPort = Prelude.Nothing,
        sourceServerId = pSourceServerId_,
        destinationServerId = pDestinationServerId_,
        connectionsCount = pConnectionsCount_
      }

-- | The network protocol used for the connection.
neighborConnectionDetail_transportProtocol :: Lens.Lens' NeighborConnectionDetail (Prelude.Maybe Prelude.Text)
neighborConnectionDetail_transportProtocol = Lens.lens (\NeighborConnectionDetail' {transportProtocol} -> transportProtocol) (\s@NeighborConnectionDetail' {} a -> s {transportProtocol = a} :: NeighborConnectionDetail)

-- | The destination network port for the connection.
neighborConnectionDetail_destinationPort :: Lens.Lens' NeighborConnectionDetail (Prelude.Maybe Prelude.Int)
neighborConnectionDetail_destinationPort = Lens.lens (\NeighborConnectionDetail' {destinationPort} -> destinationPort) (\s@NeighborConnectionDetail' {} a -> s {destinationPort = a} :: NeighborConnectionDetail)

-- | The ID of the server that opened the network connection.
neighborConnectionDetail_sourceServerId :: Lens.Lens' NeighborConnectionDetail Prelude.Text
neighborConnectionDetail_sourceServerId = Lens.lens (\NeighborConnectionDetail' {sourceServerId} -> sourceServerId) (\s@NeighborConnectionDetail' {} a -> s {sourceServerId = a} :: NeighborConnectionDetail)

-- | The ID of the server that accepted the network connection.
neighborConnectionDetail_destinationServerId :: Lens.Lens' NeighborConnectionDetail Prelude.Text
neighborConnectionDetail_destinationServerId = Lens.lens (\NeighborConnectionDetail' {destinationServerId} -> destinationServerId) (\s@NeighborConnectionDetail' {} a -> s {destinationServerId = a} :: NeighborConnectionDetail)

-- | The number of open network connections with the neighboring server.
neighborConnectionDetail_connectionsCount :: Lens.Lens' NeighborConnectionDetail Prelude.Integer
neighborConnectionDetail_connectionsCount = Lens.lens (\NeighborConnectionDetail' {connectionsCount} -> connectionsCount) (\s@NeighborConnectionDetail' {} a -> s {connectionsCount = a} :: NeighborConnectionDetail)

instance Prelude.FromJSON NeighborConnectionDetail where
  parseJSON =
    Prelude.withObject
      "NeighborConnectionDetail"
      ( \x ->
          NeighborConnectionDetail'
            Prelude.<$> (x Prelude..:? "transportProtocol")
            Prelude.<*> (x Prelude..:? "destinationPort")
            Prelude.<*> (x Prelude..: "sourceServerId")
            Prelude.<*> (x Prelude..: "destinationServerId")
            Prelude.<*> (x Prelude..: "connectionsCount")
      )

instance Prelude.Hashable NeighborConnectionDetail

instance Prelude.NFData NeighborConnectionDetail
