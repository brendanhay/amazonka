-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.NeighborConnectionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.NeighborConnectionDetail
  ( NeighborConnectionDetail (..),

    -- * Smart constructor
    mkNeighborConnectionDetail,

    -- * Lenses
    ncdTransportProtocol,
    ncdDestinationPort,
    ncdSourceServerId,
    ncdDestinationServerId,
    ncdConnectionsCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about neighboring servers.
--
-- /See:/ 'mkNeighborConnectionDetail' smart constructor.
data NeighborConnectionDetail = NeighborConnectionDetail'
  { transportProtocol ::
      Lude.Maybe Lude.Text,
    destinationPort :: Lude.Maybe Lude.Int,
    sourceServerId :: Lude.Text,
    destinationServerId :: Lude.Text,
    connectionsCount :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NeighborConnectionDetail' with the minimum fields required to make a request.
--
-- * 'connectionsCount' - The number of open network connections with the neighboring server.
-- * 'destinationPort' - The destination network port for the connection.
-- * 'destinationServerId' - The ID of the server that accepted the network connection.
-- * 'sourceServerId' - The ID of the server that opened the network connection.
-- * 'transportProtocol' - The network protocol used for the connection.
mkNeighborConnectionDetail ::
  -- | 'sourceServerId'
  Lude.Text ->
  -- | 'destinationServerId'
  Lude.Text ->
  -- | 'connectionsCount'
  Lude.Integer ->
  NeighborConnectionDetail
mkNeighborConnectionDetail
  pSourceServerId_
  pDestinationServerId_
  pConnectionsCount_ =
    NeighborConnectionDetail'
      { transportProtocol = Lude.Nothing,
        destinationPort = Lude.Nothing,
        sourceServerId = pSourceServerId_,
        destinationServerId = pDestinationServerId_,
        connectionsCount = pConnectionsCount_
      }

-- | The network protocol used for the connection.
--
-- /Note:/ Consider using 'transportProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdTransportProtocol :: Lens.Lens' NeighborConnectionDetail (Lude.Maybe Lude.Text)
ncdTransportProtocol = Lens.lens (transportProtocol :: NeighborConnectionDetail -> Lude.Maybe Lude.Text) (\s a -> s {transportProtocol = a} :: NeighborConnectionDetail)
{-# DEPRECATED ncdTransportProtocol "Use generic-lens or generic-optics with 'transportProtocol' instead." #-}

-- | The destination network port for the connection.
--
-- /Note:/ Consider using 'destinationPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdDestinationPort :: Lens.Lens' NeighborConnectionDetail (Lude.Maybe Lude.Int)
ncdDestinationPort = Lens.lens (destinationPort :: NeighborConnectionDetail -> Lude.Maybe Lude.Int) (\s a -> s {destinationPort = a} :: NeighborConnectionDetail)
{-# DEPRECATED ncdDestinationPort "Use generic-lens or generic-optics with 'destinationPort' instead." #-}

-- | The ID of the server that opened the network connection.
--
-- /Note:/ Consider using 'sourceServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdSourceServerId :: Lens.Lens' NeighborConnectionDetail Lude.Text
ncdSourceServerId = Lens.lens (sourceServerId :: NeighborConnectionDetail -> Lude.Text) (\s a -> s {sourceServerId = a} :: NeighborConnectionDetail)
{-# DEPRECATED ncdSourceServerId "Use generic-lens or generic-optics with 'sourceServerId' instead." #-}

-- | The ID of the server that accepted the network connection.
--
-- /Note:/ Consider using 'destinationServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdDestinationServerId :: Lens.Lens' NeighborConnectionDetail Lude.Text
ncdDestinationServerId = Lens.lens (destinationServerId :: NeighborConnectionDetail -> Lude.Text) (\s a -> s {destinationServerId = a} :: NeighborConnectionDetail)
{-# DEPRECATED ncdDestinationServerId "Use generic-lens or generic-optics with 'destinationServerId' instead." #-}

-- | The number of open network connections with the neighboring server.
--
-- /Note:/ Consider using 'connectionsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdConnectionsCount :: Lens.Lens' NeighborConnectionDetail Lude.Integer
ncdConnectionsCount = Lens.lens (connectionsCount :: NeighborConnectionDetail -> Lude.Integer) (\s a -> s {connectionsCount = a} :: NeighborConnectionDetail)
{-# DEPRECATED ncdConnectionsCount "Use generic-lens or generic-optics with 'connectionsCount' instead." #-}

instance Lude.FromJSON NeighborConnectionDetail where
  parseJSON =
    Lude.withObject
      "NeighborConnectionDetail"
      ( \x ->
          NeighborConnectionDetail'
            Lude.<$> (x Lude..:? "transportProtocol")
            Lude.<*> (x Lude..:? "destinationPort")
            Lude.<*> (x Lude..: "sourceServerId")
            Lude.<*> (x Lude..: "destinationServerId")
            Lude.<*> (x Lude..: "connectionsCount")
      )
