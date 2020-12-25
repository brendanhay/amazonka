{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ncdSourceServerId,
    ncdDestinationServerId,
    ncdConnectionsCount,
    ncdDestinationPort,
    ncdTransportProtocol,
  )
where

import qualified Network.AWS.Discovery.Types.ConfigurationId as Types
import qualified Network.AWS.Discovery.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about neighboring servers.
--
-- /See:/ 'mkNeighborConnectionDetail' smart constructor.
data NeighborConnectionDetail = NeighborConnectionDetail'
  { -- | The ID of the server that opened the network connection.
    sourceServerId :: Types.ConfigurationId,
    -- | The ID of the server that accepted the network connection.
    destinationServerId :: Types.ConfigurationId,
    -- | The number of open network connections with the neighboring server.
    connectionsCount :: Core.Integer,
    -- | The destination network port for the connection.
    destinationPort :: Core.Maybe Core.Int,
    -- | The network protocol used for the connection.
    transportProtocol :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NeighborConnectionDetail' value with any optional fields omitted.
mkNeighborConnectionDetail ::
  -- | 'sourceServerId'
  Types.ConfigurationId ->
  -- | 'destinationServerId'
  Types.ConfigurationId ->
  -- | 'connectionsCount'
  Core.Integer ->
  NeighborConnectionDetail
mkNeighborConnectionDetail
  sourceServerId
  destinationServerId
  connectionsCount =
    NeighborConnectionDetail'
      { sourceServerId,
        destinationServerId,
        connectionsCount,
        destinationPort = Core.Nothing,
        transportProtocol = Core.Nothing
      }

-- | The ID of the server that opened the network connection.
--
-- /Note:/ Consider using 'sourceServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdSourceServerId :: Lens.Lens' NeighborConnectionDetail Types.ConfigurationId
ncdSourceServerId = Lens.field @"sourceServerId"
{-# DEPRECATED ncdSourceServerId "Use generic-lens or generic-optics with 'sourceServerId' instead." #-}

-- | The ID of the server that accepted the network connection.
--
-- /Note:/ Consider using 'destinationServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdDestinationServerId :: Lens.Lens' NeighborConnectionDetail Types.ConfigurationId
ncdDestinationServerId = Lens.field @"destinationServerId"
{-# DEPRECATED ncdDestinationServerId "Use generic-lens or generic-optics with 'destinationServerId' instead." #-}

-- | The number of open network connections with the neighboring server.
--
-- /Note:/ Consider using 'connectionsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdConnectionsCount :: Lens.Lens' NeighborConnectionDetail Core.Integer
ncdConnectionsCount = Lens.field @"connectionsCount"
{-# DEPRECATED ncdConnectionsCount "Use generic-lens or generic-optics with 'connectionsCount' instead." #-}

-- | The destination network port for the connection.
--
-- /Note:/ Consider using 'destinationPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdDestinationPort :: Lens.Lens' NeighborConnectionDetail (Core.Maybe Core.Int)
ncdDestinationPort = Lens.field @"destinationPort"
{-# DEPRECATED ncdDestinationPort "Use generic-lens or generic-optics with 'destinationPort' instead." #-}

-- | The network protocol used for the connection.
--
-- /Note:/ Consider using 'transportProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncdTransportProtocol :: Lens.Lens' NeighborConnectionDetail (Core.Maybe Types.String)
ncdTransportProtocol = Lens.field @"transportProtocol"
{-# DEPRECATED ncdTransportProtocol "Use generic-lens or generic-optics with 'transportProtocol' instead." #-}

instance Core.FromJSON NeighborConnectionDetail where
  parseJSON =
    Core.withObject "NeighborConnectionDetail" Core.$
      \x ->
        NeighborConnectionDetail'
          Core.<$> (x Core..: "sourceServerId")
          Core.<*> (x Core..: "destinationServerId")
          Core.<*> (x Core..: "connectionsCount")
          Core.<*> (x Core..:? "destinationPort")
          Core.<*> (x Core..:? "transportProtocol")
