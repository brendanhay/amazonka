{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the specified connection or all connections in this Region.
module Network.AWS.DirectConnect.DescribeConnections
  ( -- * Creating a request
    DescribeConnections (..),
    mkDescribeConnections,

    -- ** Request lenses
    dConnectionId,

    -- * Destructuring the response
    Types.Connections (..),
    Types.mkConnections,

    -- ** Response lenses
    Types.cConnections,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConnections' smart constructor.
newtype DescribeConnections = DescribeConnections'
  { -- | The ID of the connection.
    connectionId :: Core.Maybe Types.ConnectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConnections' value with any optional fields omitted.
mkDescribeConnections ::
  DescribeConnections
mkDescribeConnections =
  DescribeConnections' {connectionId = Core.Nothing}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConnectionId :: Lens.Lens' DescribeConnections (Core.Maybe Types.ConnectionId)
dConnectionId = Lens.field @"connectionId"
{-# DEPRECATED dConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Core.FromJSON DescribeConnections where
  toJSON DescribeConnections {..} =
    Core.object
      (Core.catMaybes [("connectionId" Core..=) Core.<$> connectionId])

instance Core.AWSRequest DescribeConnections where
  type Rs DescribeConnections = Types.Connections
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.DescribeConnections")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
