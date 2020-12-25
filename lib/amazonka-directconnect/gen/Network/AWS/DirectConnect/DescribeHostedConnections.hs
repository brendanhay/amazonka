{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeHostedConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hosted connections that have been provisioned on the specified interconnect or link aggregation group (LAG).
module Network.AWS.DirectConnect.DescribeHostedConnections
  ( -- * Creating a request
    DescribeHostedConnections (..),
    mkDescribeHostedConnections,

    -- ** Request lenses
    dhcConnectionId,

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

-- | /See:/ 'mkDescribeHostedConnections' smart constructor.
newtype DescribeHostedConnections = DescribeHostedConnections'
  { -- | The ID of the interconnect or LAG.
    connectionId :: Types.ConnectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHostedConnections' value with any optional fields omitted.
mkDescribeHostedConnections ::
  -- | 'connectionId'
  Types.ConnectionId ->
  DescribeHostedConnections
mkDescribeHostedConnections connectionId =
  DescribeHostedConnections' {connectionId}

-- | The ID of the interconnect or LAG.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcConnectionId :: Lens.Lens' DescribeHostedConnections Types.ConnectionId
dhcConnectionId = Lens.field @"connectionId"
{-# DEPRECATED dhcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Core.FromJSON DescribeHostedConnections where
  toJSON DescribeHostedConnections {..} =
    Core.object
      (Core.catMaybes [Core.Just ("connectionId" Core..= connectionId)])

instance Core.AWSRequest DescribeHostedConnections where
  type Rs DescribeHostedConnections = Types.Connections
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.DescribeHostedConnections")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
