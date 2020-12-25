{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AssociateConnectionWithLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing connection with a link aggregation group (LAG). The connection is interrupted and re-established as a member of the LAG (connectivity to AWS is interrupted). The connection must be hosted on the same AWS Direct Connect endpoint as the LAG, and its bandwidth must match the bandwidth for the LAG. You can re-associate a connection that's currently associated with a different LAG; however, if removing the connection would cause the original LAG to fall below its setting for minimum number of operational connections, the request fails.
--
-- Any virtual interfaces that are directly associated with the connection are automatically re-associated with the LAG. If the connection was originally associated with a different LAG, the virtual interfaces remain associated with the original LAG.
-- For interconnects, any hosted connections are automatically re-associated with the LAG. If the interconnect was originally associated with a different LAG, the hosted connections remain associated with the original LAG.
module Network.AWS.DirectConnect.AssociateConnectionWithLag
  ( -- * Creating a request
    AssociateConnectionWithLag (..),
    mkAssociateConnectionWithLag,

    -- ** Request lenses
    acwlConnectionId,
    acwlLagId,

    -- * Destructuring the response
    Types.Connection (..),
    Types.mkConnection,

    -- ** Response lenses
    Types.cAwsDevice,
    Types.cAwsDeviceV2,
    Types.cBandwidth,
    Types.cConnectionId,
    Types.cConnectionName,
    Types.cConnectionState,
    Types.cHasLogicalRedundancy,
    Types.cJumboFrameCapable,
    Types.cLagId,
    Types.cLoaIssueTime,
    Types.cLocation,
    Types.cOwnerAccount,
    Types.cPartnerName,
    Types.cProviderName,
    Types.cRegion,
    Types.cTags,
    Types.cVlan,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateConnectionWithLag' smart constructor.
data AssociateConnectionWithLag = AssociateConnectionWithLag'
  { -- | The ID of the connection.
    connectionId :: Types.ConnectionId,
    -- | The ID of the LAG with which to associate the connection.
    lagId :: Types.LagId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateConnectionWithLag' value with any optional fields omitted.
mkAssociateConnectionWithLag ::
  -- | 'connectionId'
  Types.ConnectionId ->
  -- | 'lagId'
  Types.LagId ->
  AssociateConnectionWithLag
mkAssociateConnectionWithLag connectionId lagId =
  AssociateConnectionWithLag' {connectionId, lagId}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwlConnectionId :: Lens.Lens' AssociateConnectionWithLag Types.ConnectionId
acwlConnectionId = Lens.field @"connectionId"
{-# DEPRECATED acwlConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the LAG with which to associate the connection.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwlLagId :: Lens.Lens' AssociateConnectionWithLag Types.LagId
acwlLagId = Lens.field @"lagId"
{-# DEPRECATED acwlLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Core.FromJSON AssociateConnectionWithLag where
  toJSON AssociateConnectionWithLag {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("lagId" Core..= lagId)
          ]
      )

instance Core.AWSRequest AssociateConnectionWithLag where
  type Rs AssociateConnectionWithLag = Types.Connection
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.AssociateConnectionWithLag")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
