{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DisassociateConnectionFromLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection from a link aggregation group (LAG). The connection is interrupted and re-established as a standalone connection (the connection is not deleted; to delete the connection, use the 'DeleteConnection' request). If the LAG has associated virtual interfaces or hosted connections, they remain associated with the LAG. A disassociated connection owned by an AWS Direct Connect Partner is automatically converted to an interconnect.
--
-- If disassociating the connection would cause the LAG to fall below its setting for minimum number of operational connections, the request fails, except when it's the last member of the LAG. If all connections are disassociated, the LAG continues to exist as an empty LAG with no physical connections.
module Network.AWS.DirectConnect.DisassociateConnectionFromLag
  ( -- * Creating a request
    DisassociateConnectionFromLag (..),
    mkDisassociateConnectionFromLag,

    -- ** Request lenses
    dcflConnectionId,
    dcflLagId,

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

-- | /See:/ 'mkDisassociateConnectionFromLag' smart constructor.
data DisassociateConnectionFromLag = DisassociateConnectionFromLag'
  { -- | The ID of the connection.
    connectionId :: Types.ConnectionId,
    -- | The ID of the LAG.
    lagId :: Types.LagId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConnectionFromLag' value with any optional fields omitted.
mkDisassociateConnectionFromLag ::
  -- | 'connectionId'
  Types.ConnectionId ->
  -- | 'lagId'
  Types.LagId ->
  DisassociateConnectionFromLag
mkDisassociateConnectionFromLag connectionId lagId =
  DisassociateConnectionFromLag' {connectionId, lagId}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcflConnectionId :: Lens.Lens' DisassociateConnectionFromLag Types.ConnectionId
dcflConnectionId = Lens.field @"connectionId"
{-# DEPRECATED dcflConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcflLagId :: Lens.Lens' DisassociateConnectionFromLag Types.LagId
dcflLagId = Lens.field @"lagId"
{-# DEPRECATED dcflLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Core.FromJSON DisassociateConnectionFromLag where
  toJSON DisassociateConnectionFromLag {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("lagId" Core..= lagId)
          ]
      )

instance Core.AWSRequest DisassociateConnectionFromLag where
  type Rs DisassociateConnectionFromLag = Types.Connection
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.DisassociateConnectionFromLag")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
