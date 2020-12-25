{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms the creation of the specified hosted connection on an interconnect.
--
-- Upon creation, the hosted connection is initially in the @Ordering@ state, and remains in this state until the owner confirms creation of the hosted connection.
module Network.AWS.DirectConnect.ConfirmConnection
  ( -- * Creating a request
    ConfirmConnection (..),
    mkConfirmConnection,

    -- ** Request lenses
    ccConnectionId,

    -- * Destructuring the response
    ConfirmConnectionResponse (..),
    mkConfirmConnectionResponse,

    -- ** Response lenses
    ccrrsConnectionState,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkConfirmConnection' smart constructor.
newtype ConfirmConnection = ConfirmConnection'
  { -- | The ID of the hosted connection.
    connectionId :: Types.ConnectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmConnection' value with any optional fields omitted.
mkConfirmConnection ::
  -- | 'connectionId'
  Types.ConnectionId ->
  ConfirmConnection
mkConfirmConnection connectionId = ConfirmConnection' {connectionId}

-- | The ID of the hosted connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConnectionId :: Lens.Lens' ConfirmConnection Types.ConnectionId
ccConnectionId = Lens.field @"connectionId"
{-# DEPRECATED ccConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Core.FromJSON ConfirmConnection where
  toJSON ConfirmConnection {..} =
    Core.object
      (Core.catMaybes [Core.Just ("connectionId" Core..= connectionId)])

instance Core.AWSRequest ConfirmConnection where
  type Rs ConfirmConnection = ConfirmConnectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.ConfirmConnection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmConnectionResponse'
            Core.<$> (x Core..:? "connectionState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkConfirmConnectionResponse' smart constructor.
data ConfirmConnectionResponse = ConfirmConnectionResponse'
  { -- | The state of the connection. The following are the possible values:
    --
    --
    --     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
    --
    --
    --     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
    --
    --
    --     * @pending@ : The connection has been approved and is being initialized.
    --
    --
    --     * @available@ : The network link is up and the connection is ready for use.
    --
    --
    --     * @down@ : The network link is down.
    --
    --
    --     * @deleting@ : The connection is being deleted.
    --
    --
    --     * @deleted@ : The connection has been deleted.
    --
    --
    --     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.
    --
    --
    --     * @unknown@ : The state of the connection is not available.
    connectionState :: Core.Maybe Types.ConnectionState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmConnectionResponse' value with any optional fields omitted.
mkConfirmConnectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConfirmConnectionResponse
mkConfirmConnectionResponse responseStatus =
  ConfirmConnectionResponse'
    { connectionState = Core.Nothing,
      responseStatus
    }

-- | The state of the connection. The following are the possible values:
--
--
--     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
--
--
--     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The connection has been approved and is being initialized.
--
--
--     * @available@ : The network link is up and the connection is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The connection is being deleted.
--
--
--     * @deleted@ : The connection has been deleted.
--
--
--     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.
--
--
--     * @unknown@ : The state of the connection is not available.
--
--
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsConnectionState :: Lens.Lens' ConfirmConnectionResponse (Core.Maybe Types.ConnectionState)
ccrrsConnectionState = Lens.field @"connectionState"
{-# DEPRECATED ccrrsConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' ConfirmConnectionResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
