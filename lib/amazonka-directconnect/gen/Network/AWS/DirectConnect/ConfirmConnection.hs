{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ConfirmConnection (..)
    , mkConfirmConnection
    -- ** Request lenses
    , ccConnectionId

    -- * Destructuring the response
    , ConfirmConnectionResponse (..)
    , mkConfirmConnectionResponse
    -- ** Response lenses
    , ccrrsConnectionState
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkConfirmConnection' smart constructor.
newtype ConfirmConnection = ConfirmConnection'
  { connectionId :: Types.ConnectionId
    -- ^ The ID of the hosted connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmConnection' value with any optional fields omitted.
mkConfirmConnection
    :: Types.ConnectionId -- ^ 'connectionId'
    -> ConfirmConnection
mkConfirmConnection connectionId = ConfirmConnection'{connectionId}

-- | The ID of the hosted connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConnectionId :: Lens.Lens' ConfirmConnection Types.ConnectionId
ccConnectionId = Lens.field @"connectionId"
{-# INLINEABLE ccConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

instance Core.ToQuery ConfirmConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ConfirmConnection where
        toHeaders ConfirmConnection{..}
          = Core.pure ("X-Amz-Target", "OvertureService.ConfirmConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ConfirmConnection where
        toJSON ConfirmConnection{..}
          = Core.object
              (Core.catMaybes [Core.Just ("connectionId" Core..= connectionId)])

instance Core.AWSRequest ConfirmConnection where
        type Rs ConfirmConnection = ConfirmConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ConfirmConnectionResponse' Core.<$>
                   (x Core..:? "connectionState") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkConfirmConnectionResponse' smart constructor.
data ConfirmConnectionResponse = ConfirmConnectionResponse'
  { connectionState :: Core.Maybe Types.ConnectionState
    -- ^ The state of the connection. The following are the possible values:
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
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmConnectionResponse' value with any optional fields omitted.
mkConfirmConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ConfirmConnectionResponse
mkConfirmConnectionResponse responseStatus
  = ConfirmConnectionResponse'{connectionState = Core.Nothing,
                               responseStatus}

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
{-# INLINEABLE ccrrsConnectionState #-}
{-# DEPRECATED connectionState "Use generic-lens or generic-optics with 'connectionState' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' ConfirmConnectionResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
