{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified interconnect.
module Network.AWS.DirectConnect.DeleteInterconnect
    (
    -- * Creating a request
      DeleteInterconnect (..)
    , mkDeleteInterconnect
    -- ** Request lenses
    , dInterconnectId

    -- * Destructuring the response
    , DeleteInterconnectResponse (..)
    , mkDeleteInterconnectResponse
    -- ** Response lenses
    , dirrsInterconnectState
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInterconnect' smart constructor.
newtype DeleteInterconnect = DeleteInterconnect'
  { interconnectId :: Types.InterconnectId
    -- ^ The ID of the interconnect.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInterconnect' value with any optional fields omitted.
mkDeleteInterconnect
    :: Types.InterconnectId -- ^ 'interconnectId'
    -> DeleteInterconnect
mkDeleteInterconnect interconnectId
  = DeleteInterconnect'{interconnectId}

-- | The ID of the interconnect.
--
-- /Note:/ Consider using 'interconnectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInterconnectId :: Lens.Lens' DeleteInterconnect Types.InterconnectId
dInterconnectId = Lens.field @"interconnectId"
{-# INLINEABLE dInterconnectId #-}
{-# DEPRECATED interconnectId "Use generic-lens or generic-optics with 'interconnectId' instead"  #-}

instance Core.ToQuery DeleteInterconnect where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInterconnect where
        toHeaders DeleteInterconnect{..}
          = Core.pure ("X-Amz-Target", "OvertureService.DeleteInterconnect")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteInterconnect where
        toJSON DeleteInterconnect{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("interconnectId" Core..= interconnectId)])

instance Core.AWSRequest DeleteInterconnect where
        type Rs DeleteInterconnect = DeleteInterconnectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteInterconnectResponse' Core.<$>
                   (x Core..:? "interconnectState") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteInterconnectResponse' smart constructor.
data DeleteInterconnectResponse = DeleteInterconnectResponse'
  { interconnectState :: Core.Maybe Types.InterconnectState
    -- ^ The state of the interconnect. The following are the possible values:
--
--
--     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The interconnect is approved, and is being initialized.
--
--
--     * @available@ : The network link is up, and the interconnect is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The interconnect is being deleted.
--
--
--     * @deleted@ : The interconnect is deleted.
--
--
--     * @unknown@ : The state of the interconnect is not available.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInterconnectResponse' value with any optional fields omitted.
mkDeleteInterconnectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteInterconnectResponse
mkDeleteInterconnectResponse responseStatus
  = DeleteInterconnectResponse'{interconnectState = Core.Nothing,
                                responseStatus}

-- | The state of the interconnect. The following are the possible values:
--
--
--     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The interconnect is approved, and is being initialized.
--
--
--     * @available@ : The network link is up, and the interconnect is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The interconnect is being deleted.
--
--
--     * @deleted@ : The interconnect is deleted.
--
--
--     * @unknown@ : The state of the interconnect is not available.
--
--
--
-- /Note:/ Consider using 'interconnectState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsInterconnectState :: Lens.Lens' DeleteInterconnectResponse (Core.Maybe Types.InterconnectState)
dirrsInterconnectState = Lens.field @"interconnectState"
{-# INLINEABLE dirrsInterconnectState #-}
{-# DEPRECATED interconnectState "Use generic-lens or generic-optics with 'interconnectState' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteInterconnectResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
