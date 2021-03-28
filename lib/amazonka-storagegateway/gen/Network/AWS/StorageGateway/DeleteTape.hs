{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.DeleteTape
    (
    -- * Creating a request
      DeleteTape (..)
    , mkDeleteTape
    -- ** Request lenses
    , dtfGatewayARN
    , dtfTapeARN
    , dtfBypassGovernanceRetention

    -- * Destructuring the response
    , DeleteTapeResponse (..)
    , mkDeleteTapeResponse
    -- ** Response lenses
    , dtrfrsTapeARN
    , dtrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DeleteTapeInput
--
-- /See:/ 'mkDeleteTape' smart constructor.
data DeleteTape = DeleteTape'
  { gatewayARN :: Types.GatewayARN
    -- ^ The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
  , tapeARN :: Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of the virtual tape to delete.
  , bypassGovernanceRetention :: Core.Maybe Core.Bool
    -- ^ Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTape' value with any optional fields omitted.
mkDeleteTape
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Types.TapeARN -- ^ 'tapeARN'
    -> DeleteTape
mkDeleteTape gatewayARN tapeARN
  = DeleteTape'{gatewayARN, tapeARN,
                bypassGovernanceRetention = Core.Nothing}

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfGatewayARN :: Lens.Lens' DeleteTape Types.GatewayARN
dtfGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dtfGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfTapeARN :: Lens.Lens' DeleteTape Types.TapeARN
dtfTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE dtfTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfBypassGovernanceRetention :: Lens.Lens' DeleteTape (Core.Maybe Core.Bool)
dtfBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# INLINEABLE dtfBypassGovernanceRetention #-}
{-# DEPRECATED bypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead"  #-}

instance Core.ToQuery DeleteTape where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTape where
        toHeaders DeleteTape{..}
          = Core.pure ("X-Amz-Target", "StorageGateway_20130630.DeleteTape")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTape where
        toJSON DeleteTape{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("TapeARN" Core..= tapeARN),
                  ("BypassGovernanceRetention" Core..=) Core.<$>
                    bypassGovernanceRetention])

instance Core.AWSRequest DeleteTape where
        type Rs DeleteTape = DeleteTapeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteTapeResponse' Core.<$>
                   (x Core..:? "TapeARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | DeleteTapeOutput
--
-- /See:/ 'mkDeleteTapeResponse' smart constructor.
data DeleteTapeResponse = DeleteTapeResponse'
  { tapeARN :: Core.Maybe Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of the deleted virtual tape.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapeResponse' value with any optional fields omitted.
mkDeleteTapeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTapeResponse
mkDeleteTapeResponse responseStatus
  = DeleteTapeResponse'{tapeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsTapeARN :: Lens.Lens' DeleteTapeResponse (Core.Maybe Types.TapeARN)
dtrfrsTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE dtrfrsTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeleteTapeResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
