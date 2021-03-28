{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to a gateway after the retrieval process is initiated. The virtual tape is returned to the VTS. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CancelRetrieval
    (
    -- * Creating a request
      CancelRetrieval (..)
    , mkCancelRetrieval
    -- ** Request lenses
    , crGatewayARN
    , crTapeARN

    -- * Destructuring the response
    , CancelRetrievalResponse (..)
    , mkCancelRetrievalResponse
    -- ** Response lenses
    , crrrsTapeARN
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CancelRetrievalInput
--
-- /See:/ 'mkCancelRetrieval' smart constructor.
data CancelRetrieval = CancelRetrieval'
  { gatewayARN :: Types.GatewayARN
  , tapeARN :: Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of the virtual tape you want to cancel retrieval for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelRetrieval' value with any optional fields omitted.
mkCancelRetrieval
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Types.TapeARN -- ^ 'tapeARN'
    -> CancelRetrieval
mkCancelRetrieval gatewayARN tapeARN
  = CancelRetrieval'{gatewayARN, tapeARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crGatewayARN :: Lens.Lens' CancelRetrieval Types.GatewayARN
crGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE crGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel retrieval for.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTapeARN :: Lens.Lens' CancelRetrieval Types.TapeARN
crTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE crTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

instance Core.ToQuery CancelRetrieval where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelRetrieval where
        toHeaders CancelRetrieval{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.CancelRetrieval")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelRetrieval where
        toJSON CancelRetrieval{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("TapeARN" Core..= tapeARN)])

instance Core.AWSRequest CancelRetrieval where
        type Rs CancelRetrieval = CancelRetrievalResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelRetrievalResponse' Core.<$>
                   (x Core..:? "TapeARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | CancelRetrievalOutput
--
-- /See:/ 'mkCancelRetrievalResponse' smart constructor.
data CancelRetrievalResponse = CancelRetrievalResponse'
  { tapeARN :: Core.Maybe Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of the virtual tape for which retrieval was canceled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelRetrievalResponse' value with any optional fields omitted.
mkCancelRetrievalResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelRetrievalResponse
mkCancelRetrievalResponse responseStatus
  = CancelRetrievalResponse'{tapeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval was canceled.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsTapeARN :: Lens.Lens' CancelRetrievalResponse (Core.Maybe Types.TapeARN)
crrrsTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE crrrsTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CancelRetrievalResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
