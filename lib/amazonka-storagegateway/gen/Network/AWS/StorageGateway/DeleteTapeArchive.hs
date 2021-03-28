{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape from the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.DeleteTapeArchive
    (
    -- * Creating a request
      DeleteTapeArchive (..)
    , mkDeleteTapeArchive
    -- ** Request lenses
    , dtaTapeARN
    , dtaBypassGovernanceRetention

    -- * Destructuring the response
    , DeleteTapeArchiveResponse (..)
    , mkDeleteTapeArchiveResponse
    -- ** Response lenses
    , dtarfrsTapeARN
    , dtarfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DeleteTapeArchiveInput
--
-- /See:/ 'mkDeleteTapeArchive' smart constructor.
data DeleteTapeArchive = DeleteTapeArchive'
  { tapeARN :: Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual tape shelf (VTS).
  , bypassGovernanceRetention :: Core.Maybe Core.Bool
    -- ^ Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapeArchive' value with any optional fields omitted.
mkDeleteTapeArchive
    :: Types.TapeARN -- ^ 'tapeARN'
    -> DeleteTapeArchive
mkDeleteTapeArchive tapeARN
  = DeleteTapeArchive'{tapeARN,
                       bypassGovernanceRetention = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaTapeARN :: Lens.Lens' DeleteTapeArchive Types.TapeARN
dtaTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE dtaTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaBypassGovernanceRetention :: Lens.Lens' DeleteTapeArchive (Core.Maybe Core.Bool)
dtaBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# INLINEABLE dtaBypassGovernanceRetention #-}
{-# DEPRECATED bypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead"  #-}

instance Core.ToQuery DeleteTapeArchive where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTapeArchive where
        toHeaders DeleteTapeArchive{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DeleteTapeArchive")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTapeArchive where
        toJSON DeleteTapeArchive{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TapeARN" Core..= tapeARN),
                  ("BypassGovernanceRetention" Core..=) Core.<$>
                    bypassGovernanceRetention])

instance Core.AWSRequest DeleteTapeArchive where
        type Rs DeleteTapeArchive = DeleteTapeArchiveResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteTapeArchiveResponse' Core.<$>
                   (x Core..:? "TapeARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | DeleteTapeArchiveOutput
--
-- /See:/ 'mkDeleteTapeArchiveResponse' smart constructor.
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
  { tapeARN :: Core.Maybe Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of the virtual tape that was deleted from the virtual tape shelf (VTS).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapeArchiveResponse' value with any optional fields omitted.
mkDeleteTapeArchiveResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTapeArchiveResponse
mkDeleteTapeArchiveResponse responseStatus
  = DeleteTapeArchiveResponse'{tapeARN = Core.Nothing,
                               responseStatus}

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarfrsTapeARN :: Lens.Lens' DeleteTapeArchiveResponse (Core.Maybe Types.TapeARN)
dtarfrsTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE dtarfrsTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarfrsResponseStatus :: Lens.Lens' DeleteTapeArchiveResponse Core.Int
dtarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
