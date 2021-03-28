{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RejectSharedDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a directory sharing request that was sent from the directory owner account.
module Network.AWS.DirectoryService.RejectSharedDirectory
    (
    -- * Creating a request
      RejectSharedDirectory (..)
    , mkRejectSharedDirectory
    -- ** Request lenses
    , rsdSharedDirectoryId

    -- * Destructuring the response
    , RejectSharedDirectoryResponse (..)
    , mkRejectSharedDirectoryResponse
    -- ** Response lenses
    , rsdrrsSharedDirectoryId
    , rsdrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectSharedDirectory' smart constructor.
newtype RejectSharedDirectory = RejectSharedDirectory'
  { sharedDirectoryId :: Types.DirectoryId
    -- ^ Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectSharedDirectory' value with any optional fields omitted.
mkRejectSharedDirectory
    :: Types.DirectoryId -- ^ 'sharedDirectoryId'
    -> RejectSharedDirectory
mkRejectSharedDirectory sharedDirectoryId
  = RejectSharedDirectory'{sharedDirectoryId}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdSharedDirectoryId :: Lens.Lens' RejectSharedDirectory Types.DirectoryId
rsdSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# INLINEABLE rsdSharedDirectoryId #-}
{-# DEPRECATED sharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead"  #-}

instance Core.ToQuery RejectSharedDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RejectSharedDirectory where
        toHeaders RejectSharedDirectory{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.RejectSharedDirectory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RejectSharedDirectory where
        toJSON RejectSharedDirectory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SharedDirectoryId" Core..= sharedDirectoryId)])

instance Core.AWSRequest RejectSharedDirectory where
        type Rs RejectSharedDirectory = RejectSharedDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RejectSharedDirectoryResponse' Core.<$>
                   (x Core..:? "SharedDirectoryId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectSharedDirectoryResponse' smart constructor.
data RejectSharedDirectoryResponse = RejectSharedDirectoryResponse'
  { sharedDirectoryId :: Core.Maybe Types.DirectoryId
    -- ^ Identifier of the shared directory in the directory consumer account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectSharedDirectoryResponse' value with any optional fields omitted.
mkRejectSharedDirectoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectSharedDirectoryResponse
mkRejectSharedDirectoryResponse responseStatus
  = RejectSharedDirectoryResponse'{sharedDirectoryId = Core.Nothing,
                                   responseStatus}

-- | Identifier of the shared directory in the directory consumer account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdrrsSharedDirectoryId :: Lens.Lens' RejectSharedDirectoryResponse (Core.Maybe Types.DirectoryId)
rsdrrsSharedDirectoryId = Lens.field @"sharedDirectoryId"
{-# INLINEABLE rsdrrsSharedDirectoryId #-}
{-# DEPRECATED sharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdrrsResponseStatus :: Lens.Lens' RejectSharedDirectoryResponse Core.Int
rsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
