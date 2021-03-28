{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeleteArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified archive.
module Network.AWS.CloudWatchEvents.DeleteArchive
    (
    -- * Creating a request
      DeleteArchive (..)
    , mkDeleteArchive
    -- ** Request lenses
    , dArchiveName

    -- * Destructuring the response
    , DeleteArchiveResponse (..)
    , mkDeleteArchiveResponse
    -- ** Response lenses
    , darfrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteArchive' smart constructor.
newtype DeleteArchive = DeleteArchive'
  { archiveName :: Types.ArchiveName
    -- ^ The name of the archive to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteArchive' value with any optional fields omitted.
mkDeleteArchive
    :: Types.ArchiveName -- ^ 'archiveName'
    -> DeleteArchive
mkDeleteArchive archiveName = DeleteArchive'{archiveName}

-- | The name of the archive to delete.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArchiveName :: Lens.Lens' DeleteArchive Types.ArchiveName
dArchiveName = Lens.field @"archiveName"
{-# INLINEABLE dArchiveName #-}
{-# DEPRECATED archiveName "Use generic-lens or generic-optics with 'archiveName' instead"  #-}

instance Core.ToQuery DeleteArchive where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteArchive where
        toHeaders DeleteArchive{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DeleteArchive") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteArchive where
        toJSON DeleteArchive{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ArchiveName" Core..= archiveName)])

instance Core.AWSRequest DeleteArchive where
        type Rs DeleteArchive = DeleteArchiveResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteArchiveResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteArchiveResponse' smart constructor.
newtype DeleteArchiveResponse = DeleteArchiveResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteArchiveResponse' value with any optional fields omitted.
mkDeleteArchiveResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteArchiveResponse
mkDeleteArchiveResponse responseStatus
  = DeleteArchiveResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsResponseStatus :: Lens.Lens' DeleteArchiveResponse Core.Int
darfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
