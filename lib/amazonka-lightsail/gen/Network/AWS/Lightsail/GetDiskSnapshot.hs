{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk snapshot.
module Network.AWS.Lightsail.GetDiskSnapshot
    (
    -- * Creating a request
      GetDiskSnapshot (..)
    , mkGetDiskSnapshot
    -- ** Request lenses
    , gdsDiskSnapshotName

    -- * Destructuring the response
    , GetDiskSnapshotResponse (..)
    , mkGetDiskSnapshotResponse
    -- ** Response lenses
    , gdsrfrsDiskSnapshot
    , gdsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDiskSnapshot' smart constructor.
newtype GetDiskSnapshot = GetDiskSnapshot'
  { diskSnapshotName :: Types.ResourceName
    -- ^ The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDiskSnapshot' value with any optional fields omitted.
mkGetDiskSnapshot
    :: Types.ResourceName -- ^ 'diskSnapshotName'
    -> GetDiskSnapshot
mkGetDiskSnapshot diskSnapshotName
  = GetDiskSnapshot'{diskSnapshotName}

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDiskSnapshotName :: Lens.Lens' GetDiskSnapshot Types.ResourceName
gdsDiskSnapshotName = Lens.field @"diskSnapshotName"
{-# INLINEABLE gdsDiskSnapshotName #-}
{-# DEPRECATED diskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead"  #-}

instance Core.ToQuery GetDiskSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDiskSnapshot where
        toHeaders GetDiskSnapshot{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDiskSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDiskSnapshot where
        toJSON GetDiskSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("diskSnapshotName" Core..= diskSnapshotName)])

instance Core.AWSRequest GetDiskSnapshot where
        type Rs GetDiskSnapshot = GetDiskSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDiskSnapshotResponse' Core.<$>
                   (x Core..:? "diskSnapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDiskSnapshotResponse' smart constructor.
data GetDiskSnapshotResponse = GetDiskSnapshotResponse'
  { diskSnapshot :: Core.Maybe Types.DiskSnapshot
    -- ^ An object containing information about the disk snapshot.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDiskSnapshotResponse' value with any optional fields omitted.
mkGetDiskSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDiskSnapshotResponse
mkGetDiskSnapshotResponse responseStatus
  = GetDiskSnapshotResponse'{diskSnapshot = Core.Nothing,
                             responseStatus}

-- | An object containing information about the disk snapshot.
--
-- /Note:/ Consider using 'diskSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrfrsDiskSnapshot :: Lens.Lens' GetDiskSnapshotResponse (Core.Maybe Types.DiskSnapshot)
gdsrfrsDiskSnapshot = Lens.field @"diskSnapshot"
{-# INLINEABLE gdsrfrsDiskSnapshot #-}
{-# DEPRECATED diskSnapshot "Use generic-lens or generic-optics with 'diskSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrfrsResponseStatus :: Lens.Lens' GetDiskSnapshotResponse Core.Int
gdsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
