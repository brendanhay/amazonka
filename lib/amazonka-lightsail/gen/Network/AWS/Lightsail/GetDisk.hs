{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk.
module Network.AWS.Lightsail.GetDisk
    (
    -- * Creating a request
      GetDisk (..)
    , mkGetDisk
    -- ** Request lenses
    , gdDiskName

    -- * Destructuring the response
    , GetDiskResponse (..)
    , mkGetDiskResponse
    -- ** Response lenses
    , gdrlrsDisk
    , gdrlrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDisk' smart constructor.
newtype GetDisk = GetDisk'
  { diskName :: Types.ResourceName
    -- ^ The name of the disk (e.g., @my-disk@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDisk' value with any optional fields omitted.
mkGetDisk
    :: Types.ResourceName -- ^ 'diskName'
    -> GetDisk
mkGetDisk diskName = GetDisk'{diskName}

-- | The name of the disk (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDiskName :: Lens.Lens' GetDisk Types.ResourceName
gdDiskName = Lens.field @"diskName"
{-# INLINEABLE gdDiskName #-}
{-# DEPRECATED diskName "Use generic-lens or generic-optics with 'diskName' instead"  #-}

instance Core.ToQuery GetDisk where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDisk where
        toHeaders GetDisk{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDisk") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDisk where
        toJSON GetDisk{..}
          = Core.object
              (Core.catMaybes [Core.Just ("diskName" Core..= diskName)])

instance Core.AWSRequest GetDisk where
        type Rs GetDisk = GetDiskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDiskResponse' Core.<$>
                   (x Core..:? "disk") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDiskResponse' smart constructor.
data GetDiskResponse = GetDiskResponse'
  { disk :: Core.Maybe Types.Disk
    -- ^ An object containing information about the disk.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDiskResponse' value with any optional fields omitted.
mkGetDiskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDiskResponse
mkGetDiskResponse responseStatus
  = GetDiskResponse'{disk = Core.Nothing, responseStatus}

-- | An object containing information about the disk.
--
-- /Note:/ Consider using 'disk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrlrsDisk :: Lens.Lens' GetDiskResponse (Core.Maybe Types.Disk)
gdrlrsDisk = Lens.field @"disk"
{-# INLINEABLE gdrlrsDisk #-}
{-# DEPRECATED disk "Use generic-lens or generic-optics with 'disk' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrlrsResponseStatus :: Lens.Lens' GetDiskResponse Core.Int
gdrlrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrlrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
