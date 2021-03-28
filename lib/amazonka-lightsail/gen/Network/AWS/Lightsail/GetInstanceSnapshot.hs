{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific instance snapshot.
module Network.AWS.Lightsail.GetInstanceSnapshot
    (
    -- * Creating a request
      GetInstanceSnapshot (..)
    , mkGetInstanceSnapshot
    -- ** Request lenses
    , gisInstanceSnapshotName

    -- * Destructuring the response
    , GetInstanceSnapshotResponse (..)
    , mkGetInstanceSnapshotResponse
    -- ** Response lenses
    , gisrfrsInstanceSnapshot
    , gisrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceSnapshot' smart constructor.
newtype GetInstanceSnapshot = GetInstanceSnapshot'
  { instanceSnapshotName :: Types.InstanceSnapshotName
    -- ^ The name of the snapshot for which you are requesting information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceSnapshot' value with any optional fields omitted.
mkGetInstanceSnapshot
    :: Types.InstanceSnapshotName -- ^ 'instanceSnapshotName'
    -> GetInstanceSnapshot
mkGetInstanceSnapshot instanceSnapshotName
  = GetInstanceSnapshot'{instanceSnapshotName}

-- | The name of the snapshot for which you are requesting information.
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisInstanceSnapshotName :: Lens.Lens' GetInstanceSnapshot Types.InstanceSnapshotName
gisInstanceSnapshotName = Lens.field @"instanceSnapshotName"
{-# INLINEABLE gisInstanceSnapshotName #-}
{-# DEPRECATED instanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead"  #-}

instance Core.ToQuery GetInstanceSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInstanceSnapshot where
        toHeaders GetInstanceSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.GetInstanceSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInstanceSnapshot where
        toJSON GetInstanceSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("instanceSnapshotName" Core..= instanceSnapshotName)])

instance Core.AWSRequest GetInstanceSnapshot where
        type Rs GetInstanceSnapshot = GetInstanceSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInstanceSnapshotResponse' Core.<$>
                   (x Core..:? "instanceSnapshot") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInstanceSnapshotResponse' smart constructor.
data GetInstanceSnapshotResponse = GetInstanceSnapshotResponse'
  { instanceSnapshot :: Core.Maybe Types.InstanceSnapshot
    -- ^ An array of key-value pairs containing information about the results of your get instance snapshot request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetInstanceSnapshotResponse' value with any optional fields omitted.
mkGetInstanceSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInstanceSnapshotResponse
mkGetInstanceSnapshotResponse responseStatus
  = GetInstanceSnapshotResponse'{instanceSnapshot = Core.Nothing,
                                 responseStatus}

-- | An array of key-value pairs containing information about the results of your get instance snapshot request.
--
-- /Note:/ Consider using 'instanceSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrfrsInstanceSnapshot :: Lens.Lens' GetInstanceSnapshotResponse (Core.Maybe Types.InstanceSnapshot)
gisrfrsInstanceSnapshot = Lens.field @"instanceSnapshot"
{-# INLINEABLE gisrfrsInstanceSnapshot #-}
{-# DEPRECATED instanceSnapshot "Use generic-lens or generic-optics with 'instanceSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrfrsResponseStatus :: Lens.Lens' GetInstanceSnapshotResponse Core.Int
gisrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gisrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
