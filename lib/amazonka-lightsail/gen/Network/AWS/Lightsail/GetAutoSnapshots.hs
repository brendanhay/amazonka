{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetAutoSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the available automatic snapshots for an instance or disk. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.GetAutoSnapshots
    (
    -- * Creating a request
      GetAutoSnapshots (..)
    , mkGetAutoSnapshots
    -- ** Request lenses
    , gasResourceName

    -- * Destructuring the response
    , GetAutoSnapshotsResponse (..)
    , mkGetAutoSnapshotsResponse
    -- ** Response lenses
    , gasrrsAutoSnapshots
    , gasrrsResourceName
    , gasrrsResourceType
    , gasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAutoSnapshots' smart constructor.
newtype GetAutoSnapshots = GetAutoSnapshots'
  { resourceName :: Types.ResourceName
    -- ^ The name of the source instance or disk from which to get automatic snapshot information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAutoSnapshots' value with any optional fields omitted.
mkGetAutoSnapshots
    :: Types.ResourceName -- ^ 'resourceName'
    -> GetAutoSnapshots
mkGetAutoSnapshots resourceName = GetAutoSnapshots'{resourceName}

-- | The name of the source instance or disk from which to get automatic snapshot information.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasResourceName :: Lens.Lens' GetAutoSnapshots Types.ResourceName
gasResourceName = Lens.field @"resourceName"
{-# INLINEABLE gasResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

instance Core.ToQuery GetAutoSnapshots where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAutoSnapshots where
        toHeaders GetAutoSnapshots{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetAutoSnapshots")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAutoSnapshots where
        toJSON GetAutoSnapshots{..}
          = Core.object
              (Core.catMaybes [Core.Just ("resourceName" Core..= resourceName)])

instance Core.AWSRequest GetAutoSnapshots where
        type Rs GetAutoSnapshots = GetAutoSnapshotsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAutoSnapshotsResponse' Core.<$>
                   (x Core..:? "autoSnapshots") Core.<*> x Core..:? "resourceName"
                     Core.<*> x Core..:? "resourceType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAutoSnapshotsResponse' smart constructor.
data GetAutoSnapshotsResponse = GetAutoSnapshotsResponse'
  { autoSnapshots :: Core.Maybe [Types.AutoSnapshotDetails]
    -- ^ An array of objects that describe the automatic snapshots that are available for the specified source instance or disk.
  , resourceName :: Core.Maybe Types.ResourceName
    -- ^ The name of the source instance or disk for the automatic snapshots.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type (e.g., @Instance@ or @Disk@ ).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAutoSnapshotsResponse' value with any optional fields omitted.
mkGetAutoSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAutoSnapshotsResponse
mkGetAutoSnapshotsResponse responseStatus
  = GetAutoSnapshotsResponse'{autoSnapshots = Core.Nothing,
                              resourceName = Core.Nothing, resourceType = Core.Nothing,
                              responseStatus}

-- | An array of objects that describe the automatic snapshots that are available for the specified source instance or disk.
--
-- /Note:/ Consider using 'autoSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAutoSnapshots :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe [Types.AutoSnapshotDetails])
gasrrsAutoSnapshots = Lens.field @"autoSnapshots"
{-# INLINEABLE gasrrsAutoSnapshots #-}
{-# DEPRECATED autoSnapshots "Use generic-lens or generic-optics with 'autoSnapshots' instead"  #-}

-- | The name of the source instance or disk for the automatic snapshots.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResourceName :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe Types.ResourceName)
gasrrsResourceName = Lens.field @"resourceName"
{-# INLINEABLE gasrrsResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The resource type (e.g., @Instance@ or @Disk@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResourceType :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe Types.ResourceType)
gasrrsResourceType = Lens.field @"resourceType"
{-# INLINEABLE gasrrsResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAutoSnapshotsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
