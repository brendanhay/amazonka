{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeMountTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of all the current mount targets, or a specific mount target, for a file system. When requesting all of the current mount targets, the order of mount targets returned in the response is unspecified.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeMountTargets@ action, on either the file system ID that you specify in @FileSystemId@ , or on the file system of the mount target that you specify in @MountTargetId@ .
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeMountTargets
    (
    -- * Creating a request
      DescribeMountTargets (..)
    , mkDescribeMountTargets
    -- ** Request lenses
    , dmtAccessPointId
    , dmtFileSystemId
    , dmtMarker
    , dmtMaxItems
    , dmtMountTargetId

    -- * Destructuring the response
    , DescribeMountTargetsResponse (..)
    , mkDescribeMountTargetsResponse
    -- ** Response lenses
    , dmtrrsMarker
    , dmtrrsMountTargets
    , dmtrrsNextMarker
    , dmtrrsResponseStatus
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeMountTargets' smart constructor.
data DescribeMountTargets = DescribeMountTargets'
  { accessPointId :: Core.Maybe Types.AccessPointId
    -- ^ (Optional) The ID of the access point whose mount targets that you want to list. It must be included in your request if a @FileSystemId@ or @MountTargetId@ is not included in your request. Accepts either an access point ID or ARN as input.
  , fileSystemId :: Core.Maybe Types.FileSystemId
    -- ^ (Optional) ID of the file system whose mount targets you want to list (String). It must be included in your request if an @AccessPointId@ or @MountTargetId@ is not included. Accepts either a file system ID or ARN as input.
  , marker :: Core.Maybe Types.Marker
    -- ^ (Optional) Opaque pagination token returned from a previous @DescribeMountTargets@ operation (String). If present, it specifies to continue the list from where the previous returning call left off.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ (Optional) Maximum number of mount targets to return in the response. Currently, this number is automatically set to 10, and other values are ignored. The response is paginated at 100 per page if you have more than 100 mount targets.
  , mountTargetId :: Core.Maybe Types.MountTargetId
    -- ^ (Optional) ID of the mount target that you want to have described (String). It must be included in your request if @FileSystemId@ is not included. Accepts either a mount target ID or ARN as input.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMountTargets' value with any optional fields omitted.
mkDescribeMountTargets
    :: DescribeMountTargets
mkDescribeMountTargets
  = DescribeMountTargets'{accessPointId = Core.Nothing,
                          fileSystemId = Core.Nothing, marker = Core.Nothing,
                          maxItems = Core.Nothing, mountTargetId = Core.Nothing}

-- | (Optional) The ID of the access point whose mount targets that you want to list. It must be included in your request if a @FileSystemId@ or @MountTargetId@ is not included in your request. Accepts either an access point ID or ARN as input.
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtAccessPointId :: Lens.Lens' DescribeMountTargets (Core.Maybe Types.AccessPointId)
dmtAccessPointId = Lens.field @"accessPointId"
{-# INLINEABLE dmtAccessPointId #-}
{-# DEPRECATED accessPointId "Use generic-lens or generic-optics with 'accessPointId' instead"  #-}

-- | (Optional) ID of the file system whose mount targets you want to list (String). It must be included in your request if an @AccessPointId@ or @MountTargetId@ is not included. Accepts either a file system ID or ARN as input.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtFileSystemId :: Lens.Lens' DescribeMountTargets (Core.Maybe Types.FileSystemId)
dmtFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE dmtFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | (Optional) Opaque pagination token returned from a previous @DescribeMountTargets@ operation (String). If present, it specifies to continue the list from where the previous returning call left off.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtMarker :: Lens.Lens' DescribeMountTargets (Core.Maybe Types.Marker)
dmtMarker = Lens.field @"marker"
{-# INLINEABLE dmtMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | (Optional) Maximum number of mount targets to return in the response. Currently, this number is automatically set to 10, and other values are ignored. The response is paginated at 100 per page if you have more than 100 mount targets.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtMaxItems :: Lens.Lens' DescribeMountTargets (Core.Maybe Core.Natural)
dmtMaxItems = Lens.field @"maxItems"
{-# INLINEABLE dmtMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | (Optional) ID of the mount target that you want to have described (String). It must be included in your request if @FileSystemId@ is not included. Accepts either a mount target ID or ARN as input.
--
-- /Note:/ Consider using 'mountTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtMountTargetId :: Lens.Lens' DescribeMountTargets (Core.Maybe Types.MountTargetId)
dmtMountTargetId = Lens.field @"mountTargetId"
{-# INLINEABLE dmtMountTargetId #-}
{-# DEPRECATED mountTargetId "Use generic-lens or generic-optics with 'mountTargetId' instead"  #-}

instance Core.ToQuery DescribeMountTargets where
        toQuery DescribeMountTargets{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AccessPointId")
              accessPointId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FileSystemId")
                fileSystemId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MountTargetId")
                mountTargetId

instance Core.ToHeaders DescribeMountTargets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeMountTargets where
        type Rs DescribeMountTargets = DescribeMountTargetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2015-02-01/mount-targets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMountTargetsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "MountTargets" Core.<*>
                     x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeMountTargets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"mountTargets" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | 
--
-- /See:/ 'mkDescribeMountTargetsResponse' smart constructor.
data DescribeMountTargetsResponse = DescribeMountTargetsResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ If the request included the @Marker@ , the response returns that value in this field.
  , mountTargets :: Core.Maybe [Types.MountTargetDescription]
    -- ^ Returns the file system's mount targets as an array of @MountTargetDescription@ objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If a value is present, there are more mount targets to return. In a subsequent request, you can provide @Marker@ in your request with this value to retrieve the next set of mount targets.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMountTargetsResponse' value with any optional fields omitted.
mkDescribeMountTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMountTargetsResponse
mkDescribeMountTargetsResponse responseStatus
  = DescribeMountTargetsResponse'{marker = Core.Nothing,
                                  mountTargets = Core.Nothing, nextMarker = Core.Nothing,
                                  responseStatus}

-- | If the request included the @Marker@ , the response returns that value in this field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrrsMarker :: Lens.Lens' DescribeMountTargetsResponse (Core.Maybe Types.Marker)
dmtrrsMarker = Lens.field @"marker"
{-# INLINEABLE dmtrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Returns the file system's mount targets as an array of @MountTargetDescription@ objects.
--
-- /Note:/ Consider using 'mountTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrrsMountTargets :: Lens.Lens' DescribeMountTargetsResponse (Core.Maybe [Types.MountTargetDescription])
dmtrrsMountTargets = Lens.field @"mountTargets"
{-# INLINEABLE dmtrrsMountTargets #-}
{-# DEPRECATED mountTargets "Use generic-lens or generic-optics with 'mountTargets' instead"  #-}

-- | If a value is present, there are more mount targets to return. In a subsequent request, you can provide @Marker@ in your request with this value to retrieve the next set of mount targets.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrrsNextMarker :: Lens.Lens' DescribeMountTargetsResponse (Core.Maybe Types.NextMarker)
dmtrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE dmtrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmtrrsResponseStatus :: Lens.Lens' DescribeMountTargetsResponse Core.Int
dmtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
