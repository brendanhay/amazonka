{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetAutoSnapshots (..),
    mkGetAutoSnapshots,

    -- ** Request lenses
    gasResourceName,

    -- * Destructuring the response
    GetAutoSnapshotsResponse (..),
    mkGetAutoSnapshotsResponse,

    -- ** Response lenses
    gasrrsAutoSnapshots,
    gasrrsResourceName,
    gasrrsResourceType,
    gasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAutoSnapshots' smart constructor.
newtype GetAutoSnapshots = GetAutoSnapshots'
  { -- | The name of the source instance or disk from which to get automatic snapshot information.
    resourceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAutoSnapshots' value with any optional fields omitted.
mkGetAutoSnapshots ::
  -- | 'resourceName'
  Types.ResourceName ->
  GetAutoSnapshots
mkGetAutoSnapshots resourceName = GetAutoSnapshots' {resourceName}

-- | The name of the source instance or disk from which to get automatic snapshot information.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasResourceName :: Lens.Lens' GetAutoSnapshots Types.ResourceName
gasResourceName = Lens.field @"resourceName"
{-# DEPRECATED gasResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Core.FromJSON GetAutoSnapshots where
  toJSON GetAutoSnapshots {..} =
    Core.object
      (Core.catMaybes [Core.Just ("resourceName" Core..= resourceName)])

instance Core.AWSRequest GetAutoSnapshots where
  type Rs GetAutoSnapshots = GetAutoSnapshotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetAutoSnapshots")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutoSnapshotsResponse'
            Core.<$> (x Core..:? "autoSnapshots")
            Core.<*> (x Core..:? "resourceName")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAutoSnapshotsResponse' smart constructor.
data GetAutoSnapshotsResponse = GetAutoSnapshotsResponse'
  { -- | An array of objects that describe the automatic snapshots that are available for the specified source instance or disk.
    autoSnapshots :: Core.Maybe [Types.AutoSnapshotDetails],
    -- | The name of the source instance or disk for the automatic snapshots.
    resourceName :: Core.Maybe Types.ResourceName,
    -- | The resource type (e.g., @Instance@ or @Disk@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetAutoSnapshotsResponse' value with any optional fields omitted.
mkGetAutoSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAutoSnapshotsResponse
mkGetAutoSnapshotsResponse responseStatus =
  GetAutoSnapshotsResponse'
    { autoSnapshots = Core.Nothing,
      resourceName = Core.Nothing,
      resourceType = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the automatic snapshots that are available for the specified source instance or disk.
--
-- /Note:/ Consider using 'autoSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAutoSnapshots :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe [Types.AutoSnapshotDetails])
gasrrsAutoSnapshots = Lens.field @"autoSnapshots"
{-# DEPRECATED gasrrsAutoSnapshots "Use generic-lens or generic-optics with 'autoSnapshots' instead." #-}

-- | The name of the source instance or disk for the automatic snapshots.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResourceName :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe Types.ResourceName)
gasrrsResourceName = Lens.field @"resourceName"
{-# DEPRECATED gasrrsResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The resource type (e.g., @Instance@ or @Disk@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResourceType :: Lens.Lens' GetAutoSnapshotsResponse (Core.Maybe Types.ResourceType)
gasrrsResourceType = Lens.field @"resourceType"
{-# DEPRECATED gasrrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAutoSnapshotsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
