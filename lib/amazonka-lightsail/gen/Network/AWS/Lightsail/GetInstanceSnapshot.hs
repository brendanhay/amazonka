{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetInstanceSnapshot (..),
    mkGetInstanceSnapshot,

    -- ** Request lenses
    gisInstanceSnapshotName,

    -- * Destructuring the response
    GetInstanceSnapshotResponse (..),
    mkGetInstanceSnapshotResponse,

    -- ** Response lenses
    gisrfrsInstanceSnapshot,
    gisrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceSnapshot' smart constructor.
newtype GetInstanceSnapshot = GetInstanceSnapshot'
  { -- | The name of the snapshot for which you are requesting information.
    instanceSnapshotName :: Types.InstanceSnapshotName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceSnapshot' value with any optional fields omitted.
mkGetInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Types.InstanceSnapshotName ->
  GetInstanceSnapshot
mkGetInstanceSnapshot instanceSnapshotName =
  GetInstanceSnapshot' {instanceSnapshotName}

-- | The name of the snapshot for which you are requesting information.
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisInstanceSnapshotName :: Lens.Lens' GetInstanceSnapshot Types.InstanceSnapshotName
gisInstanceSnapshotName = Lens.field @"instanceSnapshotName"
{-# DEPRECATED gisInstanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead." #-}

instance Core.FromJSON GetInstanceSnapshot where
  toJSON GetInstanceSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("instanceSnapshotName" Core..= instanceSnapshotName)]
      )

instance Core.AWSRequest GetInstanceSnapshot where
  type Rs GetInstanceSnapshot = GetInstanceSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetInstanceSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotResponse'
            Core.<$> (x Core..:? "instanceSnapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInstanceSnapshotResponse' smart constructor.
data GetInstanceSnapshotResponse = GetInstanceSnapshotResponse'
  { -- | An array of key-value pairs containing information about the results of your get instance snapshot request.
    instanceSnapshot :: Core.Maybe Types.InstanceSnapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInstanceSnapshotResponse' value with any optional fields omitted.
mkGetInstanceSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstanceSnapshotResponse
mkGetInstanceSnapshotResponse responseStatus =
  GetInstanceSnapshotResponse'
    { instanceSnapshot = Core.Nothing,
      responseStatus
    }

-- | An array of key-value pairs containing information about the results of your get instance snapshot request.
--
-- /Note:/ Consider using 'instanceSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrfrsInstanceSnapshot :: Lens.Lens' GetInstanceSnapshotResponse (Core.Maybe Types.InstanceSnapshot)
gisrfrsInstanceSnapshot = Lens.field @"instanceSnapshot"
{-# DEPRECATED gisrfrsInstanceSnapshot "Use generic-lens or generic-optics with 'instanceSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrfrsResponseStatus :: Lens.Lens' GetInstanceSnapshotResponse Core.Int
gisrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gisrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
