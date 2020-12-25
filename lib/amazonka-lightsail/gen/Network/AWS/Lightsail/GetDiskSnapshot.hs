{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetDiskSnapshot (..),
    mkGetDiskSnapshot,

    -- ** Request lenses
    gdsDiskSnapshotName,

    -- * Destructuring the response
    GetDiskSnapshotResponse (..),
    mkGetDiskSnapshotResponse,

    -- ** Response lenses
    gdsrfrsDiskSnapshot,
    gdsrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDiskSnapshot' smart constructor.
newtype GetDiskSnapshot = GetDiskSnapshot'
  { -- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
    diskSnapshotName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDiskSnapshot' value with any optional fields omitted.
mkGetDiskSnapshot ::
  -- | 'diskSnapshotName'
  Types.ResourceName ->
  GetDiskSnapshot
mkGetDiskSnapshot diskSnapshotName =
  GetDiskSnapshot' {diskSnapshotName}

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDiskSnapshotName :: Lens.Lens' GetDiskSnapshot Types.ResourceName
gdsDiskSnapshotName = Lens.field @"diskSnapshotName"
{-# DEPRECATED gdsDiskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead." #-}

instance Core.FromJSON GetDiskSnapshot where
  toJSON GetDiskSnapshot {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("diskSnapshotName" Core..= diskSnapshotName)]
      )

instance Core.AWSRequest GetDiskSnapshot where
  type Rs GetDiskSnapshot = GetDiskSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDiskSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiskSnapshotResponse'
            Core.<$> (x Core..:? "diskSnapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDiskSnapshotResponse' smart constructor.
data GetDiskSnapshotResponse = GetDiskSnapshotResponse'
  { -- | An object containing information about the disk snapshot.
    diskSnapshot :: Core.Maybe Types.DiskSnapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDiskSnapshotResponse' value with any optional fields omitted.
mkGetDiskSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDiskSnapshotResponse
mkGetDiskSnapshotResponse responseStatus =
  GetDiskSnapshotResponse'
    { diskSnapshot = Core.Nothing,
      responseStatus
    }

-- | An object containing information about the disk snapshot.
--
-- /Note:/ Consider using 'diskSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrfrsDiskSnapshot :: Lens.Lens' GetDiskSnapshotResponse (Core.Maybe Types.DiskSnapshot)
gdsrfrsDiskSnapshot = Lens.field @"diskSnapshot"
{-# DEPRECATED gdsrfrsDiskSnapshot "Use generic-lens or generic-optics with 'diskSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrfrsResponseStatus :: Lens.Lens' GetDiskSnapshotResponse Core.Int
gdsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
