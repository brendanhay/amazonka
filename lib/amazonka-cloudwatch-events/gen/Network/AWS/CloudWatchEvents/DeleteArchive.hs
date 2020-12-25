{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteArchive (..),
    mkDeleteArchive,

    -- ** Request lenses
    dArchiveName,

    -- * Destructuring the response
    DeleteArchiveResponse (..),
    mkDeleteArchiveResponse,

    -- ** Response lenses
    darfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteArchive' smart constructor.
newtype DeleteArchive = DeleteArchive'
  { -- | The name of the archive to delete.
    archiveName :: Types.ArchiveName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteArchive' value with any optional fields omitted.
mkDeleteArchive ::
  -- | 'archiveName'
  Types.ArchiveName ->
  DeleteArchive
mkDeleteArchive archiveName = DeleteArchive' {archiveName}

-- | The name of the archive to delete.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArchiveName :: Lens.Lens' DeleteArchive Types.ArchiveName
dArchiveName = Lens.field @"archiveName"
{-# DEPRECATED dArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

instance Core.FromJSON DeleteArchive where
  toJSON DeleteArchive {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ArchiveName" Core..= archiveName)])

instance Core.AWSRequest DeleteArchive where
  type Rs DeleteArchive = DeleteArchiveResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DeleteArchive")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteArchiveResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteArchiveResponse' smart constructor.
newtype DeleteArchiveResponse = DeleteArchiveResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteArchiveResponse' value with any optional fields omitted.
mkDeleteArchiveResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteArchiveResponse
mkDeleteArchiveResponse responseStatus =
  DeleteArchiveResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsResponseStatus :: Lens.Lens' DeleteArchiveResponse Core.Int
darfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
