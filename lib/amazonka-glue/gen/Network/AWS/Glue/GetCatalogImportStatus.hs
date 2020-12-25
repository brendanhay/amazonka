{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCatalogImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a migration operation.
module Network.AWS.Glue.GetCatalogImportStatus
  ( -- * Creating a request
    GetCatalogImportStatus (..),
    mkGetCatalogImportStatus,

    -- ** Request lenses
    gcisCatalogId,

    -- * Destructuring the response
    GetCatalogImportStatusResponse (..),
    mkGetCatalogImportStatusResponse,

    -- ** Response lenses
    gcisrrsImportStatus,
    gcisrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCatalogImportStatus' smart constructor.
newtype GetCatalogImportStatus = GetCatalogImportStatus'
  { -- | The ID of the catalog to migrate. Currently, this should be the AWS account ID.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCatalogImportStatus' value with any optional fields omitted.
mkGetCatalogImportStatus ::
  GetCatalogImportStatus
mkGetCatalogImportStatus =
  GetCatalogImportStatus' {catalogId = Core.Nothing}

-- | The ID of the catalog to migrate. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisCatalogId :: Lens.Lens' GetCatalogImportStatus (Core.Maybe Types.CatalogId)
gcisCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gcisCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON GetCatalogImportStatus where
  toJSON GetCatalogImportStatus {..} =
    Core.object
      (Core.catMaybes [("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest GetCatalogImportStatus where
  type Rs GetCatalogImportStatus = GetCatalogImportStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetCatalogImportStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCatalogImportStatusResponse'
            Core.<$> (x Core..:? "ImportStatus") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCatalogImportStatusResponse' smart constructor.
data GetCatalogImportStatusResponse = GetCatalogImportStatusResponse'
  { -- | The status of the specified catalog migration.
    importStatus :: Core.Maybe Types.CatalogImportStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCatalogImportStatusResponse' value with any optional fields omitted.
mkGetCatalogImportStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCatalogImportStatusResponse
mkGetCatalogImportStatusResponse responseStatus =
  GetCatalogImportStatusResponse'
    { importStatus = Core.Nothing,
      responseStatus
    }

-- | The status of the specified catalog migration.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisrrsImportStatus :: Lens.Lens' GetCatalogImportStatusResponse (Core.Maybe Types.CatalogImportStatus)
gcisrrsImportStatus = Lens.field @"importStatus"
{-# DEPRECATED gcisrrsImportStatus "Use generic-lens or generic-optics with 'importStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisrrsResponseStatus :: Lens.Lens' GetCatalogImportStatusResponse Core.Int
gcisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
