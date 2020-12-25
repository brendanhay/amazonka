{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ImportCatalogToGlue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing Amazon Athena Data Catalog to AWS Glue
module Network.AWS.Glue.ImportCatalogToGlue
  ( -- * Creating a request
    ImportCatalogToGlue (..),
    mkImportCatalogToGlue,

    -- ** Request lenses
    ictgCatalogId,

    -- * Destructuring the response
    ImportCatalogToGlueResponse (..),
    mkImportCatalogToGlueResponse,

    -- ** Response lenses
    ictgrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportCatalogToGlue' smart constructor.
newtype ImportCatalogToGlue = ImportCatalogToGlue'
  { -- | The ID of the catalog to import. Currently, this should be the AWS account ID.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCatalogToGlue' value with any optional fields omitted.
mkImportCatalogToGlue ::
  ImportCatalogToGlue
mkImportCatalogToGlue =
  ImportCatalogToGlue' {catalogId = Core.Nothing}

-- | The ID of the catalog to import. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ictgCatalogId :: Lens.Lens' ImportCatalogToGlue (Core.Maybe Types.CatalogId)
ictgCatalogId = Lens.field @"catalogId"
{-# DEPRECATED ictgCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON ImportCatalogToGlue where
  toJSON ImportCatalogToGlue {..} =
    Core.object
      (Core.catMaybes [("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest ImportCatalogToGlue where
  type Rs ImportCatalogToGlue = ImportCatalogToGlueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.ImportCatalogToGlue")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportCatalogToGlueResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportCatalogToGlueResponse' smart constructor.
newtype ImportCatalogToGlueResponse = ImportCatalogToGlueResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCatalogToGlueResponse' value with any optional fields omitted.
mkImportCatalogToGlueResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportCatalogToGlueResponse
mkImportCatalogToGlueResponse responseStatus =
  ImportCatalogToGlueResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ictgrrsResponseStatus :: Lens.Lens' ImportCatalogToGlueResponse Core.Int
ictgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ictgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
