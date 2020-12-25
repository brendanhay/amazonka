{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.DeleteDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data catalog.
module Network.AWS.Athena.DeleteDataCatalog
  ( -- * Creating a request
    DeleteDataCatalog (..),
    mkDeleteDataCatalog,

    -- ** Request lenses
    ddcName,

    -- * Destructuring the response
    DeleteDataCatalogResponse (..),
    mkDeleteDataCatalogResponse,

    -- ** Response lenses
    ddcrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDataCatalog' smart constructor.
newtype DeleteDataCatalog = DeleteDataCatalog'
  { -- | The name of the data catalog to delete.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataCatalog' value with any optional fields omitted.
mkDeleteDataCatalog ::
  -- | 'name'
  Types.Name ->
  DeleteDataCatalog
mkDeleteDataCatalog name = DeleteDataCatalog' {name}

-- | The name of the data catalog to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcName :: Lens.Lens' DeleteDataCatalog Types.Name
ddcName = Lens.field @"name"
{-# DEPRECATED ddcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteDataCatalog where
  toJSON DeleteDataCatalog {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteDataCatalog where
  type Rs DeleteDataCatalog = DeleteDataCatalogResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.DeleteDataCatalog")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataCatalogResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDataCatalogResponse' smart constructor.
newtype DeleteDataCatalogResponse = DeleteDataCatalogResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataCatalogResponse' value with any optional fields omitted.
mkDeleteDataCatalogResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDataCatalogResponse
mkDeleteDataCatalogResponse responseStatus =
  DeleteDataCatalogResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsResponseStatus :: Lens.Lens' DeleteDataCatalogResponse Core.Int
ddcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
