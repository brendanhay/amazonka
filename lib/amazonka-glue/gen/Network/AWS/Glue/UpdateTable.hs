{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a metadata table in the Data Catalog.
module Network.AWS.Glue.UpdateTable
  ( -- * Creating a request
    UpdateTable (..),
    mkUpdateTable,

    -- ** Request lenses
    utDatabaseName,
    utTableInput,
    utCatalogId,
    utSkipArchive,

    -- * Destructuring the response
    UpdateTableResponse (..),
    mkUpdateTableResponse,

    -- ** Response lenses
    utrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Types.NameString,
    -- | An updated @TableInput@ object to define the metadata table in the catalog.
    tableInput :: Types.TableInput,
    -- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogIdString,
    -- | By default, @UpdateTable@ always creates an archived version of the table before updating it. However, if @skipArchive@ is set to true, @UpdateTable@ does not create the archived version.
    skipArchive :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateTable' value with any optional fields omitted.
mkUpdateTable ::
  -- | 'databaseName'
  Types.NameString ->
  -- | 'tableInput'
  Types.TableInput ->
  UpdateTable
mkUpdateTable databaseName tableInput =
  UpdateTable'
    { databaseName,
      tableInput,
      catalogId = Core.Nothing,
      skipArchive = Core.Nothing
    }

-- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDatabaseName :: Lens.Lens' UpdateTable Types.NameString
utDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED utDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | An updated @TableInput@ object to define the metadata table in the catalog.
--
-- /Note:/ Consider using 'tableInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTableInput :: Lens.Lens' UpdateTable Types.TableInput
utTableInput = Lens.field @"tableInput"
{-# DEPRECATED utTableInput "Use generic-lens or generic-optics with 'tableInput' instead." #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utCatalogId :: Lens.Lens' UpdateTable (Core.Maybe Types.CatalogIdString)
utCatalogId = Lens.field @"catalogId"
{-# DEPRECATED utCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | By default, @UpdateTable@ always creates an archived version of the table before updating it. However, if @skipArchive@ is set to true, @UpdateTable@ does not create the archived version.
--
-- /Note:/ Consider using 'skipArchive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSkipArchive :: Lens.Lens' UpdateTable (Core.Maybe Core.Bool)
utSkipArchive = Lens.field @"skipArchive"
{-# DEPRECATED utSkipArchive "Use generic-lens or generic-optics with 'skipArchive' instead." #-}

instance Core.FromJSON UpdateTable where
  toJSON UpdateTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableInput" Core..= tableInput),
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("SkipArchive" Core..=) Core.<$> skipArchive
          ]
      )

instance Core.AWSRequest UpdateTable where
  type Rs UpdateTable = UpdateTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTableResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTableResponse' smart constructor.
newtype UpdateTableResponse = UpdateTableResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTableResponse' value with any optional fields omitted.
mkUpdateTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTableResponse
mkUpdateTableResponse responseStatus =
  UpdateTableResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTableResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
