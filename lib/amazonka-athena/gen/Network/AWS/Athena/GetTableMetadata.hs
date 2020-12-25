{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetTableMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table metadata for the specified catalog, database, and table.
module Network.AWS.Athena.GetTableMetadata
  ( -- * Creating a request
    GetTableMetadata (..),
    mkGetTableMetadata,

    -- ** Request lenses
    gtmCatalogName,
    gtmDatabaseName,
    gtmTableName,

    -- * Destructuring the response
    GetTableMetadataResponse (..),
    mkGetTableMetadataResponse,

    -- ** Response lenses
    gtmrrsTableMetadata,
    gtmrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTableMetadata' smart constructor.
data GetTableMetadata = GetTableMetadata'
  { -- | The name of the data catalog that contains the database and table metadata to return.
    catalogName :: Types.CatalogName,
    -- | The name of the database that contains the table metadata to return.
    databaseName :: Types.DatabaseName,
    -- | The name of the table for which metadata is returned.
    tableName :: Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTableMetadata' value with any optional fields omitted.
mkGetTableMetadata ::
  -- | 'catalogName'
  Types.CatalogName ->
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  GetTableMetadata
mkGetTableMetadata catalogName databaseName tableName =
  GetTableMetadata' {catalogName, databaseName, tableName}

-- | The name of the data catalog that contains the database and table metadata to return.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmCatalogName :: Lens.Lens' GetTableMetadata Types.CatalogName
gtmCatalogName = Lens.field @"catalogName"
{-# DEPRECATED gtmCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | The name of the database that contains the table metadata to return.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmDatabaseName :: Lens.Lens' GetTableMetadata Types.DatabaseName
gtmDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gtmDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table for which metadata is returned.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmTableName :: Lens.Lens' GetTableMetadata Types.TableName
gtmTableName = Lens.field @"tableName"
{-# DEPRECATED gtmTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON GetTableMetadata where
  toJSON GetTableMetadata {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CatalogName" Core..= catalogName),
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.AWSRequest GetTableMetadata where
  type Rs GetTableMetadata = GetTableMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.GetTableMetadata")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableMetadataResponse'
            Core.<$> (x Core..:? "TableMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTableMetadataResponse' smart constructor.
data GetTableMetadataResponse = GetTableMetadataResponse'
  { -- | An object that contains table metadata.
    tableMetadata :: Core.Maybe Types.TableMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTableMetadataResponse' value with any optional fields omitted.
mkGetTableMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTableMetadataResponse
mkGetTableMetadataResponse responseStatus =
  GetTableMetadataResponse'
    { tableMetadata = Core.Nothing,
      responseStatus
    }

-- | An object that contains table metadata.
--
-- /Note:/ Consider using 'tableMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsTableMetadata :: Lens.Lens' GetTableMetadataResponse (Core.Maybe Types.TableMetadata)
gtmrrsTableMetadata = Lens.field @"tableMetadata"
{-# DEPRECATED gtmrrsTableMetadata "Use generic-lens or generic-optics with 'tableMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsResponseStatus :: Lens.Lens' GetTableMetadataResponse Core.Int
gtmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
