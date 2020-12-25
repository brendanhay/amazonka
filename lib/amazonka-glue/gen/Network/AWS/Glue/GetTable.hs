{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @Table@ definition in a Data Catalog for a specified table.
module Network.AWS.Glue.GetTable
  ( -- * Creating a request
    GetTable (..),
    mkGetTable,

    -- ** Request lenses
    gtfDatabaseName,
    gtfName,
    gtfCatalogId,

    -- * Destructuring the response
    GetTableResponse (..),
    mkGetTableResponse,

    -- ** Response lenses
    gtrlrsTable,
    gtrlrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTable' smart constructor.
data GetTable = GetTable'
  { -- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Types.DatabaseName,
    -- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
    name :: Types.Name,
    -- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTable' value with any optional fields omitted.
mkGetTable ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'name'
  Types.Name ->
  GetTable
mkGetTable databaseName name =
  GetTable' {databaseName, name, catalogId = Core.Nothing}

-- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfDatabaseName :: Lens.Lens' GetTable Types.DatabaseName
gtfDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gtfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfName :: Lens.Lens' GetTable Types.Name
gtfName = Lens.field @"name"
{-# DEPRECATED gtfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfCatalogId :: Lens.Lens' GetTable (Core.Maybe Types.CatalogId)
gtfCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gtfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON GetTable where
  toJSON GetTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("Name" Core..= name),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest GetTable where
  type Rs GetTable = GetTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableResponse'
            Core.<$> (x Core..:? "Table") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'
  { -- | The @Table@ object that defines the specified table.
    table :: Core.Maybe Types.Table,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTableResponse' value with any optional fields omitted.
mkGetTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTableResponse
mkGetTableResponse responseStatus =
  GetTableResponse' {table = Core.Nothing, responseStatus}

-- | The @Table@ object that defines the specified table.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrlrsTable :: Lens.Lens' GetTableResponse (Core.Maybe Types.Table)
gtrlrsTable = Lens.field @"table"
{-# DEPRECATED gtrlrsTable "Use generic-lens or generic-optics with 'table' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrlrsResponseStatus :: Lens.Lens' GetTableResponse Core.Int
gtrlrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
