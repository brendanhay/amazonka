{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table definition in the Data Catalog.
module Network.AWS.Glue.CreateTable
  ( -- * Creating a request
    CreateTable (..),
    mkCreateTable,

    -- ** Request lenses
    cDatabaseName,
    cTableInput,
    cCatalogId,
    cPartitionIndexes,

    -- * Destructuring the response
    CreateTableResponse (..),
    mkCreateTableResponse,

    -- ** Response lenses
    ctrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | The catalog database in which to create the new table. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Types.DatabaseName,
    -- | The @TableInput@ object that defines the metadata table to create in the catalog.
    tableInput :: Types.TableInput,
    -- | The ID of the Data Catalog in which to create the @Table@ . If none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId,
    -- | A list of partition indexes, @PartitionIndex@ structures, to create in the table.
    partitionIndexes :: Core.Maybe [Types.PartitionIndex]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTable' value with any optional fields omitted.
mkCreateTable ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableInput'
  Types.TableInput ->
  CreateTable
mkCreateTable databaseName tableInput =
  CreateTable'
    { databaseName,
      tableInput,
      catalogId = Core.Nothing,
      partitionIndexes = Core.Nothing
    }

-- | The catalog database in which to create the new table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDatabaseName :: Lens.Lens' CreateTable Types.DatabaseName
cDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED cDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The @TableInput@ object that defines the metadata table to create in the catalog.
--
-- /Note:/ Consider using 'tableInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTableInput :: Lens.Lens' CreateTable Types.TableInput
cTableInput = Lens.field @"tableInput"
{-# DEPRECATED cTableInput "Use generic-lens or generic-optics with 'tableInput' instead." #-}

-- | The ID of the Data Catalog in which to create the @Table@ . If none is supplied, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCatalogId :: Lens.Lens' CreateTable (Core.Maybe Types.CatalogId)
cCatalogId = Lens.field @"catalogId"
{-# DEPRECATED cCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A list of partition indexes, @PartitionIndex@ structures, to create in the table.
--
-- /Note:/ Consider using 'partitionIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPartitionIndexes :: Lens.Lens' CreateTable (Core.Maybe [Types.PartitionIndex])
cPartitionIndexes = Lens.field @"partitionIndexes"
{-# DEPRECATED cPartitionIndexes "Use generic-lens or generic-optics with 'partitionIndexes' instead." #-}

instance Core.FromJSON CreateTable where
  toJSON CreateTable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableInput" Core..= tableInput),
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("PartitionIndexes" Core..=) Core.<$> partitionIndexes
          ]
      )

instance Core.AWSRequest CreateTable where
  type Rs CreateTable = CreateTableResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateTable")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTableResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTableResponse' smart constructor.
newtype CreateTableResponse = CreateTableResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTableResponse' value with any optional fields omitted.
mkCreateTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTableResponse
mkCreateTableResponse responseStatus =
  CreateTableResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrfrsResponseStatus :: Lens.Lens' CreateTableResponse Core.Int
ctrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
