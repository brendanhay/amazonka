{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreatePartition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new partition.
module Network.AWS.Glue.CreatePartition
  ( -- * Creating a request
    CreatePartition (..),
    mkCreatePartition,

    -- ** Request lenses
    cpDatabaseName,
    cpTableName,
    cpPartitionInput,
    cpCatalogId,

    -- * Destructuring the response
    CreatePartitionResponse (..),
    mkCreatePartitionResponse,

    -- ** Response lenses
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePartition' smart constructor.
data CreatePartition = CreatePartition'
  { -- | The name of the metadata database in which the partition is to be created.
    databaseName :: Types.DatabaseName,
    -- | The name of the metadata table in which the partition is to be created.
    tableName :: Types.TableName,
    -- | A @PartitionInput@ structure defining the partition to be created.
    partitionInput :: Types.PartitionInput,
    -- | The AWS account ID of the catalog in which the partition is to be created.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePartition' value with any optional fields omitted.
mkCreatePartition ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  -- | 'partitionInput'
  Types.PartitionInput ->
  CreatePartition
mkCreatePartition databaseName tableName partitionInput =
  CreatePartition'
    { databaseName,
      tableName,
      partitionInput,
      catalogId = Core.Nothing
    }

-- | The name of the metadata database in which the partition is to be created.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDatabaseName :: Lens.Lens' CreatePartition Types.DatabaseName
cpDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED cpDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the metadata table in which the partition is to be created.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTableName :: Lens.Lens' CreatePartition Types.TableName
cpTableName = Lens.field @"tableName"
{-# DEPRECATED cpTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A @PartitionInput@ structure defining the partition to be created.
--
-- /Note:/ Consider using 'partitionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPartitionInput :: Lens.Lens' CreatePartition Types.PartitionInput
cpPartitionInput = Lens.field @"partitionInput"
{-# DEPRECATED cpPartitionInput "Use generic-lens or generic-optics with 'partitionInput' instead." #-}

-- | The AWS account ID of the catalog in which the partition is to be created.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCatalogId :: Lens.Lens' CreatePartition (Core.Maybe Types.CatalogId)
cpCatalogId = Lens.field @"catalogId"
{-# DEPRECATED cpCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON CreatePartition where
  toJSON CreatePartition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("PartitionInput" Core..= partitionInput),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest CreatePartition where
  type Rs CreatePartition = CreatePartitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreatePartition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreatePartitionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePartitionResponse' smart constructor.
newtype CreatePartitionResponse = CreatePartitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePartitionResponse' value with any optional fields omitted.
mkCreatePartitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePartitionResponse
mkCreatePartitionResponse responseStatus =
  CreatePartitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePartitionResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
