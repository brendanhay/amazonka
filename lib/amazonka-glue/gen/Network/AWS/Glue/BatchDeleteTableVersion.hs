{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeleteTableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified batch of versions of a table.
module Network.AWS.Glue.BatchDeleteTableVersion
  ( -- * Creating a request
    BatchDeleteTableVersion (..),
    mkBatchDeleteTableVersion,

    -- ** Request lenses
    bdtvDatabaseName,
    bdtvTableName,
    bdtvVersionIds,
    bdtvCatalogId,

    -- * Destructuring the response
    BatchDeleteTableVersionResponse (..),
    mkBatchDeleteTableVersionResponse,

    -- ** Response lenses
    bdtvrrsErrors,
    bdtvrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteTableVersion' smart constructor.
data BatchDeleteTableVersion = BatchDeleteTableVersion'
  { -- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
    databaseName :: Types.DatabaseName,
    -- | The name of the table. For Hive compatibility, this name is entirely lowercase.
    tableName :: Types.TableName,
    -- | A list of the IDs of versions to be deleted. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
    versionIds :: [Types.VersionString],
    -- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteTableVersion' value with any optional fields omitted.
mkBatchDeleteTableVersion ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  BatchDeleteTableVersion
mkBatchDeleteTableVersion databaseName tableName =
  BatchDeleteTableVersion'
    { databaseName,
      tableName,
      versionIds = Core.mempty,
      catalogId = Core.Nothing
    }

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvDatabaseName :: Lens.Lens' BatchDeleteTableVersion Types.DatabaseName
bdtvDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED bdtvDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvTableName :: Lens.Lens' BatchDeleteTableVersion Types.TableName
bdtvTableName = Lens.field @"tableName"
{-# DEPRECATED bdtvTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A list of the IDs of versions to be deleted. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvVersionIds :: Lens.Lens' BatchDeleteTableVersion [Types.VersionString]
bdtvVersionIds = Lens.field @"versionIds"
{-# DEPRECATED bdtvVersionIds "Use generic-lens or generic-optics with 'versionIds' instead." #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvCatalogId :: Lens.Lens' BatchDeleteTableVersion (Core.Maybe Types.CatalogId)
bdtvCatalogId = Lens.field @"catalogId"
{-# DEPRECATED bdtvCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON BatchDeleteTableVersion where
  toJSON BatchDeleteTableVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("VersionIds" Core..= versionIds),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest BatchDeleteTableVersion where
  type Rs BatchDeleteTableVersion = BatchDeleteTableVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.BatchDeleteTableVersion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableVersionResponse'
            Core.<$> (x Core..:? "Errors") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDeleteTableVersionResponse' smart constructor.
data BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse'
  { -- | A list of errors encountered while trying to delete the specified table versions.
    errors :: Core.Maybe [Types.TableVersionError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteTableVersionResponse' value with any optional fields omitted.
mkBatchDeleteTableVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDeleteTableVersionResponse
mkBatchDeleteTableVersionResponse responseStatus =
  BatchDeleteTableVersionResponse'
    { errors = Core.Nothing,
      responseStatus
    }

-- | A list of errors encountered while trying to delete the specified table versions.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvrrsErrors :: Lens.Lens' BatchDeleteTableVersionResponse (Core.Maybe [Types.TableVersionError])
bdtvrrsErrors = Lens.field @"errors"
{-# DEPRECATED bdtvrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvrrsResponseStatus :: Lens.Lens' BatchDeleteTableVersionResponse Core.Int
bdtvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bdtvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
