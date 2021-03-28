{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchDeleteTableVersion (..)
    , mkBatchDeleteTableVersion
    -- ** Request lenses
    , bdtvDatabaseName
    , bdtvTableName
    , bdtvVersionIds
    , bdtvCatalogId

    -- * Destructuring the response
    , BatchDeleteTableVersionResponse (..)
    , mkBatchDeleteTableVersionResponse
    -- ** Response lenses
    , bdtvrrsErrors
    , bdtvrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDeleteTableVersion' smart constructor.
data BatchDeleteTableVersion = BatchDeleteTableVersion'
  { databaseName :: Types.DatabaseName
    -- ^ The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
  , tableName :: Types.TableName
    -- ^ The name of the table. For Hive compatibility, this name is entirely lowercase.
  , versionIds :: [Types.VersionString]
    -- ^ A list of the IDs of versions to be deleted. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteTableVersion' value with any optional fields omitted.
mkBatchDeleteTableVersion
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.TableName -- ^ 'tableName'
    -> BatchDeleteTableVersion
mkBatchDeleteTableVersion databaseName tableName
  = BatchDeleteTableVersion'{databaseName, tableName,
                             versionIds = Core.mempty, catalogId = Core.Nothing}

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvDatabaseName :: Lens.Lens' BatchDeleteTableVersion Types.DatabaseName
bdtvDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE bdtvDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvTableName :: Lens.Lens' BatchDeleteTableVersion Types.TableName
bdtvTableName = Lens.field @"tableName"
{-# INLINEABLE bdtvTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A list of the IDs of versions to be deleted. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvVersionIds :: Lens.Lens' BatchDeleteTableVersion [Types.VersionString]
bdtvVersionIds = Lens.field @"versionIds"
{-# INLINEABLE bdtvVersionIds #-}
{-# DEPRECATED versionIds "Use generic-lens or generic-optics with 'versionIds' instead"  #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvCatalogId :: Lens.Lens' BatchDeleteTableVersion (Core.Maybe Types.CatalogId)
bdtvCatalogId = Lens.field @"catalogId"
{-# INLINEABLE bdtvCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery BatchDeleteTableVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchDeleteTableVersion where
        toHeaders BatchDeleteTableVersion{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchDeleteTableVersion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchDeleteTableVersion where
        toJSON BatchDeleteTableVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  Core.Just ("VersionIds" Core..= versionIds),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest BatchDeleteTableVersion where
        type Rs BatchDeleteTableVersion = BatchDeleteTableVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDeleteTableVersionResponse' Core.<$>
                   (x Core..:? "Errors") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDeleteTableVersionResponse' smart constructor.
data BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse'
  { errors :: Core.Maybe [Types.TableVersionError]
    -- ^ A list of errors encountered while trying to delete the specified table versions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteTableVersionResponse' value with any optional fields omitted.
mkBatchDeleteTableVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDeleteTableVersionResponse
mkBatchDeleteTableVersionResponse responseStatus
  = BatchDeleteTableVersionResponse'{errors = Core.Nothing,
                                     responseStatus}

-- | A list of errors encountered while trying to delete the specified table versions.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvrrsErrors :: Lens.Lens' BatchDeleteTableVersionResponse (Core.Maybe [Types.TableVersionError])
bdtvrrsErrors = Lens.field @"errors"
{-# INLINEABLE bdtvrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtvrrsResponseStatus :: Lens.Lens' BatchDeleteTableVersionResponse Core.Int
bdtvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdtvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
