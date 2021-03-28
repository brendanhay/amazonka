{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTableMetadata (..)
    , mkGetTableMetadata
    -- ** Request lenses
    , gtmCatalogName
    , gtmDatabaseName
    , gtmTableName

    -- * Destructuring the response
    , GetTableMetadataResponse (..)
    , mkGetTableMetadataResponse
    -- ** Response lenses
    , gtmrrsTableMetadata
    , gtmrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTableMetadata' smart constructor.
data GetTableMetadata = GetTableMetadata'
  { catalogName :: Types.CatalogName
    -- ^ The name of the data catalog that contains the database and table metadata to return.
  , databaseName :: Types.DatabaseName
    -- ^ The name of the database that contains the table metadata to return.
  , tableName :: Types.TableName
    -- ^ The name of the table for which metadata is returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTableMetadata' value with any optional fields omitted.
mkGetTableMetadata
    :: Types.CatalogName -- ^ 'catalogName'
    -> Types.DatabaseName -- ^ 'databaseName'
    -> Types.TableName -- ^ 'tableName'
    -> GetTableMetadata
mkGetTableMetadata catalogName databaseName tableName
  = GetTableMetadata'{catalogName, databaseName, tableName}

-- | The name of the data catalog that contains the database and table metadata to return.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmCatalogName :: Lens.Lens' GetTableMetadata Types.CatalogName
gtmCatalogName = Lens.field @"catalogName"
{-# INLINEABLE gtmCatalogName #-}
{-# DEPRECATED catalogName "Use generic-lens or generic-optics with 'catalogName' instead"  #-}

-- | The name of the database that contains the table metadata to return.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmDatabaseName :: Lens.Lens' GetTableMetadata Types.DatabaseName
gtmDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gtmDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table for which metadata is returned.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmTableName :: Lens.Lens' GetTableMetadata Types.TableName
gtmTableName = Lens.field @"tableName"
{-# INLINEABLE gtmTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.ToQuery GetTableMetadata where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTableMetadata where
        toHeaders GetTableMetadata{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.GetTableMetadata")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTableMetadata where
        toJSON GetTableMetadata{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CatalogName" Core..= catalogName),
                  Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName)])

instance Core.AWSRequest GetTableMetadata where
        type Rs GetTableMetadata = GetTableMetadataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTableMetadataResponse' Core.<$>
                   (x Core..:? "TableMetadata") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTableMetadataResponse' smart constructor.
data GetTableMetadataResponse = GetTableMetadataResponse'
  { tableMetadata :: Core.Maybe Types.TableMetadata
    -- ^ An object that contains table metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTableMetadataResponse' value with any optional fields omitted.
mkGetTableMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTableMetadataResponse
mkGetTableMetadataResponse responseStatus
  = GetTableMetadataResponse'{tableMetadata = Core.Nothing,
                              responseStatus}

-- | An object that contains table metadata.
--
-- /Note:/ Consider using 'tableMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsTableMetadata :: Lens.Lens' GetTableMetadataResponse (Core.Maybe Types.TableMetadata)
gtmrrsTableMetadata = Lens.field @"tableMetadata"
{-# INLINEABLE gtmrrsTableMetadata #-}
{-# DEPRECATED tableMetadata "Use generic-lens or generic-optics with 'tableMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrrsResponseStatus :: Lens.Lens' GetTableMetadataResponse Core.Int
gtmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
