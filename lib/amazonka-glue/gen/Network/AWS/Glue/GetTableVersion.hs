{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified version of a table.
module Network.AWS.Glue.GetTableVersion
    (
    -- * Creating a request
      GetTableVersion (..)
    , mkGetTableVersion
    -- ** Request lenses
    , gtvDatabaseName
    , gtvTableName
    , gtvCatalogId
    , gtvVersionId

    -- * Destructuring the response
    , GetTableVersionResponse (..)
    , mkGetTableVersionResponse
    -- ** Response lenses
    , gtvrrsTableVersion
    , gtvrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTableVersion' smart constructor.
data GetTableVersion = GetTableVersion'
  { databaseName :: Types.NameString
    -- ^ The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
  , tableName :: Types.NameString
    -- ^ The name of the table. For Hive compatibility, this name is entirely lowercase.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
  , versionId :: Core.Maybe Types.VersionString
    -- ^ The ID value of the table version to be retrieved. A @VersionID@ is a string representation of an integer. Each version is incremented by 1. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTableVersion' value with any optional fields omitted.
mkGetTableVersion
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> GetTableVersion
mkGetTableVersion databaseName tableName
  = GetTableVersion'{databaseName, tableName,
                     catalogId = Core.Nothing, versionId = Core.Nothing}

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvDatabaseName :: Lens.Lens' GetTableVersion Types.NameString
gtvDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gtvDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvTableName :: Lens.Lens' GetTableVersion Types.NameString
gtvTableName = Lens.field @"tableName"
{-# INLINEABLE gtvTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvCatalogId :: Lens.Lens' GetTableVersion (Core.Maybe Types.CatalogId)
gtvCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gtvCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | The ID value of the table version to be retrieved. A @VersionID@ is a string representation of an integer. Each version is incremented by 1. 
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvVersionId :: Lens.Lens' GetTableVersion (Core.Maybe Types.VersionString)
gtvVersionId = Lens.field @"versionId"
{-# INLINEABLE gtvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery GetTableVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTableVersion where
        toHeaders GetTableVersion{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetTableVersion") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTableVersion where
        toJSON GetTableVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  ("CatalogId" Core..=) Core.<$> catalogId,
                  ("VersionId" Core..=) Core.<$> versionId])

instance Core.AWSRequest GetTableVersion where
        type Rs GetTableVersion = GetTableVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTableVersionResponse' Core.<$>
                   (x Core..:? "TableVersion") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTableVersionResponse' smart constructor.
data GetTableVersionResponse = GetTableVersionResponse'
  { tableVersion :: Core.Maybe Types.TableVersion
    -- ^ The requested table version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTableVersionResponse' value with any optional fields omitted.
mkGetTableVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTableVersionResponse
mkGetTableVersionResponse responseStatus
  = GetTableVersionResponse'{tableVersion = Core.Nothing,
                             responseStatus}

-- | The requested table version.
--
-- /Note:/ Consider using 'tableVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrrsTableVersion :: Lens.Lens' GetTableVersionResponse (Core.Maybe Types.TableVersion)
gtvrrsTableVersion = Lens.field @"tableVersion"
{-# INLINEABLE gtvrrsTableVersion #-}
{-# DEPRECATED tableVersion "Use generic-lens or generic-optics with 'tableVersion' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrrsResponseStatus :: Lens.Lens' GetTableVersionResponse Core.Int
gtvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
