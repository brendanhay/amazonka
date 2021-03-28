{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTable (..)
    , mkGetTable
    -- ** Request lenses
    , gtfDatabaseName
    , gtfName
    , gtfCatalogId

    -- * Destructuring the response
    , GetTableResponse (..)
    , mkGetTableResponse
    -- ** Response lenses
    , gtrlrsTable
    , gtrlrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTable' smart constructor.
data GetTable = GetTable'
  { databaseName :: Types.DatabaseName
    -- ^ The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
  , name :: Types.Name
    -- ^ The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTable' value with any optional fields omitted.
mkGetTable
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.Name -- ^ 'name'
    -> GetTable
mkGetTable databaseName name
  = GetTable'{databaseName, name, catalogId = Core.Nothing}

-- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfDatabaseName :: Lens.Lens' GetTable Types.DatabaseName
gtfDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gtfDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfName :: Lens.Lens' GetTable Types.Name
gtfName = Lens.field @"name"
{-# INLINEABLE gtfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtfCatalogId :: Lens.Lens' GetTable (Core.Maybe Types.CatalogId)
gtfCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gtfCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery GetTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTable where
        toHeaders GetTable{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetTable") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTable where
        toJSON GetTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("Name" Core..= name),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest GetTable where
        type Rs GetTable = GetTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTableResponse' Core.<$>
                   (x Core..:? "Table") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'
  { table :: Core.Maybe Types.Table
    -- ^ The @Table@ object that defines the specified table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTableResponse' value with any optional fields omitted.
mkGetTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTableResponse
mkGetTableResponse responseStatus
  = GetTableResponse'{table = Core.Nothing, responseStatus}

-- | The @Table@ object that defines the specified table.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrlrsTable :: Lens.Lens' GetTableResponse (Core.Maybe Types.Table)
gtrlrsTable = Lens.field @"table"
{-# INLINEABLE gtrlrsTable #-}
{-# DEPRECATED table "Use generic-lens or generic-optics with 'table' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrlrsResponseStatus :: Lens.Lens' GetTableResponse Core.Int
gtrlrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrlrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
