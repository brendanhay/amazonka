{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a table definition from the Data Catalog.
module Network.AWS.Glue.DeleteTable
    (
    -- * Creating a request
      DeleteTable (..)
    , mkDeleteTable
    -- ** Request lenses
    , dtDatabaseName
    , dtName
    , dtCatalogId

    -- * Destructuring the response
    , DeleteTableResponse (..)
    , mkDeleteTableResponse
    -- ** Response lenses
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTable' smart constructor.
data DeleteTable = DeleteTable'
  { databaseName :: Types.NameString
    -- ^ The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
  , name :: Types.NameString
    -- ^ The name of the table to be deleted. For Hive compatibility, this name is entirely lowercase.
  , catalogId :: Core.Maybe Types.CatalogIdString
    -- ^ The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTable' value with any optional fields omitted.
mkDeleteTable
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'name'
    -> DeleteTable
mkDeleteTable databaseName name
  = DeleteTable'{databaseName, name, catalogId = Core.Nothing}

-- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDatabaseName :: Lens.Lens' DeleteTable Types.NameString
dtDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE dtDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the table to be deleted. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtName :: Lens.Lens' DeleteTable Types.NameString
dtName = Lens.field @"name"
{-# INLINEABLE dtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtCatalogId :: Lens.Lens' DeleteTable (Core.Maybe Types.CatalogIdString)
dtCatalogId = Lens.field @"catalogId"
{-# INLINEABLE dtCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery DeleteTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTable where
        toHeaders DeleteTable{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteTable") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTable where
        toJSON DeleteTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("Name" Core..= name),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest DeleteTable where
        type Rs DeleteTable = DeleteTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteTableResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTableResponse' smart constructor.
newtype DeleteTableResponse = DeleteTableResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTableResponse' value with any optional fields omitted.
mkDeleteTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTableResponse
mkDeleteTableResponse responseStatus
  = DeleteTableResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DeleteTableResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
