{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified database from a Data Catalog.
module Network.AWS.Glue.DeleteDatabase
    (
    -- * Creating a request
      DeleteDatabase (..)
    , mkDeleteDatabase
    -- ** Request lenses
    , ddName
    , ddCatalogId

    -- * Destructuring the response
    , DeleteDatabaseResponse (..)
    , mkDeleteDatabaseResponse
    -- ** Response lenses
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDatabase' smart constructor.
data DeleteDatabase = DeleteDatabase'
  { name :: Types.Name
    -- ^ The name of the database to delete. For Hive compatibility, this must be all lowercase.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDatabase' value with any optional fields omitted.
mkDeleteDatabase
    :: Types.Name -- ^ 'name'
    -> DeleteDatabase
mkDeleteDatabase name
  = DeleteDatabase'{name, catalogId = Core.Nothing}

-- | The name of the database to delete. For Hive compatibility, this must be all lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DeleteDatabase Types.Name
ddName = Lens.field @"name"
{-# INLINEABLE ddName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the Data Catalog in which the database resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCatalogId :: Lens.Lens' DeleteDatabase (Core.Maybe Types.CatalogId)
ddCatalogId = Lens.field @"catalogId"
{-# INLINEABLE ddCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery DeleteDatabase where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDatabase where
        toHeaders DeleteDatabase{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteDatabase") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDatabase where
        toJSON DeleteDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest DeleteDatabase where
        type Rs DeleteDatabase = DeleteDatabaseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDatabaseResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDatabaseResponse' smart constructor.
newtype DeleteDatabaseResponse = DeleteDatabaseResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDatabaseResponse' value with any optional fields omitted.
mkDeleteDatabaseResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDatabaseResponse
mkDeleteDatabaseResponse responseStatus
  = DeleteDatabaseResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDatabaseResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
