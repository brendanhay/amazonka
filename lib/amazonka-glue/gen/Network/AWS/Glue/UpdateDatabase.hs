{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing database definition in a Data Catalog.
module Network.AWS.Glue.UpdateDatabase
    (
    -- * Creating a request
      UpdateDatabase (..)
    , mkUpdateDatabase
    -- ** Request lenses
    , udName
    , udDatabaseInput
    , udCatalogId

    -- * Destructuring the response
    , UpdateDatabaseResponse (..)
    , mkUpdateDatabaseResponse
    -- ** Response lenses
    , udrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDatabase' smart constructor.
data UpdateDatabase = UpdateDatabase'
  { name :: Types.Name
    -- ^ The name of the database to update in the catalog. For Hive compatibility, this is folded to lowercase.
  , databaseInput :: Types.DatabaseInput
    -- ^ A @DatabaseInput@ object specifying the new definition of the metadata database in the catalog.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog in which the metadata database resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDatabase' value with any optional fields omitted.
mkUpdateDatabase
    :: Types.Name -- ^ 'name'
    -> Types.DatabaseInput -- ^ 'databaseInput'
    -> UpdateDatabase
mkUpdateDatabase name databaseInput
  = UpdateDatabase'{name, databaseInput, catalogId = Core.Nothing}

-- | The name of the database to update in the catalog. For Hive compatibility, this is folded to lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDatabase Types.Name
udName = Lens.field @"name"
{-# INLINEABLE udName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A @DatabaseInput@ object specifying the new definition of the metadata database in the catalog.
--
-- /Note:/ Consider using 'databaseInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDatabaseInput :: Lens.Lens' UpdateDatabase Types.DatabaseInput
udDatabaseInput = Lens.field @"databaseInput"
{-# INLINEABLE udDatabaseInput #-}
{-# DEPRECATED databaseInput "Use generic-lens or generic-optics with 'databaseInput' instead"  #-}

-- | The ID of the Data Catalog in which the metadata database resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCatalogId :: Lens.Lens' UpdateDatabase (Core.Maybe Types.CatalogId)
udCatalogId = Lens.field @"catalogId"
{-# INLINEABLE udCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery UpdateDatabase where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDatabase where
        toHeaders UpdateDatabase{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdateDatabase") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDatabase where
        toJSON UpdateDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("DatabaseInput" Core..= databaseInput),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest UpdateDatabase where
        type Rs UpdateDatabase = UpdateDatabaseResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateDatabaseResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDatabaseResponse' smart constructor.
newtype UpdateDatabaseResponse = UpdateDatabaseResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDatabaseResponse' value with any optional fields omitted.
mkUpdateDatabaseResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDatabaseResponse
mkUpdateDatabaseResponse responseStatus
  = UpdateDatabaseResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDatabaseResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
