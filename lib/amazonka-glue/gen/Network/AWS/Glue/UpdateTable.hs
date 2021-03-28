{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a metadata table in the Data Catalog.
module Network.AWS.Glue.UpdateTable
    (
    -- * Creating a request
      UpdateTable (..)
    , mkUpdateTable
    -- ** Request lenses
    , utDatabaseName
    , utTableInput
    , utCatalogId
    , utSkipArchive

    -- * Destructuring the response
    , UpdateTableResponse (..)
    , mkUpdateTableResponse
    -- ** Response lenses
    , utrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { databaseName :: Types.NameString
    -- ^ The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
  , tableInput :: Types.TableInput
    -- ^ An updated @TableInput@ object to define the metadata table in the catalog.
  , catalogId :: Core.Maybe Types.CatalogIdString
    -- ^ The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
  , skipArchive :: Core.Maybe Core.Bool
    -- ^ By default, @UpdateTable@ always creates an archived version of the table before updating it. However, if @skipArchive@ is set to true, @UpdateTable@ does not create the archived version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateTable' value with any optional fields omitted.
mkUpdateTable
    :: Types.NameString -- ^ 'databaseName'
    -> Types.TableInput -- ^ 'tableInput'
    -> UpdateTable
mkUpdateTable databaseName tableInput
  = UpdateTable'{databaseName, tableInput, catalogId = Core.Nothing,
                 skipArchive = Core.Nothing}

-- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDatabaseName :: Lens.Lens' UpdateTable Types.NameString
utDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE utDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | An updated @TableInput@ object to define the metadata table in the catalog.
--
-- /Note:/ Consider using 'tableInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTableInput :: Lens.Lens' UpdateTable Types.TableInput
utTableInput = Lens.field @"tableInput"
{-# INLINEABLE utTableInput #-}
{-# DEPRECATED tableInput "Use generic-lens or generic-optics with 'tableInput' instead"  #-}

-- | The ID of the Data Catalog where the table resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utCatalogId :: Lens.Lens' UpdateTable (Core.Maybe Types.CatalogIdString)
utCatalogId = Lens.field @"catalogId"
{-# INLINEABLE utCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | By default, @UpdateTable@ always creates an archived version of the table before updating it. However, if @skipArchive@ is set to true, @UpdateTable@ does not create the archived version.
--
-- /Note:/ Consider using 'skipArchive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSkipArchive :: Lens.Lens' UpdateTable (Core.Maybe Core.Bool)
utSkipArchive = Lens.field @"skipArchive"
{-# INLINEABLE utSkipArchive #-}
{-# DEPRECATED skipArchive "Use generic-lens or generic-optics with 'skipArchive' instead"  #-}

instance Core.ToQuery UpdateTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTable where
        toHeaders UpdateTable{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdateTable") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTable where
        toJSON UpdateTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableInput" Core..= tableInput),
                  ("CatalogId" Core..=) Core.<$> catalogId,
                  ("SkipArchive" Core..=) Core.<$> skipArchive])

instance Core.AWSRequest UpdateTable where
        type Rs UpdateTable = UpdateTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateTableResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTableResponse' smart constructor.
newtype UpdateTableResponse = UpdateTableResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTableResponse' value with any optional fields omitted.
mkUpdateTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTableResponse
mkUpdateTableResponse responseStatus
  = UpdateTableResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTableResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
