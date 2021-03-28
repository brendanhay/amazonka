{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ImportCatalogToGlue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing Amazon Athena Data Catalog to AWS Glue
module Network.AWS.Glue.ImportCatalogToGlue
    (
    -- * Creating a request
      ImportCatalogToGlue (..)
    , mkImportCatalogToGlue
    -- ** Request lenses
    , ictgCatalogId

    -- * Destructuring the response
    , ImportCatalogToGlueResponse (..)
    , mkImportCatalogToGlueResponse
    -- ** Response lenses
    , ictgrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportCatalogToGlue' smart constructor.
newtype ImportCatalogToGlue = ImportCatalogToGlue'
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the catalog to import. Currently, this should be the AWS account ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCatalogToGlue' value with any optional fields omitted.
mkImportCatalogToGlue
    :: ImportCatalogToGlue
mkImportCatalogToGlue
  = ImportCatalogToGlue'{catalogId = Core.Nothing}

-- | The ID of the catalog to import. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ictgCatalogId :: Lens.Lens' ImportCatalogToGlue (Core.Maybe Types.CatalogId)
ictgCatalogId = Lens.field @"catalogId"
{-# INLINEABLE ictgCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery ImportCatalogToGlue where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportCatalogToGlue where
        toHeaders ImportCatalogToGlue{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ImportCatalogToGlue") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportCatalogToGlue where
        toJSON ImportCatalogToGlue{..}
          = Core.object
              (Core.catMaybes [("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest ImportCatalogToGlue where
        type Rs ImportCatalogToGlue = ImportCatalogToGlueResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ImportCatalogToGlueResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportCatalogToGlueResponse' smart constructor.
newtype ImportCatalogToGlueResponse = ImportCatalogToGlueResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCatalogToGlueResponse' value with any optional fields omitted.
mkImportCatalogToGlueResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportCatalogToGlueResponse
mkImportCatalogToGlueResponse responseStatus
  = ImportCatalogToGlueResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ictgrrsResponseStatus :: Lens.Lens' ImportCatalogToGlueResponse Core.Int
ictgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ictgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
