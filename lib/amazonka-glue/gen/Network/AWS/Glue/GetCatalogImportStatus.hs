{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCatalogImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a migration operation.
module Network.AWS.Glue.GetCatalogImportStatus
    (
    -- * Creating a request
      GetCatalogImportStatus (..)
    , mkGetCatalogImportStatus
    -- ** Request lenses
    , gcisCatalogId

    -- * Destructuring the response
    , GetCatalogImportStatusResponse (..)
    , mkGetCatalogImportStatusResponse
    -- ** Response lenses
    , gcisrrsImportStatus
    , gcisrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCatalogImportStatus' smart constructor.
newtype GetCatalogImportStatus = GetCatalogImportStatus'
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the catalog to migrate. Currently, this should be the AWS account ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCatalogImportStatus' value with any optional fields omitted.
mkGetCatalogImportStatus
    :: GetCatalogImportStatus
mkGetCatalogImportStatus
  = GetCatalogImportStatus'{catalogId = Core.Nothing}

-- | The ID of the catalog to migrate. Currently, this should be the AWS account ID.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisCatalogId :: Lens.Lens' GetCatalogImportStatus (Core.Maybe Types.CatalogId)
gcisCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gcisCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery GetCatalogImportStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCatalogImportStatus where
        toHeaders GetCatalogImportStatus{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetCatalogImportStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCatalogImportStatus where
        toJSON GetCatalogImportStatus{..}
          = Core.object
              (Core.catMaybes [("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest GetCatalogImportStatus where
        type Rs GetCatalogImportStatus = GetCatalogImportStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCatalogImportStatusResponse' Core.<$>
                   (x Core..:? "ImportStatus") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCatalogImportStatusResponse' smart constructor.
data GetCatalogImportStatusResponse = GetCatalogImportStatusResponse'
  { importStatus :: Core.Maybe Types.CatalogImportStatus
    -- ^ The status of the specified catalog migration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCatalogImportStatusResponse' value with any optional fields omitted.
mkGetCatalogImportStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCatalogImportStatusResponse
mkGetCatalogImportStatusResponse responseStatus
  = GetCatalogImportStatusResponse'{importStatus = Core.Nothing,
                                    responseStatus}

-- | The status of the specified catalog migration.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisrrsImportStatus :: Lens.Lens' GetCatalogImportStatusResponse (Core.Maybe Types.CatalogImportStatus)
gcisrrsImportStatus = Lens.field @"importStatus"
{-# INLINEABLE gcisrrsImportStatus #-}
{-# DEPRECATED importStatus "Use generic-lens or generic-optics with 'importStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcisrrsResponseStatus :: Lens.Lens' GetCatalogImportStatusResponse Core.Int
gcisrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcisrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
