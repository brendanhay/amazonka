{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified data catalog.
module Network.AWS.Athena.GetDataCatalog
    (
    -- * Creating a request
      GetDataCatalog (..)
    , mkGetDataCatalog
    -- ** Request lenses
    , gdcName

    -- * Destructuring the response
    , GetDataCatalogResponse (..)
    , mkGetDataCatalogResponse
    -- ** Response lenses
    , gdcrrsDataCatalog
    , gdcrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDataCatalog' smart constructor.
newtype GetDataCatalog = GetDataCatalog'
  { name :: Types.Name
    -- ^ The name of the data catalog to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataCatalog' value with any optional fields omitted.
mkGetDataCatalog
    :: Types.Name -- ^ 'name'
    -> GetDataCatalog
mkGetDataCatalog name = GetDataCatalog'{name}

-- | The name of the data catalog to return.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcName :: Lens.Lens' GetDataCatalog Types.Name
gdcName = Lens.field @"name"
{-# INLINEABLE gdcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery GetDataCatalog where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDataCatalog where
        toHeaders GetDataCatalog{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.GetDataCatalog") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDataCatalog where
        toJSON GetDataCatalog{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetDataCatalog where
        type Rs GetDataCatalog = GetDataCatalogResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDataCatalogResponse' Core.<$>
                   (x Core..:? "DataCatalog") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDataCatalogResponse' smart constructor.
data GetDataCatalogResponse = GetDataCatalogResponse'
  { dataCatalog :: Core.Maybe Types.DataCatalog
    -- ^ The data catalog returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataCatalogResponse' value with any optional fields omitted.
mkGetDataCatalogResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDataCatalogResponse
mkGetDataCatalogResponse responseStatus
  = GetDataCatalogResponse'{dataCatalog = Core.Nothing,
                            responseStatus}

-- | The data catalog returned.
--
-- /Note:/ Consider using 'dataCatalog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsDataCatalog :: Lens.Lens' GetDataCatalogResponse (Core.Maybe Types.DataCatalog)
gdcrrsDataCatalog = Lens.field @"dataCatalog"
{-# INLINEABLE gdcrrsDataCatalog #-}
{-# DEPRECATED dataCatalog "Use generic-lens or generic-optics with 'dataCatalog' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsResponseStatus :: Lens.Lens' GetDataCatalogResponse Core.Int
gdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
