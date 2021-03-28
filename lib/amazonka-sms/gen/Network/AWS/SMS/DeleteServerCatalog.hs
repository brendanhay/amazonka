{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteServerCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all servers from your server catalog.
module Network.AWS.SMS.DeleteServerCatalog
    (
    -- * Creating a request
      DeleteServerCatalog (..)
    , mkDeleteServerCatalog

    -- * Destructuring the response
    , DeleteServerCatalogResponse (..)
    , mkDeleteServerCatalogResponse
    -- ** Response lenses
    , dscrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteServerCatalog' smart constructor.
data DeleteServerCatalog = DeleteServerCatalog'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServerCatalog' value with any optional fields omitted.
mkDeleteServerCatalog
    :: DeleteServerCatalog
mkDeleteServerCatalog = DeleteServerCatalog'

instance Core.ToQuery DeleteServerCatalog where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteServerCatalog where
        toHeaders DeleteServerCatalog{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.DeleteServerCatalog")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteServerCatalog where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DeleteServerCatalog where
        type Rs DeleteServerCatalog = DeleteServerCatalogResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteServerCatalogResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteServerCatalogResponse' smart constructor.
newtype DeleteServerCatalogResponse = DeleteServerCatalogResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServerCatalogResponse' value with any optional fields omitted.
mkDeleteServerCatalogResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteServerCatalogResponse
mkDeleteServerCatalogResponse responseStatus
  = DeleteServerCatalogResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsResponseStatus :: Lens.Lens' DeleteServerCatalogResponse Core.Int
dscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
