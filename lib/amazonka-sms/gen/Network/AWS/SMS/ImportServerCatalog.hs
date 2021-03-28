{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.ImportServerCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gathers a complete list of on-premises servers. Connectors must be installed and monitoring all servers to import.
--
-- This call returns immediately, but might take additional time to retrieve all the servers.
module Network.AWS.SMS.ImportServerCatalog
    (
    -- * Creating a request
      ImportServerCatalog (..)
    , mkImportServerCatalog

    -- * Destructuring the response
    , ImportServerCatalogResponse (..)
    , mkImportServerCatalogResponse
    -- ** Response lenses
    , iscrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkImportServerCatalog' smart constructor.
data ImportServerCatalog = ImportServerCatalog'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportServerCatalog' value with any optional fields omitted.
mkImportServerCatalog
    :: ImportServerCatalog
mkImportServerCatalog = ImportServerCatalog'

instance Core.ToQuery ImportServerCatalog where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportServerCatalog where
        toHeaders ImportServerCatalog{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.ImportServerCatalog")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportServerCatalog where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ImportServerCatalog where
        type Rs ImportServerCatalog = ImportServerCatalogResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ImportServerCatalogResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportServerCatalogResponse' smart constructor.
newtype ImportServerCatalogResponse = ImportServerCatalogResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportServerCatalogResponse' value with any optional fields omitted.
mkImportServerCatalogResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportServerCatalogResponse
mkImportServerCatalogResponse responseStatus
  = ImportServerCatalogResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrrsResponseStatus :: Lens.Lens' ImportServerCatalogResponse Core.Int
iscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
