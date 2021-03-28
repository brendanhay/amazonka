{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.ImportAppCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows application import from AWS Migration Hub.
module Network.AWS.SMS.ImportAppCatalog
    (
    -- * Creating a request
      ImportAppCatalog (..)
    , mkImportAppCatalog
    -- ** Request lenses
    , iacRoleName

    -- * Destructuring the response
    , ImportAppCatalogResponse (..)
    , mkImportAppCatalogResponse
    -- ** Response lenses
    , iacrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkImportAppCatalog' smart constructor.
newtype ImportAppCatalog = ImportAppCatalog'
  { roleName :: Core.Maybe Types.RoleName
    -- ^ The name of the service role. If you omit this parameter, we create a service-linked role for AWS Migration Hub in your account. Otherwise, the role that you provide must have the <https://docs.aws.amazon.com/migrationhub/latest/ug/new-customer-setup.html#sms-managed policy and trust policy> described in the /AWS Migration Hub User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportAppCatalog' value with any optional fields omitted.
mkImportAppCatalog
    :: ImportAppCatalog
mkImportAppCatalog = ImportAppCatalog'{roleName = Core.Nothing}

-- | The name of the service role. If you omit this parameter, we create a service-linked role for AWS Migration Hub in your account. Otherwise, the role that you provide must have the <https://docs.aws.amazon.com/migrationhub/latest/ug/new-customer-setup.html#sms-managed policy and trust policy> described in the /AWS Migration Hub User Guide/ .
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iacRoleName :: Lens.Lens' ImportAppCatalog (Core.Maybe Types.RoleName)
iacRoleName = Lens.field @"roleName"
{-# INLINEABLE iacRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

instance Core.ToQuery ImportAppCatalog where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportAppCatalog where
        toHeaders ImportAppCatalog{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.ImportAppCatalog")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportAppCatalog where
        toJSON ImportAppCatalog{..}
          = Core.object
              (Core.catMaybes [("roleName" Core..=) Core.<$> roleName])

instance Core.AWSRequest ImportAppCatalog where
        type Rs ImportAppCatalog = ImportAppCatalogResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ImportAppCatalogResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportAppCatalogResponse' smart constructor.
newtype ImportAppCatalogResponse = ImportAppCatalogResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportAppCatalogResponse' value with any optional fields omitted.
mkImportAppCatalogResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportAppCatalogResponse
mkImportAppCatalogResponse responseStatus
  = ImportAppCatalogResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iacrrsResponseStatus :: Lens.Lens' ImportAppCatalogResponse Core.Int
iacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
