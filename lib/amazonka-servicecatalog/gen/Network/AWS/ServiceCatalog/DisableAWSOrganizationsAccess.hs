{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disable portfolio sharing through AWS Organizations feature. This feature will not delete your current shares but it will prevent you from creating new shares throughout your organization. Current shares will not be in sync with your organization structure if it changes after calling this API. This API can only be called by the management account in the organization.
--
-- This API can't be invoked if there are active delegated administrators in the organization.
-- Note that a delegated administrator is not authorized to invoke @DisableAWSOrganizationsAccess@ .
module Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
    (
    -- * Creating a request
      DisableAWSOrganizationsAccess (..)
    , mkDisableAWSOrganizationsAccess

    -- * Destructuring the response
    , DisableAWSOrganizationsAccessResponse (..)
    , mkDisableAWSOrganizationsAccessResponse
    -- ** Response lenses
    , dawsoarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisableAWSOrganizationsAccess' smart constructor.
data DisableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAWSOrganizationsAccess' value with any optional fields omitted.
mkDisableAWSOrganizationsAccess
    :: DisableAWSOrganizationsAccess
mkDisableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'

instance Core.ToQuery DisableAWSOrganizationsAccess where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableAWSOrganizationsAccess where
        toHeaders DisableAWSOrganizationsAccess{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DisableAWSOrganizationsAccess")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableAWSOrganizationsAccess where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisableAWSOrganizationsAccess where
        type Rs DisableAWSOrganizationsAccess =
             DisableAWSOrganizationsAccessResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisableAWSOrganizationsAccessResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableAWSOrganizationsAccessResponse' smart constructor.
newtype DisableAWSOrganizationsAccessResponse = DisableAWSOrganizationsAccessResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAWSOrganizationsAccessResponse' value with any optional fields omitted.
mkDisableAWSOrganizationsAccessResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableAWSOrganizationsAccessResponse
mkDisableAWSOrganizationsAccessResponse responseStatus
  = DisableAWSOrganizationsAccessResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dawsoarrsResponseStatus :: Lens.Lens' DisableAWSOrganizationsAccessResponse Core.Int
dawsoarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dawsoarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
