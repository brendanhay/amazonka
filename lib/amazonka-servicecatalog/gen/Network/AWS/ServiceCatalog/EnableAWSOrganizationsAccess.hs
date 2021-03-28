{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable portfolio sharing feature through AWS Organizations. This API will allow Service Catalog to receive updates on your organization in order to sync your shares with the current structure. This API can only be called by the management account in the organization.
--
-- By calling this API Service Catalog will make a call to organizations:EnableAWSServiceAccess on your behalf so that your shares can be in sync with any changes in your AWS Organizations structure.
-- Note that a delegated administrator is not authorized to invoke @EnableAWSOrganizationsAccess@ .
module Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
    (
    -- * Creating a request
      EnableAWSOrganizationsAccess (..)
    , mkEnableAWSOrganizationsAccess

    -- * Destructuring the response
    , EnableAWSOrganizationsAccessResponse (..)
    , mkEnableAWSOrganizationsAccessResponse
    -- ** Response lenses
    , eawsoarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkEnableAWSOrganizationsAccess' smart constructor.
data EnableAWSOrganizationsAccess = EnableAWSOrganizationsAccess'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAWSOrganizationsAccess' value with any optional fields omitted.
mkEnableAWSOrganizationsAccess
    :: EnableAWSOrganizationsAccess
mkEnableAWSOrganizationsAccess = EnableAWSOrganizationsAccess'

instance Core.ToQuery EnableAWSOrganizationsAccess where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableAWSOrganizationsAccess where
        toHeaders EnableAWSOrganizationsAccess{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.EnableAWSOrganizationsAccess")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableAWSOrganizationsAccess where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest EnableAWSOrganizationsAccess where
        type Rs EnableAWSOrganizationsAccess =
             EnableAWSOrganizationsAccessResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 EnableAWSOrganizationsAccessResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableAWSOrganizationsAccessResponse' smart constructor.
newtype EnableAWSOrganizationsAccessResponse = EnableAWSOrganizationsAccessResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAWSOrganizationsAccessResponse' value with any optional fields omitted.
mkEnableAWSOrganizationsAccessResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableAWSOrganizationsAccessResponse
mkEnableAWSOrganizationsAccessResponse responseStatus
  = EnableAWSOrganizationsAccessResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eawsoarrsResponseStatus :: Lens.Lens' EnableAWSOrganizationsAccessResponse Core.Int
eawsoarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eawsoarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
