{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the Access Status for AWS Organization portfolio share feature. This API can only be called by the management account in the organization or by a delegated admin.
module Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
    (
    -- * Creating a request
      GetAWSOrganizationsAccessStatus (..)
    , mkGetAWSOrganizationsAccessStatus

    -- * Destructuring the response
    , GetAWSOrganizationsAccessStatusResponse (..)
    , mkGetAWSOrganizationsAccessStatusResponse
    -- ** Response lenses
    , gawsoasrrsAccessStatus
    , gawsoasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkGetAWSOrganizationsAccessStatus' smart constructor.
data GetAWSOrganizationsAccessStatus = GetAWSOrganizationsAccessStatus'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAWSOrganizationsAccessStatus' value with any optional fields omitted.
mkGetAWSOrganizationsAccessStatus
    :: GetAWSOrganizationsAccessStatus
mkGetAWSOrganizationsAccessStatus
  = GetAWSOrganizationsAccessStatus'

instance Core.ToQuery GetAWSOrganizationsAccessStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAWSOrganizationsAccessStatus where
        toHeaders GetAWSOrganizationsAccessStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.GetAWSOrganizationsAccessStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAWSOrganizationsAccessStatus where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetAWSOrganizationsAccessStatus where
        type Rs GetAWSOrganizationsAccessStatus =
             GetAWSOrganizationsAccessStatusResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAWSOrganizationsAccessStatusResponse' Core.<$>
                   (x Core..:? "AccessStatus") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAWSOrganizationsAccessStatusResponse' smart constructor.
data GetAWSOrganizationsAccessStatusResponse = GetAWSOrganizationsAccessStatusResponse'
  { accessStatus :: Core.Maybe Types.AccessStatus
    -- ^ The status of the portfolio share feature.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAWSOrganizationsAccessStatusResponse' value with any optional fields omitted.
mkGetAWSOrganizationsAccessStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAWSOrganizationsAccessStatusResponse
mkGetAWSOrganizationsAccessStatusResponse responseStatus
  = GetAWSOrganizationsAccessStatusResponse'{accessStatus =
                                               Core.Nothing,
                                             responseStatus}

-- | The status of the portfolio share feature.
--
-- /Note:/ Consider using 'accessStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gawsoasrrsAccessStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse (Core.Maybe Types.AccessStatus)
gawsoasrrsAccessStatus = Lens.field @"accessStatus"
{-# INLINEABLE gawsoasrrsAccessStatus #-}
{-# DEPRECATED accessStatus "Use generic-lens or generic-optics with 'accessStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gawsoasrrsResponseStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse Core.Int
gawsoasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gawsoasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
