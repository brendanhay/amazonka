{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the organization that the user's account belongs to.
--
-- This operation can be called from any account in the organization.
module Network.AWS.Organizations.DescribeOrganization
    (
    -- * Creating a request
      DescribeOrganization (..)
    , mkDescribeOrganization

    -- * Destructuring the response
    , DescribeOrganizationResponse (..)
    , mkDescribeOrganizationResponse
    -- ** Response lenses
    , dorrsOrganization
    , dorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOrganization' smart constructor.
data DescribeOrganization = DescribeOrganization'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganization' value with any optional fields omitted.
mkDescribeOrganization
    :: DescribeOrganization
mkDescribeOrganization = DescribeOrganization'

instance Core.ToQuery DescribeOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOrganization where
        toHeaders DescribeOrganization{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.DescribeOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOrganization where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeOrganization where
        type Rs DescribeOrganization = DescribeOrganizationResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOrganizationResponse' Core.<$>
                   (x Core..:? "Organization") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { organization :: Core.Maybe Types.Organization
    -- ^ A structure that contains information about the organization.
--
-- /Important:/ The @AvailablePolicyTypes@ part of the response is deprecated, and you shouldn't use it in your apps. It doesn't include any policy type supported by Organizations other than SCPs. To determine which policy types are enabled in your organization, use the @'ListRoots' @ operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationResponse' value with any optional fields omitted.
mkDescribeOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrganizationResponse
mkDescribeOrganizationResponse responseStatus
  = DescribeOrganizationResponse'{organization = Core.Nothing,
                                  responseStatus}

-- | A structure that contains information about the organization.
--
-- /Important:/ The @AvailablePolicyTypes@ part of the response is deprecated, and you shouldn't use it in your apps. It doesn't include any policy type supported by Organizations other than SCPs. To determine which policy types are enabled in your organization, use the @'ListRoots' @ operation.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsOrganization :: Lens.Lens' DescribeOrganizationResponse (Core.Maybe Types.Organization)
dorrsOrganization = Lens.field @"organization"
{-# INLINEABLE dorrsOrganization #-}
{-# DEPRECATED organization "Use generic-lens or generic-optics with 'organization' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsResponseStatus :: Lens.Lens' DescribeOrganizationResponse Core.Int
dorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
