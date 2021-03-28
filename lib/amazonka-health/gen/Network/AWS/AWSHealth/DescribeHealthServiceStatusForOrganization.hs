{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation provides status information on enabling or disabling AWS Health to work with your organization. To call this operation, you must sign in as an IAM user, assume an IAM role, or sign in as the root user (not recommended) in the organization's master account.
module Network.AWS.AWSHealth.DescribeHealthServiceStatusForOrganization
    (
    -- * Creating a request
      DescribeHealthServiceStatusForOrganization (..)
    , mkDescribeHealthServiceStatusForOrganization

    -- * Destructuring the response
    , DescribeHealthServiceStatusForOrganizationResponse (..)
    , mkDescribeHealthServiceStatusForOrganizationResponse
    -- ** Response lenses
    , dhssforrsHealthServiceAccessStatusForOrganization
    , dhssforrsResponseStatus
    ) where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeHealthServiceStatusForOrganization' smart constructor.
data DescribeHealthServiceStatusForOrganization = DescribeHealthServiceStatusForOrganization'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHealthServiceStatusForOrganization' value with any optional fields omitted.
mkDescribeHealthServiceStatusForOrganization
    :: DescribeHealthServiceStatusForOrganization
mkDescribeHealthServiceStatusForOrganization
  = DescribeHealthServiceStatusForOrganization'

instance Core.ToQuery DescribeHealthServiceStatusForOrganization
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeHealthServiceStatusForOrganization
         where
        toHeaders DescribeHealthServiceStatusForOrganization{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSHealth_20160804.DescribeHealthServiceStatusForOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeHealthServiceStatusForOrganization
         where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeHealthServiceStatusForOrganization
         where
        type Rs DescribeHealthServiceStatusForOrganization =
             DescribeHealthServiceStatusForOrganizationResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeHealthServiceStatusForOrganizationResponse' Core.<$>
                   (x Core..:? "healthServiceAccessStatusForOrganization") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeHealthServiceStatusForOrganizationResponse' smart constructor.
data DescribeHealthServiceStatusForOrganizationResponse = DescribeHealthServiceStatusForOrganizationResponse'
  { healthServiceAccessStatusForOrganization :: Core.Maybe Types.HealthServiceAccessStatusForOrganization
    -- ^ Information about the status of enabling or disabling AWS Health Organizational View in your organization.
--
-- Valid values are @ENABLED | DISABLED | PENDING@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHealthServiceStatusForOrganizationResponse' value with any optional fields omitted.
mkDescribeHealthServiceStatusForOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeHealthServiceStatusForOrganizationResponse
mkDescribeHealthServiceStatusForOrganizationResponse responseStatus
  = DescribeHealthServiceStatusForOrganizationResponse'{healthServiceAccessStatusForOrganization
                                                          = Core.Nothing,
                                                        responseStatus}

-- | Information about the status of enabling or disabling AWS Health Organizational View in your organization.
--
-- Valid values are @ENABLED | DISABLED | PENDING@ . 
--
-- /Note:/ Consider using 'healthServiceAccessStatusForOrganization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhssforrsHealthServiceAccessStatusForOrganization :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse (Core.Maybe Types.HealthServiceAccessStatusForOrganization)
dhssforrsHealthServiceAccessStatusForOrganization = Lens.field @"healthServiceAccessStatusForOrganization"
{-# INLINEABLE dhssforrsHealthServiceAccessStatusForOrganization #-}
{-# DEPRECATED healthServiceAccessStatusForOrganization "Use generic-lens or generic-optics with 'healthServiceAccessStatusForOrganization' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhssforrsResponseStatus :: Lens.Lens' DescribeHealthServiceStatusForOrganizationResponse Core.Int
dhssforrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhssforrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
