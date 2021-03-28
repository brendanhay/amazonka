{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization config rule deployment status for an organization.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
    (
    -- * Creating a request
      DescribeOrganizationConfigRuleStatuses (..)
    , mkDescribeOrganizationConfigRuleStatuses
    -- ** Request lenses
    , docrsLimit
    , docrsNextToken
    , docrsOrganizationConfigRuleNames

    -- * Destructuring the response
    , DescribeOrganizationConfigRuleStatusesResponse (..)
    , mkDescribeOrganizationConfigRuleStatusesResponse
    -- ** Response lenses
    , docrsrrsNextToken
    , docrsrrsOrganizationConfigRuleStatuses
    , docrsrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOrganizationConfigRuleStatuses' smart constructor.
data DescribeOrganizationConfigRuleStatuses = DescribeOrganizationConfigRuleStatuses'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of @OrganizationConfigRuleStatuses@ returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
  , organizationConfigRuleNames :: Core.Maybe [Types.StringWithCharLimit64]
    -- ^ The names of organization config rules for which you want status details. If you do not specify any names, AWS Config returns details for all your organization AWS Confg rules.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationConfigRuleStatuses' value with any optional fields omitted.
mkDescribeOrganizationConfigRuleStatuses
    :: DescribeOrganizationConfigRuleStatuses
mkDescribeOrganizationConfigRuleStatuses
  = DescribeOrganizationConfigRuleStatuses'{limit = Core.Nothing,
                                            nextToken = Core.Nothing,
                                            organizationConfigRuleNames = Core.Nothing}

-- | The maximum number of @OrganizationConfigRuleStatuses@ returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsLimit :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Core.Maybe Core.Natural)
docrsLimit = Lens.field @"limit"
{-# INLINEABLE docrsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsNextToken :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Core.Maybe Core.Text)
docrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE docrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The names of organization config rules for which you want status details. If you do not specify any names, AWS Config returns details for all your organization AWS Confg rules.
--
-- /Note:/ Consider using 'organizationConfigRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsOrganizationConfigRuleNames :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Core.Maybe [Types.StringWithCharLimit64])
docrsOrganizationConfigRuleNames = Lens.field @"organizationConfigRuleNames"
{-# INLINEABLE docrsOrganizationConfigRuleNames #-}
{-# DEPRECATED organizationConfigRuleNames "Use generic-lens or generic-optics with 'organizationConfigRuleNames' instead"  #-}

instance Core.ToQuery DescribeOrganizationConfigRuleStatuses where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOrganizationConfigRuleStatuses
         where
        toHeaders DescribeOrganizationConfigRuleStatuses{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeOrganizationConfigRuleStatuses")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOrganizationConfigRuleStatuses where
        toJSON DescribeOrganizationConfigRuleStatuses{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("OrganizationConfigRuleNames" Core..=) Core.<$>
                    organizationConfigRuleNames])

instance Core.AWSRequest DescribeOrganizationConfigRuleStatuses
         where
        type Rs DescribeOrganizationConfigRuleStatuses =
             DescribeOrganizationConfigRuleStatusesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOrganizationConfigRuleStatusesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "OrganizationConfigRuleStatuses"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeOrganizationConfigRuleStatusesResponse' smart constructor.
data DescribeOrganizationConfigRuleStatusesResponse = DescribeOrganizationConfigRuleStatusesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
  , organizationConfigRuleStatuses :: Core.Maybe [Types.OrganizationConfigRuleStatus]
    -- ^ A list of @OrganizationConfigRuleStatus@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeOrganizationConfigRuleStatusesResponse' value with any optional fields omitted.
mkDescribeOrganizationConfigRuleStatusesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrganizationConfigRuleStatusesResponse
mkDescribeOrganizationConfigRuleStatusesResponse responseStatus
  = DescribeOrganizationConfigRuleStatusesResponse'{nextToken =
                                                      Core.Nothing,
                                                    organizationConfigRuleStatuses = Core.Nothing,
                                                    responseStatus}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsrrsNextToken :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse (Core.Maybe Core.Text)
docrsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE docrsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @OrganizationConfigRuleStatus@ objects.
--
-- /Note:/ Consider using 'organizationConfigRuleStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsrrsOrganizationConfigRuleStatuses :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse (Core.Maybe [Types.OrganizationConfigRuleStatus])
docrsrrsOrganizationConfigRuleStatuses = Lens.field @"organizationConfigRuleStatuses"
{-# INLINEABLE docrsrrsOrganizationConfigRuleStatuses #-}
{-# DEPRECATED organizationConfigRuleStatuses "Use generic-lens or generic-optics with 'organizationConfigRuleStatuses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsrrsResponseStatus :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse Core.Int
docrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE docrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
