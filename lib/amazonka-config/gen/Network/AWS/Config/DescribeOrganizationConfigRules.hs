{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConfigRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization config rules. 
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added. 
module Network.AWS.Config.DescribeOrganizationConfigRules
    (
    -- * Creating a request
      DescribeOrganizationConfigRules (..)
    , mkDescribeOrganizationConfigRules
    -- ** Request lenses
    , docrLimit
    , docrNextToken
    , docrOrganizationConfigRuleNames

    -- * Destructuring the response
    , DescribeOrganizationConfigRulesResponse (..)
    , mkDescribeOrganizationConfigRulesResponse
    -- ** Response lenses
    , docrrrsNextToken
    , docrrrsOrganizationConfigRules
    , docrrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOrganizationConfigRules' smart constructor.
data DescribeOrganizationConfigRules = DescribeOrganizationConfigRules'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of organization config rules returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
  , organizationConfigRuleNames :: Core.Maybe [Types.StringWithCharLimit64]
    -- ^ The names of organization config rules for which you want details. If you do not specify any names, AWS Config returns details for all your organization config rules.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationConfigRules' value with any optional fields omitted.
mkDescribeOrganizationConfigRules
    :: DescribeOrganizationConfigRules
mkDescribeOrganizationConfigRules
  = DescribeOrganizationConfigRules'{limit = Core.Nothing,
                                     nextToken = Core.Nothing,
                                     organizationConfigRuleNames = Core.Nothing}

-- | The maximum number of organization config rules returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrLimit :: Lens.Lens' DescribeOrganizationConfigRules (Core.Maybe Core.Natural)
docrLimit = Lens.field @"limit"
{-# INLINEABLE docrLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrNextToken :: Lens.Lens' DescribeOrganizationConfigRules (Core.Maybe Core.Text)
docrNextToken = Lens.field @"nextToken"
{-# INLINEABLE docrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The names of organization config rules for which you want details. If you do not specify any names, AWS Config returns details for all your organization config rules.
--
-- /Note:/ Consider using 'organizationConfigRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrOrganizationConfigRuleNames :: Lens.Lens' DescribeOrganizationConfigRules (Core.Maybe [Types.StringWithCharLimit64])
docrOrganizationConfigRuleNames = Lens.field @"organizationConfigRuleNames"
{-# INLINEABLE docrOrganizationConfigRuleNames #-}
{-# DEPRECATED organizationConfigRuleNames "Use generic-lens or generic-optics with 'organizationConfigRuleNames' instead"  #-}

instance Core.ToQuery DescribeOrganizationConfigRules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOrganizationConfigRules where
        toHeaders DescribeOrganizationConfigRules{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeOrganizationConfigRules")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOrganizationConfigRules where
        toJSON DescribeOrganizationConfigRules{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("OrganizationConfigRuleNames" Core..=) Core.<$>
                    organizationConfigRuleNames])

instance Core.AWSRequest DescribeOrganizationConfigRules where
        type Rs DescribeOrganizationConfigRules =
             DescribeOrganizationConfigRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOrganizationConfigRulesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "OrganizationConfigRules"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeOrganizationConfigRulesResponse' smart constructor.
data DescribeOrganizationConfigRulesResponse = DescribeOrganizationConfigRulesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
  , organizationConfigRules :: Core.Maybe [Types.OrganizationConfigRule]
    -- ^ Returns a list of @OrganizationConfigRule@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeOrganizationConfigRulesResponse' value with any optional fields omitted.
mkDescribeOrganizationConfigRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrganizationConfigRulesResponse
mkDescribeOrganizationConfigRulesResponse responseStatus
  = DescribeOrganizationConfigRulesResponse'{nextToken =
                                               Core.Nothing,
                                             organizationConfigRules = Core.Nothing, responseStatus}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrrsNextToken :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Core.Maybe Core.Text)
docrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE docrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns a list of @OrganizationConfigRule@ objects.
--
-- /Note:/ Consider using 'organizationConfigRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrrsOrganizationConfigRules :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Core.Maybe [Types.OrganizationConfigRule])
docrrrsOrganizationConfigRules = Lens.field @"organizationConfigRules"
{-# INLINEABLE docrrrsOrganizationConfigRules #-}
{-# DEPRECATED organizationConfigRules "Use generic-lens or generic-optics with 'organizationConfigRules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrrsResponseStatus :: Lens.Lens' DescribeOrganizationConfigRulesResponse Core.Int
docrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE docrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
