{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for each of your AWS managed Config rules. The status includes information such as the last time AWS Config invoked the rule, the last time AWS Config failed to invoke the rule, and the related error for the last failure.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRuleEvaluationStatus
    (
    -- * Creating a request
      DescribeConfigRuleEvaluationStatus (..)
    , mkDescribeConfigRuleEvaluationStatus
    -- ** Request lenses
    , dcresConfigRuleNames
    , dcresLimit
    , dcresNextToken

    -- * Destructuring the response
    , DescribeConfigRuleEvaluationStatusResponse (..)
    , mkDescribeConfigRuleEvaluationStatusResponse
    -- ** Response lenses
    , dcresrrsConfigRulesEvaluationStatus
    , dcresrrsNextToken
    , dcresrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeConfigRuleEvaluationStatus' smart constructor.
data DescribeConfigRuleEvaluationStatus = DescribeConfigRuleEvaluationStatus'
  { configRuleNames :: Core.Maybe [Types.ConfigRuleName]
    -- ^ The name of the AWS managed Config rules for which you want status information. If you do not specify any names, AWS Config returns status information for all AWS managed Config rules that you use.
  , limit :: Core.Maybe Core.Natural
    -- ^ The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more than the default of 150 rules.
-- For information about requesting a rule limit increase, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits> in the /AWS General Reference Guide/ .
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigRuleEvaluationStatus' value with any optional fields omitted.
mkDescribeConfigRuleEvaluationStatus
    :: DescribeConfigRuleEvaluationStatus
mkDescribeConfigRuleEvaluationStatus
  = DescribeConfigRuleEvaluationStatus'{configRuleNames =
                                          Core.Nothing,
                                        limit = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the AWS managed Config rules for which you want status information. If you do not specify any names, AWS Config returns status information for all AWS managed Config rules that you use.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresConfigRuleNames :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Core.Maybe [Types.ConfigRuleName])
dcresConfigRuleNames = Lens.field @"configRuleNames"
{-# INLINEABLE dcresConfigRuleNames #-}
{-# DEPRECATED configRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead"  #-}

-- | The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more than the default of 150 rules.
-- For information about requesting a rule limit increase, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits> in the /AWS General Reference Guide/ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresLimit :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Core.Maybe Core.Natural)
dcresLimit = Lens.field @"limit"
{-# INLINEABLE dcresLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresNextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Core.Maybe Core.Text)
dcresNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcresNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeConfigRuleEvaluationStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConfigRuleEvaluationStatus where
        toHeaders DescribeConfigRuleEvaluationStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeConfigRuleEvaluationStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeConfigRuleEvaluationStatus where
        toJSON DescribeConfigRuleEvaluationStatus{..}
          = Core.object
              (Core.catMaybes
                 [("ConfigRuleNames" Core..=) Core.<$> configRuleNames,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeConfigRuleEvaluationStatus where
        type Rs DescribeConfigRuleEvaluationStatus =
             DescribeConfigRuleEvaluationStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConfigRuleEvaluationStatusResponse' Core.<$>
                   (x Core..:? "ConfigRulesEvaluationStatus") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeConfigRuleEvaluationStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"configRulesEvaluationStatus" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | 
--
-- /See:/ 'mkDescribeConfigRuleEvaluationStatusResponse' smart constructor.
data DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse'
  { configRulesEvaluationStatus :: Core.Maybe [Types.ConfigRuleEvaluationStatus]
    -- ^ Status information about your AWS managed Config rules.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string that you use in a subsequent request to get the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeConfigRuleEvaluationStatusResponse' value with any optional fields omitted.
mkDescribeConfigRuleEvaluationStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConfigRuleEvaluationStatusResponse
mkDescribeConfigRuleEvaluationStatusResponse responseStatus
  = DescribeConfigRuleEvaluationStatusResponse'{configRulesEvaluationStatus
                                                  = Core.Nothing,
                                                nextToken = Core.Nothing, responseStatus}

-- | Status information about your AWS managed Config rules.
--
-- /Note:/ Consider using 'configRulesEvaluationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresrrsConfigRulesEvaluationStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Core.Maybe [Types.ConfigRuleEvaluationStatus])
dcresrrsConfigRulesEvaluationStatus = Lens.field @"configRulesEvaluationStatus"
{-# INLINEABLE dcresrrsConfigRulesEvaluationStatus #-}
{-# DEPRECATED configRulesEvaluationStatus "Use generic-lens or generic-optics with 'configRulesEvaluationStatus' instead"  #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresrrsNextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Core.Maybe Core.Text)
dcresrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcresrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcresrrsResponseStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse Core.Int
dcresrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcresrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
