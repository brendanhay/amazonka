{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the Contributor Insights rules in your account.
--
-- For more information about Contributor Insights, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data> .
module Network.AWS.CloudWatch.DescribeInsightRules
    (
    -- * Creating a request
      DescribeInsightRules (..)
    , mkDescribeInsightRules
    -- ** Request lenses
    , dirMaxResults
    , dirNextToken

    -- * Destructuring the response
    , DescribeInsightRulesResponse (..)
    , mkDescribeInsightRulesResponse
    -- ** Response lenses
    , dirrfrsInsightRules
    , dirrfrsNextToken
    , dirrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInsightRules' smart constructor.
data DescribeInsightRules = DescribeInsightRules'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in one operation. If you omit this parameter, the default of 500 is used.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Include this value, if it was returned by the previous operation, to get the next set of rules.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInsightRules' value with any optional fields omitted.
mkDescribeInsightRules
    :: DescribeInsightRules
mkDescribeInsightRules
  = DescribeInsightRules'{maxResults = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The maximum number of results to return in one operation. If you omit this parameter, the default of 500 is used.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirMaxResults :: Lens.Lens' DescribeInsightRules (Core.Maybe Core.Natural)
dirMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dirMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Include this value, if it was returned by the previous operation, to get the next set of rules.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirNextToken :: Lens.Lens' DescribeInsightRules (Core.Maybe Types.NextToken)
dirNextToken = Lens.field @"nextToken"
{-# INLINEABLE dirNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInsightRules where
        toQuery DescribeInsightRules{..}
          = Core.toQueryPair "Action" ("DescribeInsightRules" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeInsightRules where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeInsightRules where
        type Rs DescribeInsightRules = DescribeInsightRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeInsightRulesResult"
              (\ s h x ->
                 DescribeInsightRulesResponse' Core.<$>
                   (x Core..@? "InsightRules" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeInsightRulesResponse' smart constructor.
data DescribeInsightRulesResponse = DescribeInsightRulesResponse'
  { insightRules :: Core.Maybe [Types.InsightRule]
    -- ^ The rules returned by the operation.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If this parameter is present, it is a token that marks the start of the next batch of returned results. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInsightRulesResponse' value with any optional fields omitted.
mkDescribeInsightRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInsightRulesResponse
mkDescribeInsightRulesResponse responseStatus
  = DescribeInsightRulesResponse'{insightRules = Core.Nothing,
                                  nextToken = Core.Nothing, responseStatus}

-- | The rules returned by the operation.
--
-- /Note:/ Consider using 'insightRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrfrsInsightRules :: Lens.Lens' DescribeInsightRulesResponse (Core.Maybe [Types.InsightRule])
dirrfrsInsightRules = Lens.field @"insightRules"
{-# INLINEABLE dirrfrsInsightRules #-}
{-# DEPRECATED insightRules "Use generic-lens or generic-optics with 'insightRules' instead"  #-}

-- | If this parameter is present, it is a token that marks the start of the next batch of returned results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrfrsNextToken :: Lens.Lens' DescribeInsightRulesResponse (Core.Maybe Types.NextToken)
dirrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dirrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrfrsResponseStatus :: Lens.Lens' DescribeInsightRulesResponse Core.Int
dirrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
