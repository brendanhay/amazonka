{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetSampledRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information about a specified number of requests--a sample--that AWS WAF randomly selects from among the first 5,000 requests that your AWS resource received during a time range that you choose. You can specify a sample size of up to 500 requests, and you can specify any time range in the previous three hours.
--
-- @GetSampledRequests@ returns a time range, which is usually the time range that you specified. However, if your resource (such as a CloudFront distribution) received 5,000 requests before the specified time range elapsed, @GetSampledRequests@ returns an updated time range. This new time range indicates the actual period during which AWS WAF selected the requests in the sample.
module Network.AWS.WAF.GetSampledRequests
  ( -- * Creating a request
    GetSampledRequests (..),
    mkGetSampledRequests,

    -- ** Request lenses
    gsrWebAclId,
    gsrRuleId,
    gsrTimeWindow,
    gsrMaxItems,

    -- * Destructuring the response
    GetSampledRequestsResponse (..),
    mkGetSampledRequestsResponse,

    -- ** Response lenses
    gsrrrsPopulationSize,
    gsrrrsSampledRequests,
    gsrrrsTimeWindow,
    gsrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetSampledRequests' smart constructor.
data GetSampledRequests = GetSampledRequests'
  { -- | The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@ to return a sample of requests.
    webAclId :: Types.WebAclId,
    -- | @RuleId@ is one of three values:
    --
    --
    --     * The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@ for which you want @GetSampledRequests@ to return a sample of requests.
    --
    --
    --     * @Default_Action@ , which causes @GetSampledRequests@ to return a sample of the requests that didn't match any of the rules in the specified @WebACL@ .
    ruleId :: Types.RuleId,
    -- | The start date and time and the end date and time of the range for which you want @GetSampledRequests@ to return a sample of requests. You must specify the times in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
    timeWindow :: Types.TimeWindow,
    -- | The number of requests that you want AWS WAF to return from among the first 5,000 requests that your AWS resource received during the time range. If your resource received fewer requests than the value of @MaxItems@ , @GetSampledRequests@ returns information about all of them.
    maxItems :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSampledRequests' value with any optional fields omitted.
mkGetSampledRequests ::
  -- | 'webAclId'
  Types.WebAclId ->
  -- | 'ruleId'
  Types.RuleId ->
  -- | 'timeWindow'
  Types.TimeWindow ->
  -- | 'maxItems'
  Core.Natural ->
  GetSampledRequests
mkGetSampledRequests webAclId ruleId timeWindow maxItems =
  GetSampledRequests' {webAclId, ruleId, timeWindow, maxItems}

-- | The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@ to return a sample of requests.
--
-- /Note:/ Consider using 'webAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrWebAclId :: Lens.Lens' GetSampledRequests Types.WebAclId
gsrWebAclId = Lens.field @"webAclId"
{-# DEPRECATED gsrWebAclId "Use generic-lens or generic-optics with 'webAclId' instead." #-}

-- | @RuleId@ is one of three values:
--
--
--     * The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@ for which you want @GetSampledRequests@ to return a sample of requests.
--
--
--     * @Default_Action@ , which causes @GetSampledRequests@ to return a sample of the requests that didn't match any of the rules in the specified @WebACL@ .
--
--
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrRuleId :: Lens.Lens' GetSampledRequests Types.RuleId
gsrRuleId = Lens.field @"ruleId"
{-# DEPRECATED gsrRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | The start date and time and the end date and time of the range for which you want @GetSampledRequests@ to return a sample of requests. You must specify the times in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
--
-- /Note:/ Consider using 'timeWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrTimeWindow :: Lens.Lens' GetSampledRequests Types.TimeWindow
gsrTimeWindow = Lens.field @"timeWindow"
{-# DEPRECATED gsrTimeWindow "Use generic-lens or generic-optics with 'timeWindow' instead." #-}

-- | The number of requests that you want AWS WAF to return from among the first 5,000 requests that your AWS resource received during the time range. If your resource received fewer requests than the value of @MaxItems@ , @GetSampledRequests@ returns information about all of them.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrMaxItems :: Lens.Lens' GetSampledRequests Core.Natural
gsrMaxItems = Lens.field @"maxItems"
{-# DEPRECATED gsrMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.FromJSON GetSampledRequests where
  toJSON GetSampledRequests {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WebAclId" Core..= webAclId),
            Core.Just ("RuleId" Core..= ruleId),
            Core.Just ("TimeWindow" Core..= timeWindow),
            Core.Just ("MaxItems" Core..= maxItems)
          ]
      )

instance Core.AWSRequest GetSampledRequests where
  type Rs GetSampledRequests = GetSampledRequestsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetSampledRequests")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSampledRequestsResponse'
            Core.<$> (x Core..:? "PopulationSize")
            Core.<*> (x Core..:? "SampledRequests")
            Core.<*> (x Core..:? "TimeWindow")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSampledRequestsResponse' smart constructor.
data GetSampledRequestsResponse = GetSampledRequestsResponse'
  { -- | The total number of requests from which @GetSampledRequests@ got a sample of @MaxItems@ requests. If @PopulationSize@ is less than @MaxItems@ , the sample includes every request that your AWS resource received during the specified time range.
    populationSize :: Core.Maybe Core.Integer,
    -- | A complex type that contains detailed information about each of the requests in the sample.
    sampledRequests :: Core.Maybe [Types.SampledHTTPRequest],
    -- | Usually, @TimeWindow@ is the time range that you specified in the @GetSampledRequests@ request. However, if your AWS resource received more than 5,000 requests during the time range that you specified in the request, @GetSampledRequests@ returns the time range for the first 5,000 requests. Times are in Coordinated Universal Time (UTC) format.
    timeWindow :: Core.Maybe Types.TimeWindow,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSampledRequestsResponse' value with any optional fields omitted.
mkGetSampledRequestsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSampledRequestsResponse
mkGetSampledRequestsResponse responseStatus =
  GetSampledRequestsResponse'
    { populationSize = Core.Nothing,
      sampledRequests = Core.Nothing,
      timeWindow = Core.Nothing,
      responseStatus
    }

-- | The total number of requests from which @GetSampledRequests@ got a sample of @MaxItems@ requests. If @PopulationSize@ is less than @MaxItems@ , the sample includes every request that your AWS resource received during the specified time range.
--
-- /Note:/ Consider using 'populationSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrrsPopulationSize :: Lens.Lens' GetSampledRequestsResponse (Core.Maybe Core.Integer)
gsrrrsPopulationSize = Lens.field @"populationSize"
{-# DEPRECATED gsrrrsPopulationSize "Use generic-lens or generic-optics with 'populationSize' instead." #-}

-- | A complex type that contains detailed information about each of the requests in the sample.
--
-- /Note:/ Consider using 'sampledRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrrsSampledRequests :: Lens.Lens' GetSampledRequestsResponse (Core.Maybe [Types.SampledHTTPRequest])
gsrrrsSampledRequests = Lens.field @"sampledRequests"
{-# DEPRECATED gsrrrsSampledRequests "Use generic-lens or generic-optics with 'sampledRequests' instead." #-}

-- | Usually, @TimeWindow@ is the time range that you specified in the @GetSampledRequests@ request. However, if your AWS resource received more than 5,000 requests during the time range that you specified in the request, @GetSampledRequests@ returns the time range for the first 5,000 requests. Times are in Coordinated Universal Time (UTC) format.
--
-- /Note:/ Consider using 'timeWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrrsTimeWindow :: Lens.Lens' GetSampledRequestsResponse (Core.Maybe Types.TimeWindow)
gsrrrsTimeWindow = Lens.field @"timeWindow"
{-# DEPRECATED gsrrrsTimeWindow "Use generic-lens or generic-optics with 'timeWindow' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrrsResponseStatus :: Lens.Lens' GetSampledRequestsResponse Core.Int
gsrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
