{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gsrWebACLId,
    gsrRuleId,
    gsrTimeWindow,
    gsrMaxItems,

    -- * Destructuring the response
    GetSampledRequestsResponse (..),
    mkGetSampledRequestsResponse,

    -- ** Response lenses
    gsrrsSampledRequests,
    gsrrsPopulationSize,
    gsrrsTimeWindow,
    gsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetSampledRequests' smart constructor.
data GetSampledRequests = GetSampledRequests'
  { webACLId ::
      Lude.Text,
    ruleId :: Lude.Text,
    timeWindow :: TimeWindow,
    maxItems :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSampledRequests' with the minimum fields required to make a request.
--
-- * 'maxItems' - The number of requests that you want AWS WAF to return from among the first 5,000 requests that your AWS resource received during the time range. If your resource received fewer requests than the value of @MaxItems@ , @GetSampledRequests@ returns information about all of them.
-- * 'ruleId' - @RuleId@ is one of three values:
--
--
--     * The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@ for which you want @GetSampledRequests@ to return a sample of requests.
--
--
--     * @Default_Action@ , which causes @GetSampledRequests@ to return a sample of the requests that didn't match any of the rules in the specified @WebACL@ .
--
--
-- * 'timeWindow' - The start date and time and the end date and time of the range for which you want @GetSampledRequests@ to return a sample of requests. You must specify the times in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
-- * 'webACLId' - The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@ to return a sample of requests.
mkGetSampledRequests ::
  -- | 'webACLId'
  Lude.Text ->
  -- | 'ruleId'
  Lude.Text ->
  -- | 'timeWindow'
  TimeWindow ->
  -- | 'maxItems'
  Lude.Natural ->
  GetSampledRequests
mkGetSampledRequests pWebACLId_ pRuleId_ pTimeWindow_ pMaxItems_ =
  GetSampledRequests'
    { webACLId = pWebACLId_,
      ruleId = pRuleId_,
      timeWindow = pTimeWindow_,
      maxItems = pMaxItems_
    }

-- | The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@ to return a sample of requests.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrWebACLId :: Lens.Lens' GetSampledRequests Lude.Text
gsrWebACLId = Lens.lens (webACLId :: GetSampledRequests -> Lude.Text) (\s a -> s {webACLId = a} :: GetSampledRequests)
{-# DEPRECATED gsrWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

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
gsrRuleId :: Lens.Lens' GetSampledRequests Lude.Text
gsrRuleId = Lens.lens (ruleId :: GetSampledRequests -> Lude.Text) (\s a -> s {ruleId = a} :: GetSampledRequests)
{-# DEPRECATED gsrRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | The start date and time and the end date and time of the range for which you want @GetSampledRequests@ to return a sample of requests. You must specify the times in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
--
-- /Note:/ Consider using 'timeWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrTimeWindow :: Lens.Lens' GetSampledRequests TimeWindow
gsrTimeWindow = Lens.lens (timeWindow :: GetSampledRequests -> TimeWindow) (\s a -> s {timeWindow = a} :: GetSampledRequests)
{-# DEPRECATED gsrTimeWindow "Use generic-lens or generic-optics with 'timeWindow' instead." #-}

-- | The number of requests that you want AWS WAF to return from among the first 5,000 requests that your AWS resource received during the time range. If your resource received fewer requests than the value of @MaxItems@ , @GetSampledRequests@ returns information about all of them.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrMaxItems :: Lens.Lens' GetSampledRequests Lude.Natural
gsrMaxItems = Lens.lens (maxItems :: GetSampledRequests -> Lude.Natural) (\s a -> s {maxItems = a} :: GetSampledRequests)
{-# DEPRECATED gsrMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest GetSampledRequests where
  type Rs GetSampledRequests = GetSampledRequestsResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSampledRequestsResponse'
            Lude.<$> (x Lude..?> "SampledRequests" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "PopulationSize")
            Lude.<*> (x Lude..?> "TimeWindow")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSampledRequests where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetSampledRequests" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSampledRequests where
  toJSON GetSampledRequests' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WebAclId" Lude..= webACLId),
            Lude.Just ("RuleId" Lude..= ruleId),
            Lude.Just ("TimeWindow" Lude..= timeWindow),
            Lude.Just ("MaxItems" Lude..= maxItems)
          ]
      )

instance Lude.ToPath GetSampledRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSampledRequests where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSampledRequestsResponse' smart constructor.
data GetSampledRequestsResponse = GetSampledRequestsResponse'
  { sampledRequests ::
      Lude.Maybe [SampledHTTPRequest],
    populationSize ::
      Lude.Maybe Lude.Integer,
    timeWindow :: Lude.Maybe TimeWindow,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSampledRequestsResponse' with the minimum fields required to make a request.
--
-- * 'populationSize' - The total number of requests from which @GetSampledRequests@ got a sample of @MaxItems@ requests. If @PopulationSize@ is less than @MaxItems@ , the sample includes every request that your AWS resource received during the specified time range.
-- * 'responseStatus' - The response status code.
-- * 'sampledRequests' - A complex type that contains detailed information about each of the requests in the sample.
-- * 'timeWindow' - Usually, @TimeWindow@ is the time range that you specified in the @GetSampledRequests@ request. However, if your AWS resource received more than 5,000 requests during the time range that you specified in the request, @GetSampledRequests@ returns the time range for the first 5,000 requests. Times are in Coordinated Universal Time (UTC) format.
mkGetSampledRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSampledRequestsResponse
mkGetSampledRequestsResponse pResponseStatus_ =
  GetSampledRequestsResponse'
    { sampledRequests = Lude.Nothing,
      populationSize = Lude.Nothing,
      timeWindow = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains detailed information about each of the requests in the sample.
--
-- /Note:/ Consider using 'sampledRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSampledRequests :: Lens.Lens' GetSampledRequestsResponse (Lude.Maybe [SampledHTTPRequest])
gsrrsSampledRequests = Lens.lens (sampledRequests :: GetSampledRequestsResponse -> Lude.Maybe [SampledHTTPRequest]) (\s a -> s {sampledRequests = a} :: GetSampledRequestsResponse)
{-# DEPRECATED gsrrsSampledRequests "Use generic-lens or generic-optics with 'sampledRequests' instead." #-}

-- | The total number of requests from which @GetSampledRequests@ got a sample of @MaxItems@ requests. If @PopulationSize@ is less than @MaxItems@ , the sample includes every request that your AWS resource received during the specified time range.
--
-- /Note:/ Consider using 'populationSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsPopulationSize :: Lens.Lens' GetSampledRequestsResponse (Lude.Maybe Lude.Integer)
gsrrsPopulationSize = Lens.lens (populationSize :: GetSampledRequestsResponse -> Lude.Maybe Lude.Integer) (\s a -> s {populationSize = a} :: GetSampledRequestsResponse)
{-# DEPRECATED gsrrsPopulationSize "Use generic-lens or generic-optics with 'populationSize' instead." #-}

-- | Usually, @TimeWindow@ is the time range that you specified in the @GetSampledRequests@ request. However, if your AWS resource received more than 5,000 requests during the time range that you specified in the request, @GetSampledRequests@ returns the time range for the first 5,000 requests. Times are in Coordinated Universal Time (UTC) format.
--
-- /Note:/ Consider using 'timeWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsTimeWindow :: Lens.Lens' GetSampledRequestsResponse (Lude.Maybe TimeWindow)
gsrrsTimeWindow = Lens.lens (timeWindow :: GetSampledRequestsResponse -> Lude.Maybe TimeWindow) (\s a -> s {timeWindow = a} :: GetSampledRequestsResponse)
{-# DEPRECATED gsrrsTimeWindow "Use generic-lens or generic-optics with 'timeWindow' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSampledRequestsResponse Lude.Int
gsrrsResponseStatus = Lens.lens (responseStatus :: GetSampledRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSampledRequestsResponse)
{-# DEPRECATED gsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
