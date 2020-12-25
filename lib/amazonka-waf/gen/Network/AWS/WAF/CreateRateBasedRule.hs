{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateRateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'RateBasedRule' . The @RateBasedRule@ contains a @RateLimit@ , which specifies the maximum number of requests that AWS WAF allows from a specified IP address in a five-minute period. The @RateBasedRule@ also contains the @IPSet@ objects, @ByteMatchSet@ objects, and other predicates that identify the requests that you want to count or block if these requests exceed the @RateLimit@ .
--
-- If you add more than one predicate to a @RateBasedRule@ , a request not only must exceed the @RateLimit@ , but it also must match all the conditions to be counted or blocked. For example, suppose you add the following to a @RateBasedRule@ :
--
--     * An @IPSet@ that matches the IP address @192.0.2.44/32@
--
--
--     * A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
--
-- Further, you specify a @RateLimit@ of 1,000.
-- You then add the @RateBasedRule@ to a @WebACL@ and specify that you want to block requests that meet the conditions in the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header in the request must contain the value @BadBot@ . Further, requests that match these two conditions must be received at a rate of more than 1,000 requests every five minutes. If both conditions are met and the rate is exceeded, AWS WAF blocks the requests. If the rate drops below 1,000 for a five-minute period, AWS WAF no longer blocks the requests.
-- As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a @RateBasedRule@ :
--
--     * A @ByteMatchSet@ with @FieldToMatch@ of @URI@
--
--
--     * A @PositionalConstraint@ of @STARTS_WITH@
--
--
--     * A @TargetString@ of @login@
--
--
-- Further, you specify a @RateLimit@ of 1,000.
-- By adding this @RateBasedRule@ to a @WebACL@ , you could limit requests to your login page without affecting the rest of your site.
-- To create and configure a @RateBasedRule@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in the rule. For more information, see 'CreateByteMatchSet' , 'CreateIPSet' , and 'CreateSqlInjectionMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRule@ request.
--
--
--     * Submit a @CreateRateBasedRule@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRule' request.
--
--
--     * Submit an @UpdateRateBasedRule@ request to specify the predicates that you want to include in the rule.
--
--
--     * Create and update a @WebACL@ that contains the @RateBasedRule@ . For more information, see 'CreateWebACL' .
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateRateBasedRule
  ( -- * Creating a request
    CreateRateBasedRule (..),
    mkCreateRateBasedRule,

    -- ** Request lenses
    crbrName,
    crbrMetricName,
    crbrRateKey,
    crbrRateLimit,
    crbrChangeToken,
    crbrTags,

    -- * Destructuring the response
    CreateRateBasedRuleResponse (..),
    mkCreateRateBasedRuleResponse,

    -- ** Response lenses
    crbrrrsChangeToken,
    crbrrrsRule,
    crbrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkCreateRateBasedRule' smart constructor.
data CreateRateBasedRule = CreateRateBasedRule'
  { -- | A friendly name or description of the 'RateBasedRule' . You can't change the name of a @RateBasedRule@ after you create it.
    name :: Types.ResourceName,
    -- | A friendly name or description for the metrics for this @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
    metricName :: Types.MetricName,
    -- | The field that AWS WAF uses to determine if requests are likely arriving from a single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests that arrive from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
    rateKey :: Types.RateKey,
    -- | The maximum number of requests, which have an identical value in the field that is specified by @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
    rateLimit :: Core.Natural,
    -- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Types.ChangeToken,
    -- |
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRateBasedRule' value with any optional fields omitted.
mkCreateRateBasedRule ::
  -- | 'name'
  Types.ResourceName ->
  -- | 'metricName'
  Types.MetricName ->
  -- | 'rateKey'
  Types.RateKey ->
  -- | 'rateLimit'
  Core.Natural ->
  -- | 'changeToken'
  Types.ChangeToken ->
  CreateRateBasedRule
mkCreateRateBasedRule name metricName rateKey rateLimit changeToken =
  CreateRateBasedRule'
    { name,
      metricName,
      rateKey,
      rateLimit,
      changeToken,
      tags = Core.Nothing
    }

-- | A friendly name or description of the 'RateBasedRule' . You can't change the name of a @RateBasedRule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrName :: Lens.Lens' CreateRateBasedRule Types.ResourceName
crbrName = Lens.field @"name"
{-# DEPRECATED crbrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A friendly name or description for the metrics for this @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrMetricName :: Lens.Lens' CreateRateBasedRule Types.MetricName
crbrMetricName = Lens.field @"metricName"
{-# DEPRECATED crbrMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The field that AWS WAF uses to determine if requests are likely arriving from a single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests that arrive from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
--
-- /Note:/ Consider using 'rateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrRateKey :: Lens.Lens' CreateRateBasedRule Types.RateKey
crbrRateKey = Lens.field @"rateKey"
{-# DEPRECATED crbrRateKey "Use generic-lens or generic-optics with 'rateKey' instead." #-}

-- | The maximum number of requests, which have an identical value in the field that is specified by @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrRateLimit :: Lens.Lens' CreateRateBasedRule Core.Natural
crbrRateLimit = Lens.field @"rateLimit"
{-# DEPRECATED crbrRateLimit "Use generic-lens or generic-optics with 'rateLimit' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrChangeToken :: Lens.Lens' CreateRateBasedRule Types.ChangeToken
crbrChangeToken = Lens.field @"changeToken"
{-# DEPRECATED crbrChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- |
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrTags :: Lens.Lens' CreateRateBasedRule (Core.Maybe (Core.NonEmpty Types.Tag))
crbrTags = Lens.field @"tags"
{-# DEPRECATED crbrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateRateBasedRule where
  toJSON CreateRateBasedRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("MetricName" Core..= metricName),
            Core.Just ("RateKey" Core..= rateKey),
            Core.Just ("RateLimit" Core..= rateLimit),
            Core.Just ("ChangeToken" Core..= changeToken),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateRateBasedRule where
  type Rs CreateRateBasedRule = CreateRateBasedRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.CreateRateBasedRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRateBasedRuleResponse'
            Core.<$> (x Core..:? "ChangeToken")
            Core.<*> (x Core..:? "Rule")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRateBasedRuleResponse' smart constructor.
data CreateRateBasedRuleResponse = CreateRateBasedRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The 'RateBasedRule' that is returned in the @CreateRateBasedRule@ response.
    rule :: Core.Maybe Types.RateBasedRule,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRateBasedRuleResponse' value with any optional fields omitted.
mkCreateRateBasedRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRateBasedRuleResponse
mkCreateRateBasedRuleResponse responseStatus =
  CreateRateBasedRuleResponse'
    { changeToken = Core.Nothing,
      rule = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrrrsChangeToken :: Lens.Lens' CreateRateBasedRuleResponse (Core.Maybe Types.ChangeToken)
crbrrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED crbrrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The 'RateBasedRule' that is returned in the @CreateRateBasedRule@ response.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrrrsRule :: Lens.Lens' CreateRateBasedRuleResponse (Core.Maybe Types.RateBasedRule)
crbrrrsRule = Lens.field @"rule"
{-# DEPRECATED crbrrrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrrrsResponseStatus :: Lens.Lens' CreateRateBasedRuleResponse Core.Int
crbrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crbrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
