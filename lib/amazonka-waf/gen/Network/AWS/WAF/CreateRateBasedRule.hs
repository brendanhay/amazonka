{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    crbrTags,
    crbrName,
    crbrMetricName,
    crbrRateKey,
    crbrRateLimit,
    crbrChangeToken,

    -- * Destructuring the response
    CreateRateBasedRuleResponse (..),
    mkCreateRateBasedRuleResponse,

    -- ** Response lenses
    crbrrsRule,
    crbrrsChangeToken,
    crbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkCreateRateBasedRule' smart constructor.
data CreateRateBasedRule = CreateRateBasedRule'
  { tags ::
      Lude.Maybe (Lude.NonEmpty Tag),
    name :: Lude.Text,
    metricName :: Lude.Text,
    rateKey :: RateKey,
    rateLimit :: Lude.Natural,
    changeToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRateBasedRule' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'metricName' - A friendly name or description for the metrics for this @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
-- * 'name' - A friendly name or description of the 'RateBasedRule' . You can't change the name of a @RateBasedRule@ after you create it.
-- * 'rateKey' - The field that AWS WAF uses to determine if requests are likely arriving from a single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests that arrive from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
-- * 'rateLimit' - The maximum number of requests, which have an identical value in the field that is specified by @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
-- * 'tags' -
mkCreateRateBasedRule ::
  -- | 'name'
  Lude.Text ->
  -- | 'metricName'
  Lude.Text ->
  -- | 'rateKey'
  RateKey ->
  -- | 'rateLimit'
  Lude.Natural ->
  -- | 'changeToken'
  Lude.Text ->
  CreateRateBasedRule
mkCreateRateBasedRule
  pName_
  pMetricName_
  pRateKey_
  pRateLimit_
  pChangeToken_ =
    CreateRateBasedRule'
      { tags = Lude.Nothing,
        name = pName_,
        metricName = pMetricName_,
        rateKey = pRateKey_,
        rateLimit = pRateLimit_,
        changeToken = pChangeToken_
      }

-- |
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrTags :: Lens.Lens' CreateRateBasedRule (Lude.Maybe (Lude.NonEmpty Tag))
crbrTags = Lens.lens (tags :: CreateRateBasedRule -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateRateBasedRule)
{-# DEPRECATED crbrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A friendly name or description of the 'RateBasedRule' . You can't change the name of a @RateBasedRule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrName :: Lens.Lens' CreateRateBasedRule Lude.Text
crbrName = Lens.lens (name :: CreateRateBasedRule -> Lude.Text) (\s a -> s {name = a} :: CreateRateBasedRule)
{-# DEPRECATED crbrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A friendly name or description for the metrics for this @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RateBasedRule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrMetricName :: Lens.Lens' CreateRateBasedRule Lude.Text
crbrMetricName = Lens.lens (metricName :: CreateRateBasedRule -> Lude.Text) (\s a -> s {metricName = a} :: CreateRateBasedRule)
{-# DEPRECATED crbrMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The field that AWS WAF uses to determine if requests are likely arriving from a single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests that arrive from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
--
-- /Note:/ Consider using 'rateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrRateKey :: Lens.Lens' CreateRateBasedRule RateKey
crbrRateKey = Lens.lens (rateKey :: CreateRateBasedRule -> RateKey) (\s a -> s {rateKey = a} :: CreateRateBasedRule)
{-# DEPRECATED crbrRateKey "Use generic-lens or generic-optics with 'rateKey' instead." #-}

-- | The maximum number of requests, which have an identical value in the field that is specified by @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrRateLimit :: Lens.Lens' CreateRateBasedRule Lude.Natural
crbrRateLimit = Lens.lens (rateLimit :: CreateRateBasedRule -> Lude.Natural) (\s a -> s {rateLimit = a} :: CreateRateBasedRule)
{-# DEPRECATED crbrRateLimit "Use generic-lens or generic-optics with 'rateLimit' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrChangeToken :: Lens.Lens' CreateRateBasedRule Lude.Text
crbrChangeToken = Lens.lens (changeToken :: CreateRateBasedRule -> Lude.Text) (\s a -> s {changeToken = a} :: CreateRateBasedRule)
{-# DEPRECATED crbrChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateRateBasedRule where
  type Rs CreateRateBasedRule = CreateRateBasedRuleResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRateBasedRuleResponse'
            Lude.<$> (x Lude..?> "Rule")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRateBasedRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.CreateRateBasedRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRateBasedRule where
  toJSON CreateRateBasedRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("MetricName" Lude..= metricName),
            Lude.Just ("RateKey" Lude..= rateKey),
            Lude.Just ("RateLimit" Lude..= rateLimit),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateRateBasedRule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRateBasedRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRateBasedRuleResponse' smart constructor.
data CreateRateBasedRuleResponse = CreateRateBasedRuleResponse'
  { rule ::
      Lude.Maybe RateBasedRule,
    changeToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
-- * 'rule' - The 'RateBasedRule' that is returned in the @CreateRateBasedRule@ response.
mkCreateRateBasedRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRateBasedRuleResponse
mkCreateRateBasedRuleResponse pResponseStatus_ =
  CreateRateBasedRuleResponse'
    { rule = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'RateBasedRule' that is returned in the @CreateRateBasedRule@ response.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrrsRule :: Lens.Lens' CreateRateBasedRuleResponse (Lude.Maybe RateBasedRule)
crbrrsRule = Lens.lens (rule :: CreateRateBasedRuleResponse -> Lude.Maybe RateBasedRule) (\s a -> s {rule = a} :: CreateRateBasedRuleResponse)
{-# DEPRECATED crbrrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrrsChangeToken :: Lens.Lens' CreateRateBasedRuleResponse (Lude.Maybe Lude.Text)
crbrrsChangeToken = Lens.lens (changeToken :: CreateRateBasedRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateRateBasedRuleResponse)
{-# DEPRECATED crbrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crbrrsResponseStatus :: Lens.Lens' CreateRateBasedRuleResponse Lude.Int
crbrrsResponseStatus = Lens.lens (responseStatus :: CreateRateBasedRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRateBasedRuleResponse)
{-# DEPRECATED crbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
