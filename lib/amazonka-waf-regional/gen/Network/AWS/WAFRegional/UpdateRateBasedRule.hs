{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateRateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'Predicate' objects in a rule and updates the @RateLimit@ in the rule.
--
-- Each @Predicate@ object identifies a predicate, such as a 'ByteMatchSet' or an 'IPSet' , that specifies the web requests that you want to block or count. The @RateLimit@ specifies the number of requests every five minutes that triggers the rule.
-- If you add more than one predicate to a @RateBasedRule@ , a request must match all the predicates and exceed the @RateLimit@ to be counted or blocked. For example, suppose you add the following to a @RateBasedRule@ :
--
--     * An @IPSet@ that matches the IP address @192.0.2.44/32@
--
--
--     * A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
--
-- Further, you specify a @RateLimit@ of 1,000.
-- You then add the @RateBasedRule@ to a @WebACL@ and specify that you want to block requests that satisfy the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header in the request must contain the value @BadBot@ . Further, requests that match these two conditions much be received at a rate of more than 1,000 every five minutes. If the rate drops below this limit, AWS WAF no longer blocks the requests.
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
module Network.AWS.WAFRegional.UpdateRateBasedRule
  ( -- * Creating a request
    UpdateRateBasedRule (..),
    mkUpdateRateBasedRule,

    -- ** Request lenses
    urbrRateLimit,
    urbrRuleId,
    urbrUpdates,
    urbrChangeToken,

    -- * Destructuring the response
    UpdateRateBasedRuleResponse (..),
    mkUpdateRateBasedRuleResponse,

    -- ** Response lenses
    urbrrsChangeToken,
    urbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkUpdateRateBasedRule' smart constructor.
data UpdateRateBasedRule = UpdateRateBasedRule'
  { -- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
    rateLimit :: Lude.Natural,
    -- | The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is returned by @CreateRateBasedRule@ and by 'ListRateBasedRules' .
    ruleId :: Lude.Text,
    -- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'RateBasedRule' .
    updates :: [RuleUpdate],
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRateBasedRule' with the minimum fields required to make a request.
--
-- * 'rateLimit' - The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
-- * 'ruleId' - The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is returned by @CreateRateBasedRule@ and by 'ListRateBasedRules' .
-- * 'updates' - An array of @RuleUpdate@ objects that you want to insert into or delete from a 'RateBasedRule' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkUpdateRateBasedRule ::
  -- | 'rateLimit'
  Lude.Natural ->
  -- | 'ruleId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateRateBasedRule
mkUpdateRateBasedRule pRateLimit_ pRuleId_ pChangeToken_ =
  UpdateRateBasedRule'
    { rateLimit = pRateLimit_,
      ruleId = pRuleId_,
      updates = Lude.mempty,
      changeToken = pChangeToken_
    }

-- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrRateLimit :: Lens.Lens' UpdateRateBasedRule Lude.Natural
urbrRateLimit = Lens.lens (rateLimit :: UpdateRateBasedRule -> Lude.Natural) (\s a -> s {rateLimit = a} :: UpdateRateBasedRule)
{-# DEPRECATED urbrRateLimit "Use generic-lens or generic-optics with 'rateLimit' instead." #-}

-- | The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is returned by @CreateRateBasedRule@ and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrRuleId :: Lens.Lens' UpdateRateBasedRule Lude.Text
urbrRuleId = Lens.lens (ruleId :: UpdateRateBasedRule -> Lude.Text) (\s a -> s {ruleId = a} :: UpdateRateBasedRule)
{-# DEPRECATED urbrRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'RateBasedRule' .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrUpdates :: Lens.Lens' UpdateRateBasedRule [RuleUpdate]
urbrUpdates = Lens.lens (updates :: UpdateRateBasedRule -> [RuleUpdate]) (\s a -> s {updates = a} :: UpdateRateBasedRule)
{-# DEPRECATED urbrUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrChangeToken :: Lens.Lens' UpdateRateBasedRule Lude.Text
urbrChangeToken = Lens.lens (changeToken :: UpdateRateBasedRule -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateRateBasedRule)
{-# DEPRECATED urbrChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateRateBasedRule where
  type Rs UpdateRateBasedRule = UpdateRateBasedRuleResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRateBasedRuleResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRateBasedRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.UpdateRateBasedRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRateBasedRule where
  toJSON UpdateRateBasedRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RateLimit" Lude..= rateLimit),
            Lude.Just ("RuleId" Lude..= ruleId),
            Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateRateBasedRule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRateBasedRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRateBasedRuleResponse' smart constructor.
data UpdateRateBasedRuleResponse = UpdateRateBasedRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateRateBasedRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRateBasedRuleResponse
mkUpdateRateBasedRuleResponse pResponseStatus_ =
  UpdateRateBasedRuleResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrrsChangeToken :: Lens.Lens' UpdateRateBasedRuleResponse (Lude.Maybe Lude.Text)
urbrrsChangeToken = Lens.lens (changeToken :: UpdateRateBasedRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateRateBasedRuleResponse)
{-# DEPRECATED urbrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urbrrsResponseStatus :: Lens.Lens' UpdateRateBasedRuleResponse Lude.Int
urbrrsResponseStatus = Lens.lens (responseStatus :: UpdateRateBasedRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRateBasedRuleResponse)
{-# DEPRECATED urbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
