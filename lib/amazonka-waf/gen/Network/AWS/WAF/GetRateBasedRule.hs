{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRateBasedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RateBasedRule' that is specified by the @RuleId@ that you included in the @GetRateBasedRule@ request.
module Network.AWS.WAF.GetRateBasedRule
  ( -- * Creating a request
    GetRateBasedRule (..),
    mkGetRateBasedRule,

    -- ** Request lenses
    grbrRuleId,

    -- * Destructuring the response
    GetRateBasedRuleResponse (..),
    mkGetRateBasedRuleResponse,

    -- ** Response lenses
    grbrrsRule,
    grbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetRateBasedRule' smart constructor.
newtype GetRateBasedRule = GetRateBasedRule' {ruleId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRateBasedRule' with the minimum fields required to make a request.
--
-- * 'ruleId' - The @RuleId@ of the 'RateBasedRule' that you want to get. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
mkGetRateBasedRule ::
  -- | 'ruleId'
  Lude.Text ->
  GetRateBasedRule
mkGetRateBasedRule pRuleId_ = GetRateBasedRule' {ruleId = pRuleId_}

-- | The @RuleId@ of the 'RateBasedRule' that you want to get. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrRuleId :: Lens.Lens' GetRateBasedRule Lude.Text
grbrRuleId = Lens.lens (ruleId :: GetRateBasedRule -> Lude.Text) (\s a -> s {ruleId = a} :: GetRateBasedRule)
{-# DEPRECATED grbrRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

instance Lude.AWSRequest GetRateBasedRule where
  type Rs GetRateBasedRule = GetRateBasedRuleResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRateBasedRuleResponse'
            Lude.<$> (x Lude..?> "Rule") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRateBasedRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetRateBasedRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRateBasedRule where
  toJSON GetRateBasedRule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RuleId" Lude..= ruleId)])

instance Lude.ToPath GetRateBasedRule where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRateBasedRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRateBasedRuleResponse' smart constructor.
data GetRateBasedRuleResponse = GetRateBasedRuleResponse'
  { rule ::
      Lude.Maybe RateBasedRule,
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

-- | Creates a value of 'GetRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'rule' - Information about the 'RateBasedRule' that you specified in the @GetRateBasedRule@ request.
mkGetRateBasedRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRateBasedRuleResponse
mkGetRateBasedRuleResponse pResponseStatus_ =
  GetRateBasedRuleResponse'
    { rule = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'RateBasedRule' that you specified in the @GetRateBasedRule@ request.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrrsRule :: Lens.Lens' GetRateBasedRuleResponse (Lude.Maybe RateBasedRule)
grbrrsRule = Lens.lens (rule :: GetRateBasedRuleResponse -> Lude.Maybe RateBasedRule) (\s a -> s {rule = a} :: GetRateBasedRuleResponse)
{-# DEPRECATED grbrrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrrsResponseStatus :: Lens.Lens' GetRateBasedRuleResponse Lude.Int
grbrrsResponseStatus = Lens.lens (responseStatus :: GetRateBasedRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRateBasedRuleResponse)
{-# DEPRECATED grbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
