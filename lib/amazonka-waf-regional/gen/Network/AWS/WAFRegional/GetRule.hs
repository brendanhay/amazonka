{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'Rule' that is specified by the @RuleId@ that you included in the @GetRule@ request.
module Network.AWS.WAFRegional.GetRule
  ( -- * Creating a request
    GetRule (..),
    mkGetRule,

    -- ** Request lenses
    grRuleId,

    -- * Destructuring the response
    GetRuleResponse (..),
    mkGetRuleResponse,

    -- ** Response lenses
    grrsRule,
    grrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetRule' smart constructor.
newtype GetRule = GetRule'
  { -- | The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
    ruleId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRule' with the minimum fields required to make a request.
--
-- * 'ruleId' - The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
mkGetRule ::
  -- | 'ruleId'
  Lude.Text ->
  GetRule
mkGetRule pRuleId_ = GetRule' {ruleId = pRuleId_}

-- | The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRuleId :: Lens.Lens' GetRule Lude.Text
grRuleId = Lens.lens (ruleId :: GetRule -> Lude.Text) (\s a -> s {ruleId = a} :: GetRule)
{-# DEPRECATED grRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

instance Lude.AWSRequest GetRule where
  type Rs GetRule = GetRuleResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRuleResponse'
            Lude.<$> (x Lude..?> "Rule") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.GetRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRule where
  toJSON GetRule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RuleId" Lude..= ruleId)])

instance Lude.ToPath GetRule where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { -- | Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:
    --
    --
    --     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@
    --
    --
    --     * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@
    rule :: Lude.Maybe Rule,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRuleResponse' with the minimum fields required to make a request.
--
-- * 'rule' - Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:
--
--
--     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@
--
--
--     * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@
--
--
-- * 'responseStatus' - The response status code.
mkGetRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRuleResponse
mkGetRuleResponse pResponseStatus_ =
  GetRuleResponse'
    { rule = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:
--
--
--     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@
--
--
--     * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@
--
--
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRule :: Lens.Lens' GetRuleResponse (Lude.Maybe Rule)
grrsRule = Lens.lens (rule :: GetRuleResponse -> Lude.Maybe Rule) (\s a -> s {rule = a} :: GetRuleResponse)
{-# DEPRECATED grrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRuleResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRuleResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
