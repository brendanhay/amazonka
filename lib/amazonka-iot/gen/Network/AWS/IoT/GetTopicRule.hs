{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the rule.
module Network.AWS.IoT.GetTopicRule
  ( -- * Creating a request
    GetTopicRule (..),
    mkGetTopicRule,

    -- ** Request lenses
    gtrRuleName,

    -- * Destructuring the response
    GetTopicRuleResponse (..),
    mkGetTopicRuleResponse,

    -- ** Response lenses
    gtrrsRule,
    gtrrsRuleARN,
    gtrrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the GetTopicRule operation.
--
-- /See:/ 'mkGetTopicRule' smart constructor.
newtype GetTopicRule = GetTopicRule' {ruleName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTopicRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the rule.
mkGetTopicRule ::
  -- | 'ruleName'
  Lude.Text ->
  GetTopicRule
mkGetTopicRule pRuleName_ = GetTopicRule' {ruleName = pRuleName_}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrRuleName :: Lens.Lens' GetTopicRule Lude.Text
gtrRuleName = Lens.lens (ruleName :: GetTopicRule -> Lude.Text) (\s a -> s {ruleName = a} :: GetTopicRule)
{-# DEPRECATED gtrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Lude.AWSRequest GetTopicRule where
  type Rs GetTopicRule = GetTopicRuleResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTopicRuleResponse'
            Lude.<$> (x Lude..?> "rule")
            Lude.<*> (x Lude..?> "ruleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTopicRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTopicRule where
  toPath GetTopicRule' {..} =
    Lude.mconcat ["/rules/", Lude.toBS ruleName]

instance Lude.ToQuery GetTopicRule where
  toQuery = Lude.const Lude.mempty

-- | The output from the GetTopicRule operation.
--
-- /See:/ 'mkGetTopicRuleResponse' smart constructor.
data GetTopicRuleResponse = GetTopicRuleResponse'
  { rule ::
      Lude.Maybe TopicRule,
    ruleARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetTopicRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'rule' - The rule.
-- * 'ruleARN' - The rule ARN.
mkGetTopicRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTopicRuleResponse
mkGetTopicRuleResponse pResponseStatus_ =
  GetTopicRuleResponse'
    { rule = Lude.Nothing,
      ruleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsRule :: Lens.Lens' GetTopicRuleResponse (Lude.Maybe TopicRule)
gtrrsRule = Lens.lens (rule :: GetTopicRuleResponse -> Lude.Maybe TopicRule) (\s a -> s {rule = a} :: GetTopicRuleResponse)
{-# DEPRECATED gtrrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The rule ARN.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsRuleARN :: Lens.Lens' GetTopicRuleResponse (Lude.Maybe Lude.Text)
gtrrsRuleARN = Lens.lens (ruleARN :: GetTopicRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {ruleARN = a} :: GetTopicRuleResponse)
{-# DEPRECATED gtrrsRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTopicRuleResponse Lude.Int
gtrrsResponseStatus = Lens.lens (responseStatus :: GetTopicRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTopicRuleResponse)
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
