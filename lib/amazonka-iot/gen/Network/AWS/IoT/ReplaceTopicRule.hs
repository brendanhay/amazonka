{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ReplaceTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the rule. You must specify all parameters for the new rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.
module Network.AWS.IoT.ReplaceTopicRule
  ( -- * Creating a request
    ReplaceTopicRule (..),
    mkReplaceTopicRule,

    -- ** Request lenses
    rtrRuleName,
    rtrTopicRulePayload,

    -- * Destructuring the response
    ReplaceTopicRuleResponse (..),
    mkReplaceTopicRuleResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ReplaceTopicRule operation.
--
-- /See:/ 'mkReplaceTopicRule' smart constructor.
data ReplaceTopicRule = ReplaceTopicRule'
  { ruleName :: Lude.Text,
    topicRulePayload :: TopicRulePayload
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceTopicRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the rule.
-- * 'topicRulePayload' - The rule payload.
mkReplaceTopicRule ::
  -- | 'ruleName'
  Lude.Text ->
  -- | 'topicRulePayload'
  TopicRulePayload ->
  ReplaceTopicRule
mkReplaceTopicRule pRuleName_ pTopicRulePayload_ =
  ReplaceTopicRule'
    { ruleName = pRuleName_,
      topicRulePayload = pTopicRulePayload_
    }

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrRuleName :: Lens.Lens' ReplaceTopicRule Lude.Text
rtrRuleName = Lens.lens (ruleName :: ReplaceTopicRule -> Lude.Text) (\s a -> s {ruleName = a} :: ReplaceTopicRule)
{-# DEPRECATED rtrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The rule payload.
--
-- /Note:/ Consider using 'topicRulePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrTopicRulePayload :: Lens.Lens' ReplaceTopicRule TopicRulePayload
rtrTopicRulePayload = Lens.lens (topicRulePayload :: ReplaceTopicRule -> TopicRulePayload) (\s a -> s {topicRulePayload = a} :: ReplaceTopicRule)
{-# DEPRECATED rtrTopicRulePayload "Use generic-lens or generic-optics with 'topicRulePayload' instead." #-}

instance Lude.AWSRequest ReplaceTopicRule where
  type Rs ReplaceTopicRule = ReplaceTopicRuleResponse
  request = Req.patchJSON ioTService
  response = Res.receiveNull ReplaceTopicRuleResponse'

instance Lude.ToHeaders ReplaceTopicRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ReplaceTopicRule where
  toJSON ReplaceTopicRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("topicRulePayload" Lude..= topicRulePayload)]
      )

instance Lude.ToPath ReplaceTopicRule where
  toPath ReplaceTopicRule' {..} =
    Lude.mconcat ["/rules/", Lude.toBS ruleName]

instance Lude.ToQuery ReplaceTopicRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkReplaceTopicRuleResponse' smart constructor.
data ReplaceTopicRuleResponse = ReplaceTopicRuleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceTopicRuleResponse' with the minimum fields required to make a request.
mkReplaceTopicRuleResponse ::
  ReplaceTopicRuleResponse
mkReplaceTopicRuleResponse = ReplaceTopicRuleResponse'
