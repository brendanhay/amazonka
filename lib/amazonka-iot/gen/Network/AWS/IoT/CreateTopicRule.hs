{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.
module Network.AWS.IoT.CreateTopicRule
  ( -- * Creating a request
    CreateTopicRule (..),
    mkCreateTopicRule,

    -- ** Request lenses
    ctrRuleName,
    ctrTags,
    ctrTopicRulePayload,

    -- * Destructuring the response
    CreateTopicRuleResponse (..),
    mkCreateTopicRuleResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CreateTopicRule operation.
--
-- /See:/ 'mkCreateTopicRule' smart constructor.
data CreateTopicRule = CreateTopicRule'
  { -- | The name of the rule.
    ruleName :: Lude.Text,
    -- | Metadata which can be used to manage the topic rule.
    tags :: Lude.Maybe Lude.Text,
    -- | The rule payload.
    topicRulePayload :: TopicRulePayload
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTopicRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the rule.
-- * 'tags' - Metadata which can be used to manage the topic rule.
-- * 'topicRulePayload' - The rule payload.
mkCreateTopicRule ::
  -- | 'ruleName'
  Lude.Text ->
  -- | 'topicRulePayload'
  TopicRulePayload ->
  CreateTopicRule
mkCreateTopicRule pRuleName_ pTopicRulePayload_ =
  CreateTopicRule'
    { ruleName = pRuleName_,
      tags = Lude.Nothing,
      topicRulePayload = pTopicRulePayload_
    }

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrRuleName :: Lens.Lens' CreateTopicRule Lude.Text
ctrRuleName = Lens.lens (ruleName :: CreateTopicRule -> Lude.Text) (\s a -> s {ruleName = a} :: CreateTopicRule)
{-# DEPRECATED ctrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | Metadata which can be used to manage the topic rule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrTags :: Lens.Lens' CreateTopicRule (Lude.Maybe Lude.Text)
ctrTags = Lens.lens (tags :: CreateTopicRule -> Lude.Maybe Lude.Text) (\s a -> s {tags = a} :: CreateTopicRule)
{-# DEPRECATED ctrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The rule payload.
--
-- /Note:/ Consider using 'topicRulePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrTopicRulePayload :: Lens.Lens' CreateTopicRule TopicRulePayload
ctrTopicRulePayload = Lens.lens (topicRulePayload :: CreateTopicRule -> TopicRulePayload) (\s a -> s {topicRulePayload = a} :: CreateTopicRule)
{-# DEPRECATED ctrTopicRulePayload "Use generic-lens or generic-optics with 'topicRulePayload' instead." #-}

instance Lude.AWSRequest CreateTopicRule where
  type Rs CreateTopicRule = CreateTopicRuleResponse
  request = Req.postJSON ioTService
  response = Res.receiveNull CreateTopicRuleResponse'

instance Lude.ToHeaders CreateTopicRule where
  toHeaders CreateTopicRule' {..} =
    Lude.mconcat ["x-amz-tagging" Lude.=# tags]

instance Lude.ToJSON CreateTopicRule where
  toJSON CreateTopicRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("topicRulePayload" Lude..= topicRulePayload)]
      )

instance Lude.ToPath CreateTopicRule where
  toPath CreateTopicRule' {..} =
    Lude.mconcat ["/rules/", Lude.toBS ruleName]

instance Lude.ToQuery CreateTopicRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTopicRuleResponse' smart constructor.
data CreateTopicRuleResponse = CreateTopicRuleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTopicRuleResponse' with the minimum fields required to make a request.
mkCreateTopicRuleResponse ::
  CreateTopicRuleResponse
mkCreateTopicRuleResponse = CreateTopicRuleResponse'
