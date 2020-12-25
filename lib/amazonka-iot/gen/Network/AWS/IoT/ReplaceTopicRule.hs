{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ReplaceTopicRule operation.
--
-- /See:/ 'mkReplaceTopicRule' smart constructor.
data ReplaceTopicRule = ReplaceTopicRule'
  { -- | The name of the rule.
    ruleName :: Types.RuleName,
    -- | The rule payload.
    topicRulePayload :: Types.TopicRulePayload
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceTopicRule' value with any optional fields omitted.
mkReplaceTopicRule ::
  -- | 'ruleName'
  Types.RuleName ->
  -- | 'topicRulePayload'
  Types.TopicRulePayload ->
  ReplaceTopicRule
mkReplaceTopicRule ruleName topicRulePayload =
  ReplaceTopicRule' {ruleName, topicRulePayload}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrRuleName :: Lens.Lens' ReplaceTopicRule Types.RuleName
rtrRuleName = Lens.field @"ruleName"
{-# DEPRECATED rtrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The rule payload.
--
-- /Note:/ Consider using 'topicRulePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrTopicRulePayload :: Lens.Lens' ReplaceTopicRule Types.TopicRulePayload
rtrTopicRulePayload = Lens.field @"topicRulePayload"
{-# DEPRECATED rtrTopicRulePayload "Use generic-lens or generic-optics with 'topicRulePayload' instead." #-}

instance Core.FromJSON ReplaceTopicRule where
  toJSON ReplaceTopicRule {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("topicRulePayload" Core..= topicRulePayload)]
      )

instance Core.AWSRequest ReplaceTopicRule where
  type Rs ReplaceTopicRule = ReplaceTopicRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath ("/rules/" Core.<> (Core.toText ruleName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull ReplaceTopicRuleResponse'

-- | /See:/ 'mkReplaceTopicRuleResponse' smart constructor.
data ReplaceTopicRuleResponse = ReplaceTopicRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceTopicRuleResponse' value with any optional fields omitted.
mkReplaceTopicRuleResponse ::
  ReplaceTopicRuleResponse
mkReplaceTopicRuleResponse = ReplaceTopicRuleResponse'
