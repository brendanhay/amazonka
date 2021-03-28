{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTopicRule (..)
    , mkCreateTopicRule
    -- ** Request lenses
    , ctrRuleName
    , ctrTopicRulePayload
    , ctrTags

    -- * Destructuring the response
    , CreateTopicRuleResponse (..)
    , mkCreateTopicRuleResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateTopicRule operation.
--
-- /See:/ 'mkCreateTopicRule' smart constructor.
data CreateTopicRule = CreateTopicRule'
  { ruleName :: Types.RuleName
    -- ^ The name of the rule.
  , topicRulePayload :: Types.TopicRulePayload
    -- ^ The rule payload.
  , tags :: Core.Maybe Core.Text
    -- ^ Metadata which can be used to manage the topic rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTopicRule' value with any optional fields omitted.
mkCreateTopicRule
    :: Types.RuleName -- ^ 'ruleName'
    -> Types.TopicRulePayload -- ^ 'topicRulePayload'
    -> CreateTopicRule
mkCreateTopicRule ruleName topicRulePayload
  = CreateTopicRule'{ruleName, topicRulePayload, tags = Core.Nothing}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrRuleName :: Lens.Lens' CreateTopicRule Types.RuleName
ctrRuleName = Lens.field @"ruleName"
{-# INLINEABLE ctrRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

-- | The rule payload.
--
-- /Note:/ Consider using 'topicRulePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrTopicRulePayload :: Lens.Lens' CreateTopicRule Types.TopicRulePayload
ctrTopicRulePayload = Lens.field @"topicRulePayload"
{-# INLINEABLE ctrTopicRulePayload #-}
{-# DEPRECATED topicRulePayload "Use generic-lens or generic-optics with 'topicRulePayload' instead"  #-}

-- | Metadata which can be used to manage the topic rule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrTags :: Lens.Lens' CreateTopicRule (Core.Maybe Core.Text)
ctrTags = Lens.field @"tags"
{-# INLINEABLE ctrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateTopicRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTopicRule where
        toHeaders CreateTopicRule{..} = Core.toHeaders "x-amz-tagging" tags

instance Core.FromJSON CreateTopicRule where
        toJSON CreateTopicRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("topicRulePayload" Core..= topicRulePayload)])

instance Core.AWSRequest CreateTopicRule where
        type Rs CreateTopicRule = CreateTopicRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/rules/" Core.<> Core.toText ruleName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CreateTopicRuleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTopicRuleResponse' smart constructor.
data CreateTopicRuleResponse = CreateTopicRuleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTopicRuleResponse' value with any optional fields omitted.
mkCreateTopicRuleResponse
    :: CreateTopicRuleResponse
mkCreateTopicRuleResponse = CreateTopicRuleResponse'
