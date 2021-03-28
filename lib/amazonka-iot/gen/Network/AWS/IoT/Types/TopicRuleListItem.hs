{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TopicRuleListItem
  ( TopicRuleListItem (..)
  -- * Smart constructor
  , mkTopicRuleListItem
  -- * Lenses
  , trliCreatedAt
  , trliRuleArn
  , trliRuleDisabled
  , trliRuleName
  , trliTopicPattern
  ) where

import qualified Network.AWS.IoT.Types.RuleArn as Types
import qualified Network.AWS.IoT.Types.RuleName as Types
import qualified Network.AWS.IoT.Types.TopicPattern as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a rule.
--
-- /See:/ 'mkTopicRuleListItem' smart constructor.
data TopicRuleListItem = TopicRuleListItem'
  { createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the rule was created.
  , ruleArn :: Core.Maybe Types.RuleArn
    -- ^ The rule ARN.
  , ruleDisabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the rule is disabled.
  , ruleName :: Core.Maybe Types.RuleName
    -- ^ The name of the rule.
  , topicPattern :: Core.Maybe Types.TopicPattern
    -- ^ The pattern for the topic names that apply.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TopicRuleListItem' value with any optional fields omitted.
mkTopicRuleListItem
    :: TopicRuleListItem
mkTopicRuleListItem
  = TopicRuleListItem'{createdAt = Core.Nothing,
                       ruleArn = Core.Nothing, ruleDisabled = Core.Nothing,
                       ruleName = Core.Nothing, topicPattern = Core.Nothing}

-- | The date and time the rule was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliCreatedAt :: Lens.Lens' TopicRuleListItem (Core.Maybe Core.NominalDiffTime)
trliCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE trliCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The rule ARN.
--
-- /Note:/ Consider using 'ruleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliRuleArn :: Lens.Lens' TopicRuleListItem (Core.Maybe Types.RuleArn)
trliRuleArn = Lens.field @"ruleArn"
{-# INLINEABLE trliRuleArn #-}
{-# DEPRECATED ruleArn "Use generic-lens or generic-optics with 'ruleArn' instead"  #-}

-- | Specifies whether the rule is disabled.
--
-- /Note:/ Consider using 'ruleDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliRuleDisabled :: Lens.Lens' TopicRuleListItem (Core.Maybe Core.Bool)
trliRuleDisabled = Lens.field @"ruleDisabled"
{-# INLINEABLE trliRuleDisabled #-}
{-# DEPRECATED ruleDisabled "Use generic-lens or generic-optics with 'ruleDisabled' instead"  #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliRuleName :: Lens.Lens' TopicRuleListItem (Core.Maybe Types.RuleName)
trliRuleName = Lens.field @"ruleName"
{-# INLINEABLE trliRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

-- | The pattern for the topic names that apply.
--
-- /Note:/ Consider using 'topicPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliTopicPattern :: Lens.Lens' TopicRuleListItem (Core.Maybe Types.TopicPattern)
trliTopicPattern = Lens.field @"topicPattern"
{-# INLINEABLE trliTopicPattern #-}
{-# DEPRECATED topicPattern "Use generic-lens or generic-optics with 'topicPattern' instead"  #-}

instance Core.FromJSON TopicRuleListItem where
        parseJSON
          = Core.withObject "TopicRuleListItem" Core.$
              \ x ->
                TopicRuleListItem' Core.<$>
                  (x Core..:? "createdAt") Core.<*> x Core..:? "ruleArn" Core.<*>
                    x Core..:? "ruleDisabled"
                    Core.<*> x Core..:? "ruleName"
                    Core.<*> x Core..:? "topicPattern"
