-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleListItem
  ( TopicRuleListItem (..),

    -- * Smart constructor
    mkTopicRuleListItem,

    -- * Lenses
    trliCreatedAt,
    trliRuleDisabled,
    trliRuleName,
    trliRuleARN,
    trliTopicPattern,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a rule.
--
-- /See:/ 'mkTopicRuleListItem' smart constructor.
data TopicRuleListItem = TopicRuleListItem'
  { createdAt ::
      Lude.Maybe Lude.Timestamp,
    ruleDisabled :: Lude.Maybe Lude.Bool,
    ruleName :: Lude.Maybe Lude.Text,
    ruleARN :: Lude.Maybe Lude.Text,
    topicPattern :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicRuleListItem' with the minimum fields required to make a request.
--
-- * 'createdAt' - The date and time the rule was created.
-- * 'ruleARN' - The rule ARN.
-- * 'ruleDisabled' - Specifies whether the rule is disabled.
-- * 'ruleName' - The name of the rule.
-- * 'topicPattern' - The pattern for the topic names that apply.
mkTopicRuleListItem ::
  TopicRuleListItem
mkTopicRuleListItem =
  TopicRuleListItem'
    { createdAt = Lude.Nothing,
      ruleDisabled = Lude.Nothing,
      ruleName = Lude.Nothing,
      ruleARN = Lude.Nothing,
      topicPattern = Lude.Nothing
    }

-- | The date and time the rule was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliCreatedAt :: Lens.Lens' TopicRuleListItem (Lude.Maybe Lude.Timestamp)
trliCreatedAt = Lens.lens (createdAt :: TopicRuleListItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: TopicRuleListItem)
{-# DEPRECATED trliCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Specifies whether the rule is disabled.
--
-- /Note:/ Consider using 'ruleDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliRuleDisabled :: Lens.Lens' TopicRuleListItem (Lude.Maybe Lude.Bool)
trliRuleDisabled = Lens.lens (ruleDisabled :: TopicRuleListItem -> Lude.Maybe Lude.Bool) (\s a -> s {ruleDisabled = a} :: TopicRuleListItem)
{-# DEPRECATED trliRuleDisabled "Use generic-lens or generic-optics with 'ruleDisabled' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliRuleName :: Lens.Lens' TopicRuleListItem (Lude.Maybe Lude.Text)
trliRuleName = Lens.lens (ruleName :: TopicRuleListItem -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: TopicRuleListItem)
{-# DEPRECATED trliRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The rule ARN.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliRuleARN :: Lens.Lens' TopicRuleListItem (Lude.Maybe Lude.Text)
trliRuleARN = Lens.lens (ruleARN :: TopicRuleListItem -> Lude.Maybe Lude.Text) (\s a -> s {ruleARN = a} :: TopicRuleListItem)
{-# DEPRECATED trliRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

-- | The pattern for the topic names that apply.
--
-- /Note:/ Consider using 'topicPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trliTopicPattern :: Lens.Lens' TopicRuleListItem (Lude.Maybe Lude.Text)
trliTopicPattern = Lens.lens (topicPattern :: TopicRuleListItem -> Lude.Maybe Lude.Text) (\s a -> s {topicPattern = a} :: TopicRuleListItem)
{-# DEPRECATED trliTopicPattern "Use generic-lens or generic-optics with 'topicPattern' instead." #-}

instance Lude.FromJSON TopicRuleListItem where
  parseJSON =
    Lude.withObject
      "TopicRuleListItem"
      ( \x ->
          TopicRuleListItem'
            Lude.<$> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "ruleDisabled")
            Lude.<*> (x Lude..:? "ruleName")
            Lude.<*> (x Lude..:? "ruleArn")
            Lude.<*> (x Lude..:? "topicPattern")
      )
