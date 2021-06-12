{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleListItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a rule.
--
-- /See:/ 'newTopicRuleListItem' smart constructor.
data TopicRuleListItem = TopicRuleListItem'
  { -- | The name of the rule.
    ruleName :: Core.Maybe Core.Text,
    -- | The rule ARN.
    ruleArn :: Core.Maybe Core.Text,
    -- | The date and time the rule was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The pattern for the topic names that apply.
    topicPattern :: Core.Maybe Core.Text,
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TopicRuleListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'topicRuleListItem_ruleName' - The name of the rule.
--
-- 'ruleArn', 'topicRuleListItem_ruleArn' - The rule ARN.
--
-- 'createdAt', 'topicRuleListItem_createdAt' - The date and time the rule was created.
--
-- 'topicPattern', 'topicRuleListItem_topicPattern' - The pattern for the topic names that apply.
--
-- 'ruleDisabled', 'topicRuleListItem_ruleDisabled' - Specifies whether the rule is disabled.
newTopicRuleListItem ::
  TopicRuleListItem
newTopicRuleListItem =
  TopicRuleListItem'
    { ruleName = Core.Nothing,
      ruleArn = Core.Nothing,
      createdAt = Core.Nothing,
      topicPattern = Core.Nothing,
      ruleDisabled = Core.Nothing
    }

-- | The name of the rule.
topicRuleListItem_ruleName :: Lens.Lens' TopicRuleListItem (Core.Maybe Core.Text)
topicRuleListItem_ruleName = Lens.lens (\TopicRuleListItem' {ruleName} -> ruleName) (\s@TopicRuleListItem' {} a -> s {ruleName = a} :: TopicRuleListItem)

-- | The rule ARN.
topicRuleListItem_ruleArn :: Lens.Lens' TopicRuleListItem (Core.Maybe Core.Text)
topicRuleListItem_ruleArn = Lens.lens (\TopicRuleListItem' {ruleArn} -> ruleArn) (\s@TopicRuleListItem' {} a -> s {ruleArn = a} :: TopicRuleListItem)

-- | The date and time the rule was created.
topicRuleListItem_createdAt :: Lens.Lens' TopicRuleListItem (Core.Maybe Core.UTCTime)
topicRuleListItem_createdAt = Lens.lens (\TopicRuleListItem' {createdAt} -> createdAt) (\s@TopicRuleListItem' {} a -> s {createdAt = a} :: TopicRuleListItem) Core.. Lens.mapping Core._Time

-- | The pattern for the topic names that apply.
topicRuleListItem_topicPattern :: Lens.Lens' TopicRuleListItem (Core.Maybe Core.Text)
topicRuleListItem_topicPattern = Lens.lens (\TopicRuleListItem' {topicPattern} -> topicPattern) (\s@TopicRuleListItem' {} a -> s {topicPattern = a} :: TopicRuleListItem)

-- | Specifies whether the rule is disabled.
topicRuleListItem_ruleDisabled :: Lens.Lens' TopicRuleListItem (Core.Maybe Core.Bool)
topicRuleListItem_ruleDisabled = Lens.lens (\TopicRuleListItem' {ruleDisabled} -> ruleDisabled) (\s@TopicRuleListItem' {} a -> s {ruleDisabled = a} :: TopicRuleListItem)

instance Core.FromJSON TopicRuleListItem where
  parseJSON =
    Core.withObject
      "TopicRuleListItem"
      ( \x ->
          TopicRuleListItem'
            Core.<$> (x Core..:? "ruleName")
            Core.<*> (x Core..:? "ruleArn")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "topicPattern")
            Core.<*> (x Core..:? "ruleDisabled")
      )

instance Core.Hashable TopicRuleListItem

instance Core.NFData TopicRuleListItem
