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
-- Module      : Amazonka.IoT.Types.TopicRuleListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TopicRuleListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a rule.
--
-- /See:/ 'newTopicRuleListItem' smart constructor.
data TopicRuleListItem = TopicRuleListItem'
  { -- | The rule ARN.
    ruleArn :: Prelude.Maybe Prelude.Text,
    -- | The pattern for the topic names that apply.
    topicPattern :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the rule was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRuleListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleArn', 'topicRuleListItem_ruleArn' - The rule ARN.
--
-- 'topicPattern', 'topicRuleListItem_topicPattern' - The pattern for the topic names that apply.
--
-- 'ruleName', 'topicRuleListItem_ruleName' - The name of the rule.
--
-- 'createdAt', 'topicRuleListItem_createdAt' - The date and time the rule was created.
--
-- 'ruleDisabled', 'topicRuleListItem_ruleDisabled' - Specifies whether the rule is disabled.
newTopicRuleListItem ::
  TopicRuleListItem
newTopicRuleListItem =
  TopicRuleListItem'
    { ruleArn = Prelude.Nothing,
      topicPattern = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      ruleDisabled = Prelude.Nothing
    }

-- | The rule ARN.
topicRuleListItem_ruleArn :: Lens.Lens' TopicRuleListItem (Prelude.Maybe Prelude.Text)
topicRuleListItem_ruleArn = Lens.lens (\TopicRuleListItem' {ruleArn} -> ruleArn) (\s@TopicRuleListItem' {} a -> s {ruleArn = a} :: TopicRuleListItem)

-- | The pattern for the topic names that apply.
topicRuleListItem_topicPattern :: Lens.Lens' TopicRuleListItem (Prelude.Maybe Prelude.Text)
topicRuleListItem_topicPattern = Lens.lens (\TopicRuleListItem' {topicPattern} -> topicPattern) (\s@TopicRuleListItem' {} a -> s {topicPattern = a} :: TopicRuleListItem)

-- | The name of the rule.
topicRuleListItem_ruleName :: Lens.Lens' TopicRuleListItem (Prelude.Maybe Prelude.Text)
topicRuleListItem_ruleName = Lens.lens (\TopicRuleListItem' {ruleName} -> ruleName) (\s@TopicRuleListItem' {} a -> s {ruleName = a} :: TopicRuleListItem)

-- | The date and time the rule was created.
topicRuleListItem_createdAt :: Lens.Lens' TopicRuleListItem (Prelude.Maybe Prelude.UTCTime)
topicRuleListItem_createdAt = Lens.lens (\TopicRuleListItem' {createdAt} -> createdAt) (\s@TopicRuleListItem' {} a -> s {createdAt = a} :: TopicRuleListItem) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the rule is disabled.
topicRuleListItem_ruleDisabled :: Lens.Lens' TopicRuleListItem (Prelude.Maybe Prelude.Bool)
topicRuleListItem_ruleDisabled = Lens.lens (\TopicRuleListItem' {ruleDisabled} -> ruleDisabled) (\s@TopicRuleListItem' {} a -> s {ruleDisabled = a} :: TopicRuleListItem)

instance Core.FromJSON TopicRuleListItem where
  parseJSON =
    Core.withObject
      "TopicRuleListItem"
      ( \x ->
          TopicRuleListItem'
            Prelude.<$> (x Core..:? "ruleArn")
            Prelude.<*> (x Core..:? "topicPattern")
            Prelude.<*> (x Core..:? "ruleName")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "ruleDisabled")
      )

instance Prelude.Hashable TopicRuleListItem where
  hashWithSalt _salt TopicRuleListItem' {..} =
    _salt `Prelude.hashWithSalt` ruleArn
      `Prelude.hashWithSalt` topicPattern
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` ruleDisabled

instance Prelude.NFData TopicRuleListItem where
  rnf TopicRuleListItem' {..} =
    Prelude.rnf ruleArn
      `Prelude.seq` Prelude.rnf topicPattern
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ruleDisabled
