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
-- Module      : Network.AWS.IoT.Types.TopicRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRule where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Action
import qualified Network.AWS.Lens as Lens

-- | Describes a rule.
--
-- /See:/ 'newTopicRule' smart constructor.
data TopicRule = TopicRule'
  { -- | The name of the rule.
    ruleName :: Core.Maybe Core.Text,
    -- | The action to perform when an error occurs.
    errorAction :: Core.Maybe Action,
    -- | The version of the SQL rules engine to use when evaluating the rule.
    awsIotSqlVersion :: Core.Maybe Core.Text,
    -- | The date and time the rule was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The actions associated with the rule.
    actions :: Core.Maybe [Action],
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Core.Maybe Core.Bool,
    -- | The description of the rule.
    description :: Core.Maybe Core.Text,
    -- | The SQL statement used to query the topic. When using a SQL query with
    -- multiple lines, be sure to escape the newline characters.
    sql :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'topicRule_ruleName' - The name of the rule.
--
-- 'errorAction', 'topicRule_errorAction' - The action to perform when an error occurs.
--
-- 'awsIotSqlVersion', 'topicRule_awsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- 'createdAt', 'topicRule_createdAt' - The date and time the rule was created.
--
-- 'actions', 'topicRule_actions' - The actions associated with the rule.
--
-- 'ruleDisabled', 'topicRule_ruleDisabled' - Specifies whether the rule is disabled.
--
-- 'description', 'topicRule_description' - The description of the rule.
--
-- 'sql', 'topicRule_sql' - The SQL statement used to query the topic. When using a SQL query with
-- multiple lines, be sure to escape the newline characters.
newTopicRule ::
  TopicRule
newTopicRule =
  TopicRule'
    { ruleName = Core.Nothing,
      errorAction = Core.Nothing,
      awsIotSqlVersion = Core.Nothing,
      createdAt = Core.Nothing,
      actions = Core.Nothing,
      ruleDisabled = Core.Nothing,
      description = Core.Nothing,
      sql = Core.Nothing
    }

-- | The name of the rule.
topicRule_ruleName :: Lens.Lens' TopicRule (Core.Maybe Core.Text)
topicRule_ruleName = Lens.lens (\TopicRule' {ruleName} -> ruleName) (\s@TopicRule' {} a -> s {ruleName = a} :: TopicRule)

-- | The action to perform when an error occurs.
topicRule_errorAction :: Lens.Lens' TopicRule (Core.Maybe Action)
topicRule_errorAction = Lens.lens (\TopicRule' {errorAction} -> errorAction) (\s@TopicRule' {} a -> s {errorAction = a} :: TopicRule)

-- | The version of the SQL rules engine to use when evaluating the rule.
topicRule_awsIotSqlVersion :: Lens.Lens' TopicRule (Core.Maybe Core.Text)
topicRule_awsIotSqlVersion = Lens.lens (\TopicRule' {awsIotSqlVersion} -> awsIotSqlVersion) (\s@TopicRule' {} a -> s {awsIotSqlVersion = a} :: TopicRule)

-- | The date and time the rule was created.
topicRule_createdAt :: Lens.Lens' TopicRule (Core.Maybe Core.UTCTime)
topicRule_createdAt = Lens.lens (\TopicRule' {createdAt} -> createdAt) (\s@TopicRule' {} a -> s {createdAt = a} :: TopicRule) Core.. Lens.mapping Core._Time

-- | The actions associated with the rule.
topicRule_actions :: Lens.Lens' TopicRule (Core.Maybe [Action])
topicRule_actions = Lens.lens (\TopicRule' {actions} -> actions) (\s@TopicRule' {} a -> s {actions = a} :: TopicRule) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether the rule is disabled.
topicRule_ruleDisabled :: Lens.Lens' TopicRule (Core.Maybe Core.Bool)
topicRule_ruleDisabled = Lens.lens (\TopicRule' {ruleDisabled} -> ruleDisabled) (\s@TopicRule' {} a -> s {ruleDisabled = a} :: TopicRule)

-- | The description of the rule.
topicRule_description :: Lens.Lens' TopicRule (Core.Maybe Core.Text)
topicRule_description = Lens.lens (\TopicRule' {description} -> description) (\s@TopicRule' {} a -> s {description = a} :: TopicRule)

-- | The SQL statement used to query the topic. When using a SQL query with
-- multiple lines, be sure to escape the newline characters.
topicRule_sql :: Lens.Lens' TopicRule (Core.Maybe Core.Text)
topicRule_sql = Lens.lens (\TopicRule' {sql} -> sql) (\s@TopicRule' {} a -> s {sql = a} :: TopicRule)

instance Core.FromJSON TopicRule where
  parseJSON =
    Core.withObject
      "TopicRule"
      ( \x ->
          TopicRule'
            Core.<$> (x Core..:? "ruleName")
            Core.<*> (x Core..:? "errorAction")
            Core.<*> (x Core..:? "awsIotSqlVersion")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "actions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ruleDisabled")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "sql")
      )

instance Core.Hashable TopicRule

instance Core.NFData TopicRule
