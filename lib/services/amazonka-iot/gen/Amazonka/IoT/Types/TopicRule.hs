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
-- Module      : Amazonka.IoT.Types.TopicRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TopicRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Describes a rule.
--
-- /See:/ 'newTopicRule' smart constructor.
data TopicRule = TopicRule'
  { -- | The actions associated with the rule.
    actions :: Prelude.Maybe [Action],
    -- | The version of the SQL rules engine to use when evaluating the rule.
    awsIotSqlVersion :: Prelude.Maybe Prelude.Text,
    -- | The date and time the rule was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of the rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The action to perform when an error occurs.
    errorAction :: Prelude.Maybe Action,
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The SQL statement used to query the topic. When using a SQL query with
    -- multiple lines, be sure to escape the newline characters.
    sql :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'topicRule_actions' - The actions associated with the rule.
--
-- 'awsIotSqlVersion', 'topicRule_awsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- 'createdAt', 'topicRule_createdAt' - The date and time the rule was created.
--
-- 'description', 'topicRule_description' - The description of the rule.
--
-- 'errorAction', 'topicRule_errorAction' - The action to perform when an error occurs.
--
-- 'ruleDisabled', 'topicRule_ruleDisabled' - Specifies whether the rule is disabled.
--
-- 'ruleName', 'topicRule_ruleName' - The name of the rule.
--
-- 'sql', 'topicRule_sql' - The SQL statement used to query the topic. When using a SQL query with
-- multiple lines, be sure to escape the newline characters.
newTopicRule ::
  TopicRule
newTopicRule =
  TopicRule'
    { actions = Prelude.Nothing,
      awsIotSqlVersion = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      errorAction = Prelude.Nothing,
      ruleDisabled = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      sql = Prelude.Nothing
    }

-- | The actions associated with the rule.
topicRule_actions :: Lens.Lens' TopicRule (Prelude.Maybe [Action])
topicRule_actions = Lens.lens (\TopicRule' {actions} -> actions) (\s@TopicRule' {} a -> s {actions = a} :: TopicRule) Prelude.. Lens.mapping Lens.coerced

-- | The version of the SQL rules engine to use when evaluating the rule.
topicRule_awsIotSqlVersion :: Lens.Lens' TopicRule (Prelude.Maybe Prelude.Text)
topicRule_awsIotSqlVersion = Lens.lens (\TopicRule' {awsIotSqlVersion} -> awsIotSqlVersion) (\s@TopicRule' {} a -> s {awsIotSqlVersion = a} :: TopicRule)

-- | The date and time the rule was created.
topicRule_createdAt :: Lens.Lens' TopicRule (Prelude.Maybe Prelude.UTCTime)
topicRule_createdAt = Lens.lens (\TopicRule' {createdAt} -> createdAt) (\s@TopicRule' {} a -> s {createdAt = a} :: TopicRule) Prelude.. Lens.mapping Data._Time

-- | The description of the rule.
topicRule_description :: Lens.Lens' TopicRule (Prelude.Maybe Prelude.Text)
topicRule_description = Lens.lens (\TopicRule' {description} -> description) (\s@TopicRule' {} a -> s {description = a} :: TopicRule)

-- | The action to perform when an error occurs.
topicRule_errorAction :: Lens.Lens' TopicRule (Prelude.Maybe Action)
topicRule_errorAction = Lens.lens (\TopicRule' {errorAction} -> errorAction) (\s@TopicRule' {} a -> s {errorAction = a} :: TopicRule)

-- | Specifies whether the rule is disabled.
topicRule_ruleDisabled :: Lens.Lens' TopicRule (Prelude.Maybe Prelude.Bool)
topicRule_ruleDisabled = Lens.lens (\TopicRule' {ruleDisabled} -> ruleDisabled) (\s@TopicRule' {} a -> s {ruleDisabled = a} :: TopicRule)

-- | The name of the rule.
topicRule_ruleName :: Lens.Lens' TopicRule (Prelude.Maybe Prelude.Text)
topicRule_ruleName = Lens.lens (\TopicRule' {ruleName} -> ruleName) (\s@TopicRule' {} a -> s {ruleName = a} :: TopicRule)

-- | The SQL statement used to query the topic. When using a SQL query with
-- multiple lines, be sure to escape the newline characters.
topicRule_sql :: Lens.Lens' TopicRule (Prelude.Maybe Prelude.Text)
topicRule_sql = Lens.lens (\TopicRule' {sql} -> sql) (\s@TopicRule' {} a -> s {sql = a} :: TopicRule)

instance Data.FromJSON TopicRule where
  parseJSON =
    Data.withObject
      "TopicRule"
      ( \x ->
          TopicRule'
            Prelude.<$> (x Data..:? "actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "awsIotSqlVersion")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "errorAction")
            Prelude.<*> (x Data..:? "ruleDisabled")
            Prelude.<*> (x Data..:? "ruleName")
            Prelude.<*> (x Data..:? "sql")
      )

instance Prelude.Hashable TopicRule where
  hashWithSalt _salt TopicRule' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` awsIotSqlVersion
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` errorAction
      `Prelude.hashWithSalt` ruleDisabled
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` sql

instance Prelude.NFData TopicRule where
  rnf TopicRule' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf awsIotSqlVersion
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorAction
      `Prelude.seq` Prelude.rnf ruleDisabled
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf sql
