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
-- Module      : Amazonka.IoT.Types.TopicRulePayload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TopicRulePayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Describes a rule.
--
-- /See:/ 'newTopicRulePayload' smart constructor.
data TopicRulePayload = TopicRulePayload'
  { -- | The version of the SQL rules engine to use when evaluating the rule.
    awsIotSqlVersion :: Prelude.Maybe Prelude.Text,
    -- | The description of the rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The action to take when an error occurs.
    errorAction :: Prelude.Maybe Action,
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The SQL statement used to query the topic. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html IoT SQL Reference>
    -- in the /IoT Developer Guide/.
    sql :: Prelude.Text,
    -- | The actions associated with the rule.
    actions :: [Action]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRulePayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsIotSqlVersion', 'topicRulePayload_awsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- 'description', 'topicRulePayload_description' - The description of the rule.
--
-- 'errorAction', 'topicRulePayload_errorAction' - The action to take when an error occurs.
--
-- 'ruleDisabled', 'topicRulePayload_ruleDisabled' - Specifies whether the rule is disabled.
--
-- 'sql', 'topicRulePayload_sql' - The SQL statement used to query the topic. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html IoT SQL Reference>
-- in the /IoT Developer Guide/.
--
-- 'actions', 'topicRulePayload_actions' - The actions associated with the rule.
newTopicRulePayload ::
  -- | 'sql'
  Prelude.Text ->
  TopicRulePayload
newTopicRulePayload pSql_ =
  TopicRulePayload'
    { awsIotSqlVersion =
        Prelude.Nothing,
      description = Prelude.Nothing,
      errorAction = Prelude.Nothing,
      ruleDisabled = Prelude.Nothing,
      sql = pSql_,
      actions = Prelude.mempty
    }

-- | The version of the SQL rules engine to use when evaluating the rule.
topicRulePayload_awsIotSqlVersion :: Lens.Lens' TopicRulePayload (Prelude.Maybe Prelude.Text)
topicRulePayload_awsIotSqlVersion = Lens.lens (\TopicRulePayload' {awsIotSqlVersion} -> awsIotSqlVersion) (\s@TopicRulePayload' {} a -> s {awsIotSqlVersion = a} :: TopicRulePayload)

-- | The description of the rule.
topicRulePayload_description :: Lens.Lens' TopicRulePayload (Prelude.Maybe Prelude.Text)
topicRulePayload_description = Lens.lens (\TopicRulePayload' {description} -> description) (\s@TopicRulePayload' {} a -> s {description = a} :: TopicRulePayload)

-- | The action to take when an error occurs.
topicRulePayload_errorAction :: Lens.Lens' TopicRulePayload (Prelude.Maybe Action)
topicRulePayload_errorAction = Lens.lens (\TopicRulePayload' {errorAction} -> errorAction) (\s@TopicRulePayload' {} a -> s {errorAction = a} :: TopicRulePayload)

-- | Specifies whether the rule is disabled.
topicRulePayload_ruleDisabled :: Lens.Lens' TopicRulePayload (Prelude.Maybe Prelude.Bool)
topicRulePayload_ruleDisabled = Lens.lens (\TopicRulePayload' {ruleDisabled} -> ruleDisabled) (\s@TopicRulePayload' {} a -> s {ruleDisabled = a} :: TopicRulePayload)

-- | The SQL statement used to query the topic. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html IoT SQL Reference>
-- in the /IoT Developer Guide/.
topicRulePayload_sql :: Lens.Lens' TopicRulePayload Prelude.Text
topicRulePayload_sql = Lens.lens (\TopicRulePayload' {sql} -> sql) (\s@TopicRulePayload' {} a -> s {sql = a} :: TopicRulePayload)

-- | The actions associated with the rule.
topicRulePayload_actions :: Lens.Lens' TopicRulePayload [Action]
topicRulePayload_actions = Lens.lens (\TopicRulePayload' {actions} -> actions) (\s@TopicRulePayload' {} a -> s {actions = a} :: TopicRulePayload) Prelude.. Lens.coerced

instance Prelude.Hashable TopicRulePayload where
  hashWithSalt _salt TopicRulePayload' {..} =
    _salt `Prelude.hashWithSalt` awsIotSqlVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` errorAction
      `Prelude.hashWithSalt` ruleDisabled
      `Prelude.hashWithSalt` sql
      `Prelude.hashWithSalt` actions

instance Prelude.NFData TopicRulePayload where
  rnf TopicRulePayload' {..} =
    Prelude.rnf awsIotSqlVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorAction
      `Prelude.seq` Prelude.rnf ruleDisabled
      `Prelude.seq` Prelude.rnf sql
      `Prelude.seq` Prelude.rnf actions

instance Data.ToJSON TopicRulePayload where
  toJSON TopicRulePayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsIotSqlVersion" Data..=)
              Prelude.<$> awsIotSqlVersion,
            ("description" Data..=) Prelude.<$> description,
            ("errorAction" Data..=) Prelude.<$> errorAction,
            ("ruleDisabled" Data..=) Prelude.<$> ruleDisabled,
            Prelude.Just ("sql" Data..= sql),
            Prelude.Just ("actions" Data..= actions)
          ]
      )
