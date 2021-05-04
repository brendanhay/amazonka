{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.TopicRulePayload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRulePayload where

import Network.AWS.IoT.Types.Action
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a rule.
--
-- /See:/ 'newTopicRulePayload' smart constructor.
data TopicRulePayload = TopicRulePayload'
  { -- | The action to take when an error occurs.
    errorAction :: Prelude.Maybe Action,
    -- | The version of the SQL rules engine to use when evaluating the rule.
    awsIotSqlVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The description of the rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The SQL statement used to query the topic. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference>
    -- in the /AWS IoT Developer Guide/.
    sql :: Prelude.Text,
    -- | The actions associated with the rule.
    actions :: [Action]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TopicRulePayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorAction', 'topicRulePayload_errorAction' - The action to take when an error occurs.
--
-- 'awsIotSqlVersion', 'topicRulePayload_awsIotSqlVersion' - The version of the SQL rules engine to use when evaluating the rule.
--
-- 'ruleDisabled', 'topicRulePayload_ruleDisabled' - Specifies whether the rule is disabled.
--
-- 'description', 'topicRulePayload_description' - The description of the rule.
--
-- 'sql', 'topicRulePayload_sql' - The SQL statement used to query the topic. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference>
-- in the /AWS IoT Developer Guide/.
--
-- 'actions', 'topicRulePayload_actions' - The actions associated with the rule.
newTopicRulePayload ::
  -- | 'sql'
  Prelude.Text ->
  TopicRulePayload
newTopicRulePayload pSql_ =
  TopicRulePayload'
    { errorAction = Prelude.Nothing,
      awsIotSqlVersion = Prelude.Nothing,
      ruleDisabled = Prelude.Nothing,
      description = Prelude.Nothing,
      sql = pSql_,
      actions = Prelude.mempty
    }

-- | The action to take when an error occurs.
topicRulePayload_errorAction :: Lens.Lens' TopicRulePayload (Prelude.Maybe Action)
topicRulePayload_errorAction = Lens.lens (\TopicRulePayload' {errorAction} -> errorAction) (\s@TopicRulePayload' {} a -> s {errorAction = a} :: TopicRulePayload)

-- | The version of the SQL rules engine to use when evaluating the rule.
topicRulePayload_awsIotSqlVersion :: Lens.Lens' TopicRulePayload (Prelude.Maybe Prelude.Text)
topicRulePayload_awsIotSqlVersion = Lens.lens (\TopicRulePayload' {awsIotSqlVersion} -> awsIotSqlVersion) (\s@TopicRulePayload' {} a -> s {awsIotSqlVersion = a} :: TopicRulePayload)

-- | Specifies whether the rule is disabled.
topicRulePayload_ruleDisabled :: Lens.Lens' TopicRulePayload (Prelude.Maybe Prelude.Bool)
topicRulePayload_ruleDisabled = Lens.lens (\TopicRulePayload' {ruleDisabled} -> ruleDisabled) (\s@TopicRulePayload' {} a -> s {ruleDisabled = a} :: TopicRulePayload)

-- | The description of the rule.
topicRulePayload_description :: Lens.Lens' TopicRulePayload (Prelude.Maybe Prelude.Text)
topicRulePayload_description = Lens.lens (\TopicRulePayload' {description} -> description) (\s@TopicRulePayload' {} a -> s {description = a} :: TopicRulePayload)

-- | The SQL statement used to query the topic. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-sql-reference.html AWS IoT SQL Reference>
-- in the /AWS IoT Developer Guide/.
topicRulePayload_sql :: Lens.Lens' TopicRulePayload Prelude.Text
topicRulePayload_sql = Lens.lens (\TopicRulePayload' {sql} -> sql) (\s@TopicRulePayload' {} a -> s {sql = a} :: TopicRulePayload)

-- | The actions associated with the rule.
topicRulePayload_actions :: Lens.Lens' TopicRulePayload [Action]
topicRulePayload_actions = Lens.lens (\TopicRulePayload' {actions} -> actions) (\s@TopicRulePayload' {} a -> s {actions = a} :: TopicRulePayload) Prelude.. Prelude._Coerce

instance Prelude.Hashable TopicRulePayload

instance Prelude.NFData TopicRulePayload

instance Prelude.ToJSON TopicRulePayload where
  toJSON TopicRulePayload' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("errorAction" Prelude..=) Prelude.<$> errorAction,
            ("awsIotSqlVersion" Prelude..=)
              Prelude.<$> awsIotSqlVersion,
            ("ruleDisabled" Prelude..=) Prelude.<$> ruleDisabled,
            ("description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("sql" Prelude..= sql),
            Prelude.Just ("actions" Prelude..= actions)
          ]
      )
