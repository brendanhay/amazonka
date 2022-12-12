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
-- Module      : Amazonka.Connect.Types.Rule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Rule where

import Amazonka.Connect.Types.RuleAction
import Amazonka.Connect.Types.RulePublishStatus
import Amazonka.Connect.Types.RuleTriggerEventSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a rule.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the rule.
    name :: Prelude.Text,
    -- | A unique identifier for the rule.
    ruleId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Prelude.Text,
    -- | The event source to trigger the rule.
    triggerEventSource :: RuleTriggerEventSource,
    -- | The conditions of the rule.
    function :: Prelude.Text,
    -- | A list of actions to be run when the rule is triggered.
    actions :: [RuleAction],
    -- | The publish status of the rule.
    publishStatus :: RulePublishStatus,
    -- | The timestamp for when the rule was created.
    createdTime :: Data.POSIX,
    -- | The timestamp for the when the rule was last updated.
    lastUpdatedTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who last updated the rule.
    lastUpdatedBy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'rule_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'rule_name' - The name of the rule.
--
-- 'ruleId', 'rule_ruleId' - A unique identifier for the rule.
--
-- 'ruleArn', 'rule_ruleArn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'triggerEventSource', 'rule_triggerEventSource' - The event source to trigger the rule.
--
-- 'function', 'rule_function' - The conditions of the rule.
--
-- 'actions', 'rule_actions' - A list of actions to be run when the rule is triggered.
--
-- 'publishStatus', 'rule_publishStatus' - The publish status of the rule.
--
-- 'createdTime', 'rule_createdTime' - The timestamp for when the rule was created.
--
-- 'lastUpdatedTime', 'rule_lastUpdatedTime' - The timestamp for the when the rule was last updated.
--
-- 'lastUpdatedBy', 'rule_lastUpdatedBy' - The Amazon Resource Name (ARN) of the user who last updated the rule.
newRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'ruleArn'
  Prelude.Text ->
  -- | 'triggerEventSource'
  RuleTriggerEventSource ->
  -- | 'function'
  Prelude.Text ->
  -- | 'publishStatus'
  RulePublishStatus ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastUpdatedTime'
  Prelude.UTCTime ->
  -- | 'lastUpdatedBy'
  Prelude.Text ->
  Rule
newRule
  pName_
  pRuleId_
  pRuleArn_
  pTriggerEventSource_
  pFunction_
  pPublishStatus_
  pCreatedTime_
  pLastUpdatedTime_
  pLastUpdatedBy_ =
    Rule'
      { tags = Prelude.Nothing,
        name = pName_,
        ruleId = pRuleId_,
        ruleArn = pRuleArn_,
        triggerEventSource = pTriggerEventSource_,
        function = pFunction_,
        actions = Prelude.mempty,
        publishStatus = pPublishStatus_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastUpdatedTime =
          Data._Time Lens.# pLastUpdatedTime_,
        lastUpdatedBy = pLastUpdatedBy_
      }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
rule_tags :: Lens.Lens' Rule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
rule_tags = Lens.lens (\Rule' {tags} -> tags) (\s@Rule' {} a -> s {tags = a} :: Rule) Prelude.. Lens.mapping Lens.coerced

-- | The name of the rule.
rule_name :: Lens.Lens' Rule Prelude.Text
rule_name = Lens.lens (\Rule' {name} -> name) (\s@Rule' {} a -> s {name = a} :: Rule)

-- | A unique identifier for the rule.
rule_ruleId :: Lens.Lens' Rule Prelude.Text
rule_ruleId = Lens.lens (\Rule' {ruleId} -> ruleId) (\s@Rule' {} a -> s {ruleId = a} :: Rule)

-- | The Amazon Resource Name (ARN) of the rule.
rule_ruleArn :: Lens.Lens' Rule Prelude.Text
rule_ruleArn = Lens.lens (\Rule' {ruleArn} -> ruleArn) (\s@Rule' {} a -> s {ruleArn = a} :: Rule)

-- | The event source to trigger the rule.
rule_triggerEventSource :: Lens.Lens' Rule RuleTriggerEventSource
rule_triggerEventSource = Lens.lens (\Rule' {triggerEventSource} -> triggerEventSource) (\s@Rule' {} a -> s {triggerEventSource = a} :: Rule)

-- | The conditions of the rule.
rule_function :: Lens.Lens' Rule Prelude.Text
rule_function = Lens.lens (\Rule' {function} -> function) (\s@Rule' {} a -> s {function = a} :: Rule)

-- | A list of actions to be run when the rule is triggered.
rule_actions :: Lens.Lens' Rule [RuleAction]
rule_actions = Lens.lens (\Rule' {actions} -> actions) (\s@Rule' {} a -> s {actions = a} :: Rule) Prelude.. Lens.coerced

-- | The publish status of the rule.
rule_publishStatus :: Lens.Lens' Rule RulePublishStatus
rule_publishStatus = Lens.lens (\Rule' {publishStatus} -> publishStatus) (\s@Rule' {} a -> s {publishStatus = a} :: Rule)

-- | The timestamp for when the rule was created.
rule_createdTime :: Lens.Lens' Rule Prelude.UTCTime
rule_createdTime = Lens.lens (\Rule' {createdTime} -> createdTime) (\s@Rule' {} a -> s {createdTime = a} :: Rule) Prelude.. Data._Time

-- | The timestamp for the when the rule was last updated.
rule_lastUpdatedTime :: Lens.Lens' Rule Prelude.UTCTime
rule_lastUpdatedTime = Lens.lens (\Rule' {lastUpdatedTime} -> lastUpdatedTime) (\s@Rule' {} a -> s {lastUpdatedTime = a} :: Rule) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the user who last updated the rule.
rule_lastUpdatedBy :: Lens.Lens' Rule Prelude.Text
rule_lastUpdatedBy = Lens.lens (\Rule' {lastUpdatedBy} -> lastUpdatedBy) (\s@Rule' {} a -> s {lastUpdatedBy = a} :: Rule)

instance Data.FromJSON Rule where
  parseJSON =
    Data.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "RuleId")
            Prelude.<*> (x Data..: "RuleArn")
            Prelude.<*> (x Data..: "TriggerEventSource")
            Prelude.<*> (x Data..: "Function")
            Prelude.<*> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "PublishStatus")
            Prelude.<*> (x Data..: "CreatedTime")
            Prelude.<*> (x Data..: "LastUpdatedTime")
            Prelude.<*> (x Data..: "LastUpdatedBy")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` ruleArn
      `Prelude.hashWithSalt` triggerEventSource
      `Prelude.hashWithSalt` function
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` publishStatus
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` lastUpdatedBy

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf ruleArn
      `Prelude.seq` Prelude.rnf triggerEventSource
      `Prelude.seq` Prelude.rnf function
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf publishStatus
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf lastUpdatedBy
