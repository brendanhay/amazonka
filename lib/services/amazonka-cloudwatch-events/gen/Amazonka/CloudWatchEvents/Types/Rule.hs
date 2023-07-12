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
-- Module      : Amazonka.CloudWatchEvents.Types.Rule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Rule where

import Amazonka.CloudWatchEvents.Types.RuleState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a rule in Amazon EventBridge.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The Amazon Resource Name (ARN) of the rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | The event pattern of the rule. For more information, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
    -- in the /Amazon EventBridge User Guide/.
    eventPattern :: Prelude.Maybe Prelude.Text,
    -- | If the rule was created on behalf of your account by an Amazon Web
    -- Services service, this field displays the principal name of the service
    -- that created the rule.
    managedBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role that is used for target
    -- invocation.
    --
    -- If you\'re setting an event bus in another account as the target and
    -- that account granted permission to your account through an organization
    -- instead of directly by the account ID, you must specify a @RoleArn@ with
    -- proper permissions in the @Target@ structure, instead of here in this
    -- parameter.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
    -- minutes)\". For more information, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-create-rule-schedule.html Creating an Amazon EventBridge rule that runs on a schedule>.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The state of the rule.
    state :: Prelude.Maybe RuleState
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
-- 'arn', 'rule_arn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'description', 'rule_description' - The description of the rule.
--
-- 'eventBusName', 'rule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'eventPattern', 'rule_eventPattern' - The event pattern of the rule. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
--
-- 'managedBy', 'rule_managedBy' - If the rule was created on behalf of your account by an Amazon Web
-- Services service, this field displays the principal name of the service
-- that created the rule.
--
-- 'name', 'rule_name' - The name of the rule.
--
-- 'roleArn', 'rule_roleArn' - The Amazon Resource Name (ARN) of the role that is used for target
-- invocation.
--
-- If you\'re setting an event bus in another account as the target and
-- that account granted permission to your account through an organization
-- instead of directly by the account ID, you must specify a @RoleArn@ with
-- proper permissions in the @Target@ structure, instead of here in this
-- parameter.
--
-- 'scheduleExpression', 'rule_scheduleExpression' - The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
-- minutes)\". For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-create-rule-schedule.html Creating an Amazon EventBridge rule that runs on a schedule>.
--
-- 'state', 'rule_state' - The state of the rule.
newRule ::
  Rule
newRule =
  Rule'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      eventPattern = Prelude.Nothing,
      managedBy = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the rule.
rule_arn :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_arn = Lens.lens (\Rule' {arn} -> arn) (\s@Rule' {} a -> s {arn = a} :: Rule)

-- | The description of the rule.
rule_description :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_description = Lens.lens (\Rule' {description} -> description) (\s@Rule' {} a -> s {description = a} :: Rule)

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
rule_eventBusName :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_eventBusName = Lens.lens (\Rule' {eventBusName} -> eventBusName) (\s@Rule' {} a -> s {eventBusName = a} :: Rule)

-- | The event pattern of the rule. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
rule_eventPattern :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_eventPattern = Lens.lens (\Rule' {eventPattern} -> eventPattern) (\s@Rule' {} a -> s {eventPattern = a} :: Rule)

-- | If the rule was created on behalf of your account by an Amazon Web
-- Services service, this field displays the principal name of the service
-- that created the rule.
rule_managedBy :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_managedBy = Lens.lens (\Rule' {managedBy} -> managedBy) (\s@Rule' {} a -> s {managedBy = a} :: Rule)

-- | The name of the rule.
rule_name :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_name = Lens.lens (\Rule' {name} -> name) (\s@Rule' {} a -> s {name = a} :: Rule)

-- | The Amazon Resource Name (ARN) of the role that is used for target
-- invocation.
--
-- If you\'re setting an event bus in another account as the target and
-- that account granted permission to your account through an organization
-- instead of directly by the account ID, you must specify a @RoleArn@ with
-- proper permissions in the @Target@ structure, instead of here in this
-- parameter.
rule_roleArn :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_roleArn = Lens.lens (\Rule' {roleArn} -> roleArn) (\s@Rule' {} a -> s {roleArn = a} :: Rule)

-- | The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
-- minutes)\". For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-create-rule-schedule.html Creating an Amazon EventBridge rule that runs on a schedule>.
rule_scheduleExpression :: Lens.Lens' Rule (Prelude.Maybe Prelude.Text)
rule_scheduleExpression = Lens.lens (\Rule' {scheduleExpression} -> scheduleExpression) (\s@Rule' {} a -> s {scheduleExpression = a} :: Rule)

-- | The state of the rule.
rule_state :: Lens.Lens' Rule (Prelude.Maybe RuleState)
rule_state = Lens.lens (\Rule' {state} -> state) (\s@Rule' {} a -> s {state = a} :: Rule)

instance Data.FromJSON Rule where
  parseJSON =
    Data.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EventBusName")
            Prelude.<*> (x Data..:? "EventPattern")
            Prelude.<*> (x Data..:? "ManagedBy")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "ScheduleExpression")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` eventPattern
      `Prelude.hashWithSalt` managedBy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` scheduleExpression
      `Prelude.hashWithSalt` state

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventBusName
      `Prelude.seq` Prelude.rnf eventPattern
      `Prelude.seq` Prelude.rnf managedBy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf scheduleExpression
      `Prelude.seq` Prelude.rnf state
