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
-- Module      : Network.AWS.CloudWatchEvents.Types.Rule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Rule where

import Network.AWS.CloudWatchEvents.Types.RuleState
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a rule in Amazon EventBridge.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The event pattern of the rule. For more information, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
    -- in the /Amazon EventBridge User Guide/.
    eventPattern :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the role that is used for target
    -- invocation.
    roleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the rule.
    arn :: Core.Maybe Core.Text,
    -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Core.Maybe Core.Text,
    -- | The state of the rule.
    state :: Core.Maybe RuleState,
    -- | The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
    -- minutes)\".
    scheduleExpression :: Core.Maybe Core.Text,
    -- | The name of the rule.
    name :: Core.Maybe Core.Text,
    -- | If the rule was created on behalf of your account by an AWS service,
    -- this field displays the principal name of the service that created the
    -- rule.
    managedBy :: Core.Maybe Core.Text,
    -- | The description of the rule.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventPattern', 'rule_eventPattern' - The event pattern of the rule. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
--
-- 'roleArn', 'rule_roleArn' - The Amazon Resource Name (ARN) of the role that is used for target
-- invocation.
--
-- 'arn', 'rule_arn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'eventBusName', 'rule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'state', 'rule_state' - The state of the rule.
--
-- 'scheduleExpression', 'rule_scheduleExpression' - The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
-- minutes)\".
--
-- 'name', 'rule_name' - The name of the rule.
--
-- 'managedBy', 'rule_managedBy' - If the rule was created on behalf of your account by an AWS service,
-- this field displays the principal name of the service that created the
-- rule.
--
-- 'description', 'rule_description' - The description of the rule.
newRule ::
  Rule
newRule =
  Rule'
    { eventPattern = Core.Nothing,
      roleArn = Core.Nothing,
      arn = Core.Nothing,
      eventBusName = Core.Nothing,
      state = Core.Nothing,
      scheduleExpression = Core.Nothing,
      name = Core.Nothing,
      managedBy = Core.Nothing,
      description = Core.Nothing
    }

-- | The event pattern of the rule. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
rule_eventPattern :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_eventPattern = Lens.lens (\Rule' {eventPattern} -> eventPattern) (\s@Rule' {} a -> s {eventPattern = a} :: Rule)

-- | The Amazon Resource Name (ARN) of the role that is used for target
-- invocation.
rule_roleArn :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_roleArn = Lens.lens (\Rule' {roleArn} -> roleArn) (\s@Rule' {} a -> s {roleArn = a} :: Rule)

-- | The Amazon Resource Name (ARN) of the rule.
rule_arn :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_arn = Lens.lens (\Rule' {arn} -> arn) (\s@Rule' {} a -> s {arn = a} :: Rule)

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
rule_eventBusName :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_eventBusName = Lens.lens (\Rule' {eventBusName} -> eventBusName) (\s@Rule' {} a -> s {eventBusName = a} :: Rule)

-- | The state of the rule.
rule_state :: Lens.Lens' Rule (Core.Maybe RuleState)
rule_state = Lens.lens (\Rule' {state} -> state) (\s@Rule' {} a -> s {state = a} :: Rule)

-- | The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
-- minutes)\".
rule_scheduleExpression :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_scheduleExpression = Lens.lens (\Rule' {scheduleExpression} -> scheduleExpression) (\s@Rule' {} a -> s {scheduleExpression = a} :: Rule)

-- | The name of the rule.
rule_name :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_name = Lens.lens (\Rule' {name} -> name) (\s@Rule' {} a -> s {name = a} :: Rule)

-- | If the rule was created on behalf of your account by an AWS service,
-- this field displays the principal name of the service that created the
-- rule.
rule_managedBy :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_managedBy = Lens.lens (\Rule' {managedBy} -> managedBy) (\s@Rule' {} a -> s {managedBy = a} :: Rule)

-- | The description of the rule.
rule_description :: Lens.Lens' Rule (Core.Maybe Core.Text)
rule_description = Lens.lens (\Rule' {description} -> description) (\s@Rule' {} a -> s {description = a} :: Rule)

instance Core.FromJSON Rule where
  parseJSON =
    Core.withObject
      "Rule"
      ( \x ->
          Rule'
            Core.<$> (x Core..:? "EventPattern")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "EventBusName")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "ScheduleExpression")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "ManagedBy")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable Rule

instance Core.NFData Rule
