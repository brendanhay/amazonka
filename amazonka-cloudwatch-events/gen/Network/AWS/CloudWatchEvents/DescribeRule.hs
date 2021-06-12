{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rule.
--
-- DescribeRule does not list the targets of a rule. To see the targets
-- associated with a rule, use ListTargetsByRule.
module Network.AWS.CloudWatchEvents.DescribeRule
  ( -- * Creating a Request
    DescribeRule (..),
    newDescribeRule,

    -- * Request Lenses
    describeRule_eventBusName,
    describeRule_name,

    -- * Destructuring the Response
    DescribeRuleResponse (..),
    newDescribeRuleResponse,

    -- * Response Lenses
    describeRuleResponse_eventPattern,
    describeRuleResponse_roleArn,
    describeRuleResponse_arn,
    describeRuleResponse_eventBusName,
    describeRuleResponse_state,
    describeRuleResponse_scheduleExpression,
    describeRuleResponse_name,
    describeRuleResponse_managedBy,
    describeRuleResponse_description,
    describeRuleResponse_createdBy,
    describeRuleResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRule' smart constructor.
data DescribeRule = DescribeRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Core.Maybe Core.Text,
    -- | The name of the rule.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusName', 'describeRule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'name', 'describeRule_name' - The name of the rule.
newDescribeRule ::
  -- | 'name'
  Core.Text ->
  DescribeRule
newDescribeRule pName_ =
  DescribeRule'
    { eventBusName = Core.Nothing,
      name = pName_
    }

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
describeRule_eventBusName :: Lens.Lens' DescribeRule (Core.Maybe Core.Text)
describeRule_eventBusName = Lens.lens (\DescribeRule' {eventBusName} -> eventBusName) (\s@DescribeRule' {} a -> s {eventBusName = a} :: DescribeRule)

-- | The name of the rule.
describeRule_name :: Lens.Lens' DescribeRule Core.Text
describeRule_name = Lens.lens (\DescribeRule' {name} -> name) (\s@DescribeRule' {} a -> s {name = a} :: DescribeRule)

instance Core.AWSRequest DescribeRule where
  type AWSResponse DescribeRule = DescribeRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuleResponse'
            Core.<$> (x Core..?> "EventPattern")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "EventBusName")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "ScheduleExpression")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "ManagedBy")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRule

instance Core.NFData DescribeRule

instance Core.ToHeaders DescribeRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DescribeRule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeRule where
  toJSON DescribeRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventBusName" Core..=) Core.<$> eventBusName,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath DescribeRule where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRuleResponse' smart constructor.
data DescribeRuleResponse = DescribeRuleResponse'
  { -- | The event pattern. For more information, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
    -- in the /Amazon EventBridge User Guide/.
    eventPattern :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
    roleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the rule.
    arn :: Core.Maybe Core.Text,
    -- | The name of the event bus associated with the rule.
    eventBusName :: Core.Maybe Core.Text,
    -- | Specifies whether the rule is enabled or disabled.
    state :: Core.Maybe RuleState,
    -- | The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
    -- minutes)\".
    scheduleExpression :: Core.Maybe Core.Text,
    -- | The name of the rule.
    name :: Core.Maybe Core.Text,
    -- | If this is a managed rule, created by an AWS service on your behalf,
    -- this field displays the principal name of the AWS service that created
    -- the rule.
    managedBy :: Core.Maybe Core.Text,
    -- | The description of the rule.
    description :: Core.Maybe Core.Text,
    -- | The account ID of the user that created the rule. If you use @PutRule@
    -- to put a rule on an event bus in another account, the other account is
    -- the owner of the rule, and the rule ARN includes the account ID for that
    -- account. However, the value for @CreatedBy@ is the account ID as the
    -- account that created the rule in the other account.
    createdBy :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventPattern', 'describeRuleResponse_eventPattern' - The event pattern. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
--
-- 'roleArn', 'describeRuleResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role associated with the rule.
--
-- 'arn', 'describeRuleResponse_arn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'eventBusName', 'describeRuleResponse_eventBusName' - The name of the event bus associated with the rule.
--
-- 'state', 'describeRuleResponse_state' - Specifies whether the rule is enabled or disabled.
--
-- 'scheduleExpression', 'describeRuleResponse_scheduleExpression' - The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
-- minutes)\".
--
-- 'name', 'describeRuleResponse_name' - The name of the rule.
--
-- 'managedBy', 'describeRuleResponse_managedBy' - If this is a managed rule, created by an AWS service on your behalf,
-- this field displays the principal name of the AWS service that created
-- the rule.
--
-- 'description', 'describeRuleResponse_description' - The description of the rule.
--
-- 'createdBy', 'describeRuleResponse_createdBy' - The account ID of the user that created the rule. If you use @PutRule@
-- to put a rule on an event bus in another account, the other account is
-- the owner of the rule, and the rule ARN includes the account ID for that
-- account. However, the value for @CreatedBy@ is the account ID as the
-- account that created the rule in the other account.
--
-- 'httpStatus', 'describeRuleResponse_httpStatus' - The response's http status code.
newDescribeRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeRuleResponse
newDescribeRuleResponse pHttpStatus_ =
  DescribeRuleResponse'
    { eventPattern = Core.Nothing,
      roleArn = Core.Nothing,
      arn = Core.Nothing,
      eventBusName = Core.Nothing,
      state = Core.Nothing,
      scheduleExpression = Core.Nothing,
      name = Core.Nothing,
      managedBy = Core.Nothing,
      description = Core.Nothing,
      createdBy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The event pattern. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
describeRuleResponse_eventPattern :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_eventPattern = Lens.lens (\DescribeRuleResponse' {eventPattern} -> eventPattern) (\s@DescribeRuleResponse' {} a -> s {eventPattern = a} :: DescribeRuleResponse)

-- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
describeRuleResponse_roleArn :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_roleArn = Lens.lens (\DescribeRuleResponse' {roleArn} -> roleArn) (\s@DescribeRuleResponse' {} a -> s {roleArn = a} :: DescribeRuleResponse)

-- | The Amazon Resource Name (ARN) of the rule.
describeRuleResponse_arn :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_arn = Lens.lens (\DescribeRuleResponse' {arn} -> arn) (\s@DescribeRuleResponse' {} a -> s {arn = a} :: DescribeRuleResponse)

-- | The name of the event bus associated with the rule.
describeRuleResponse_eventBusName :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_eventBusName = Lens.lens (\DescribeRuleResponse' {eventBusName} -> eventBusName) (\s@DescribeRuleResponse' {} a -> s {eventBusName = a} :: DescribeRuleResponse)

-- | Specifies whether the rule is enabled or disabled.
describeRuleResponse_state :: Lens.Lens' DescribeRuleResponse (Core.Maybe RuleState)
describeRuleResponse_state = Lens.lens (\DescribeRuleResponse' {state} -> state) (\s@DescribeRuleResponse' {} a -> s {state = a} :: DescribeRuleResponse)

-- | The scheduling expression. For example, \"cron(0 20 * * ? *)\", \"rate(5
-- minutes)\".
describeRuleResponse_scheduleExpression :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_scheduleExpression = Lens.lens (\DescribeRuleResponse' {scheduleExpression} -> scheduleExpression) (\s@DescribeRuleResponse' {} a -> s {scheduleExpression = a} :: DescribeRuleResponse)

-- | The name of the rule.
describeRuleResponse_name :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_name = Lens.lens (\DescribeRuleResponse' {name} -> name) (\s@DescribeRuleResponse' {} a -> s {name = a} :: DescribeRuleResponse)

-- | If this is a managed rule, created by an AWS service on your behalf,
-- this field displays the principal name of the AWS service that created
-- the rule.
describeRuleResponse_managedBy :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_managedBy = Lens.lens (\DescribeRuleResponse' {managedBy} -> managedBy) (\s@DescribeRuleResponse' {} a -> s {managedBy = a} :: DescribeRuleResponse)

-- | The description of the rule.
describeRuleResponse_description :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_description = Lens.lens (\DescribeRuleResponse' {description} -> description) (\s@DescribeRuleResponse' {} a -> s {description = a} :: DescribeRuleResponse)

-- | The account ID of the user that created the rule. If you use @PutRule@
-- to put a rule on an event bus in another account, the other account is
-- the owner of the rule, and the rule ARN includes the account ID for that
-- account. However, the value for @CreatedBy@ is the account ID as the
-- account that created the rule in the other account.
describeRuleResponse_createdBy :: Lens.Lens' DescribeRuleResponse (Core.Maybe Core.Text)
describeRuleResponse_createdBy = Lens.lens (\DescribeRuleResponse' {createdBy} -> createdBy) (\s@DescribeRuleResponse' {} a -> s {createdBy = a} :: DescribeRuleResponse)

-- | The response's http status code.
describeRuleResponse_httpStatus :: Lens.Lens' DescribeRuleResponse Core.Int
describeRuleResponse_httpStatus = Lens.lens (\DescribeRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeRuleResponse' {} a -> s {httpStatus = a} :: DescribeRuleResponse)

instance Core.NFData DescribeRuleResponse
