{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rule.
--
-- DescribeRule does not list the targets of a rule. To see the targets associated with a rule, use 'ListTargetsByRule' .
module Network.AWS.CloudWatchEvents.DescribeRule
  ( -- * Creating a request
    DescribeRule (..),
    mkDescribeRule,

    -- ** Request lenses
    drEventBusName,
    drName,

    -- * Destructuring the response
    DescribeRuleResponse (..),
    mkDescribeRuleResponse,

    -- ** Response lenses
    drrsEventPattern,
    drrsState,
    drrsARN,
    drrsCreatedBy,
    drrsEventBusName,
    drrsScheduleExpression,
    drrsName,
    drrsDescription,
    drrsManagedBy,
    drrsRoleARN,
    drrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRule' smart constructor.
data DescribeRule = DescribeRule'
  { -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The name of the rule.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRule' with the minimum fields required to make a request.
--
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'name' - The name of the rule.
mkDescribeRule ::
  -- | 'name'
  Lude.Text ->
  DescribeRule
mkDescribeRule pName_ =
  DescribeRule' {eventBusName = Lude.Nothing, name = pName_}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drEventBusName :: Lens.Lens' DescribeRule (Lude.Maybe Lude.Text)
drEventBusName = Lens.lens (eventBusName :: DescribeRule -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: DescribeRule)
{-# DEPRECATED drEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drName :: Lens.Lens' DescribeRule Lude.Text
drName = Lens.lens (name :: DescribeRule -> Lude.Text) (\s a -> s {name = a} :: DescribeRule)
{-# DEPRECATED drName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeRule where
  type Rs DescribeRule = DescribeRuleResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRuleResponse'
            Lude.<$> (x Lude..?> "EventPattern")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreatedBy")
            Lude.<*> (x Lude..?> "EventBusName")
            Lude.<*> (x Lude..?> "ScheduleExpression")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "ManagedBy")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DescribeRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRule where
  toJSON DescribeRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventBusName" Lude..=) Lude.<$> eventBusName,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DescribeRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRuleResponse' smart constructor.
data DescribeRuleResponse = DescribeRuleResponse'
  { -- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
    eventPattern :: Lude.Maybe Lude.Text,
    -- | Specifies whether the rule is enabled or disabled.
    state :: Lude.Maybe RuleState,
    -- | The Amazon Resource Name (ARN) of the rule.
    arn :: Lude.Maybe Lude.Text,
    -- | The account ID of the user that created the rule. If you use @PutRule@ to put a rule on an event bus in another account, the other account is the owner of the rule, and the rule ARN includes the account ID for that account. However, the value for @CreatedBy@ is the account ID as the account that created the rule in the other account.
    createdBy :: Lude.Maybe Lude.Text,
    -- | The name of the event bus associated with the rule.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
    scheduleExpression :: Lude.Maybe Lude.Text,
    -- | The name of the rule.
    name :: Lude.Maybe Lude.Text,
    -- | The description of the rule.
    description :: Lude.Maybe Lude.Text,
    -- | If this is a managed rule, created by an AWS service on your behalf, this field displays the principal name of the AWS service that created the rule.
    managedBy :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRuleResponse' with the minimum fields required to make a request.
--
-- * 'eventPattern' - The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
-- * 'state' - Specifies whether the rule is enabled or disabled.
-- * 'arn' - The Amazon Resource Name (ARN) of the rule.
-- * 'createdBy' - The account ID of the user that created the rule. If you use @PutRule@ to put a rule on an event bus in another account, the other account is the owner of the rule, and the rule ARN includes the account ID for that account. However, the value for @CreatedBy@ is the account ID as the account that created the rule in the other account.
-- * 'eventBusName' - The name of the event bus associated with the rule.
-- * 'scheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
-- * 'name' - The name of the rule.
-- * 'description' - The description of the rule.
-- * 'managedBy' - If this is a managed rule, created by an AWS service on your behalf, this field displays the principal name of the AWS service that created the rule.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the rule.
-- * 'responseStatus' - The response status code.
mkDescribeRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRuleResponse
mkDescribeRuleResponse pResponseStatus_ =
  DescribeRuleResponse'
    { eventPattern = Lude.Nothing,
      state = Lude.Nothing,
      arn = Lude.Nothing,
      createdBy = Lude.Nothing,
      eventBusName = Lude.Nothing,
      scheduleExpression = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      managedBy = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsEventPattern :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsEventPattern = Lens.lens (eventPattern :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {eventPattern = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | Specifies whether the rule is enabled or disabled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsState :: Lens.Lens' DescribeRuleResponse (Lude.Maybe RuleState)
drrsState = Lens.lens (state :: DescribeRuleResponse -> Lude.Maybe RuleState) (\s a -> s {state = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsARN :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsARN = Lens.lens (arn :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The account ID of the user that created the rule. If you use @PutRule@ to put a rule on an event bus in another account, the other account is the owner of the rule, and the rule ARN includes the account ID for that account. However, the value for @CreatedBy@ is the account ID as the account that created the rule in the other account.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsCreatedBy :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsCreatedBy = Lens.lens (createdBy :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The name of the event bus associated with the rule.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsEventBusName :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsEventBusName = Lens.lens (eventBusName :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsScheduleExpression :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsScheduleExpression = Lens.lens (scheduleExpression :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsName :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsName = Lens.lens (name :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsDescription :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsDescription = Lens.lens (description :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If this is a managed rule, created by an AWS service on your behalf, this field displays the principal name of the AWS service that created the rule.
--
-- /Note:/ Consider using 'managedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsManagedBy :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsManagedBy = Lens.lens (managedBy :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {managedBy = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsManagedBy "Use generic-lens or generic-optics with 'managedBy' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRoleARN :: Lens.Lens' DescribeRuleResponse (Lude.Maybe Lude.Text)
drrsRoleARN = Lens.lens (roleARN :: DescribeRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DescribeRuleResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DescribeRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRuleResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
