{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Rule
  ( Rule (..),

    -- * Smart constructor
    mkRule,

    -- * Lenses
    rEventPattern,
    rState,
    rARN,
    rEventBusName,
    rScheduleExpression,
    rName,
    rDescription,
    rManagedBy,
    rRoleARN,
  )
where

import Network.AWS.CloudWatchEvents.Types.RuleState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a rule in Amazon EventBridge.
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { eventPattern :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe RuleState,
    arn :: Lude.Maybe Lude.Text,
    eventBusName :: Lude.Maybe Lude.Text,
    scheduleExpression :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    managedBy :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the rule.
-- * 'description' - The description of the rule.
-- * 'eventBusName' - The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
-- * 'eventPattern' - The event pattern of the rule. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
-- * 'managedBy' - If the rule was created on behalf of your account by an AWS service, this field displays the principal name of the service that created the rule.
-- * 'name' - The name of the rule.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role that is used for target invocation.
-- * 'scheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
-- * 'state' - The state of the rule.
mkRule ::
  Rule
mkRule =
  Rule'
    { eventPattern = Lude.Nothing,
      state = Lude.Nothing,
      arn = Lude.Nothing,
      eventBusName = Lude.Nothing,
      scheduleExpression = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      managedBy = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The event pattern of the rule. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventPattern :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rEventPattern = Lens.lens (eventPattern :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {eventPattern = a} :: Rule)
{-# DEPRECATED rEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | The state of the rule.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Rule (Lude.Maybe RuleState)
rState = Lens.lens (state :: Rule -> Lude.Maybe RuleState) (\s a -> s {state = a} :: Rule)
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rARN :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rARN = Lens.lens (arn :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Rule)
{-# DEPRECATED rARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEventBusName :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rEventBusName = Lens.lens (eventBusName :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: Rule)
{-# DEPRECATED rEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rScheduleExpression :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rScheduleExpression = Lens.lens (scheduleExpression :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: Rule)
{-# DEPRECATED rScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Rule)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rDescription = Lens.lens (description :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Rule)
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If the rule was created on behalf of your account by an AWS service, this field displays the principal name of the service that created the rule.
--
-- /Note:/ Consider using 'managedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rManagedBy :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rManagedBy = Lens.lens (managedBy :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {managedBy = a} :: Rule)
{-# DEPRECATED rManagedBy "Use generic-lens or generic-optics with 'managedBy' instead." #-}

-- | The Amazon Resource Name (ARN) of the role that is used for target invocation.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoleARN :: Lens.Lens' Rule (Lude.Maybe Lude.Text)
rRoleARN = Lens.lens (roleARN :: Rule -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Rule)
{-# DEPRECATED rRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON Rule where
  parseJSON =
    Lude.withObject
      "Rule"
      ( \x ->
          Rule'
            Lude.<$> (x Lude..:? "EventPattern")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "EventBusName")
            Lude.<*> (x Lude..:? "ScheduleExpression")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ManagedBy")
            Lude.<*> (x Lude..:? "RoleArn")
      )
