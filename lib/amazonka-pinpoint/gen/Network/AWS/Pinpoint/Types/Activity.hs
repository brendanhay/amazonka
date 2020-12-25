{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Activity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Activity
  ( Activity (..),

    -- * Smart constructor
    mkActivity,

    -- * Lenses
    aCUSTOM,
    aConditionalSplit,
    aDescription,
    aEMAIL,
    aHoldout,
    aMultiCondition,
    aPUSH,
    aRandomSplit,
    aSMS,
    aWait,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ConditionalSplitActivity as Types
import qualified Network.AWS.Pinpoint.Types.CustomMessageActivity as Types
import qualified Network.AWS.Pinpoint.Types.EmailMessageActivity as Types
import qualified Network.AWS.Pinpoint.Types.HoldoutActivity as Types
import qualified Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity as Types
import qualified Network.AWS.Pinpoint.Types.PushMessageActivity as Types
import qualified Network.AWS.Pinpoint.Types.RandomSplitActivity as Types
import qualified Network.AWS.Pinpoint.Types.SMSMessageActivity as Types
import qualified Network.AWS.Pinpoint.Types.WaitActivity as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration and other settings for an activity in a journey.
--
-- /See:/ 'mkActivity' smart constructor.
data Activity = Activity'
  { -- | The settings for a custom message activity. This type of activity calls an AWS Lambda function or web hook that sends messages to participants.
    custom :: Core.Maybe Types.CustomMessageActivity,
    -- | The settings for a yes/no split activity. This type of activity sends participants down one of two paths in a journey, based on conditions that you specify.
    conditionalSplit :: Core.Maybe Types.ConditionalSplitActivity,
    -- | The custom description of the activity.
    description :: Core.Maybe Core.Text,
    -- | The settings for an email activity. This type of activity sends an email message to participants.
    email :: Core.Maybe Types.EmailMessageActivity,
    -- | The settings for a holdout activity. This type of activity stops a journey for a specified percentage of participants.
    holdout :: Core.Maybe Types.HoldoutActivity,
    -- | The settings for a multivariate split activity. This type of activity sends participants down one of as many as five paths (including a default /Else/ path) in a journey, based on conditions that you specify.
    multiCondition :: Core.Maybe Types.MultiConditionalSplitActivity,
    -- | The settings for a push notification activity. This type of activity sends a push notification to participants.
    push :: Core.Maybe Types.PushMessageActivity,
    -- | The settings for a random split activity. This type of activity randomly sends specified percentages of participants down one of as many as five paths in a journey, based on conditions that you specify.
    randomSplit :: Core.Maybe Types.RandomSplitActivity,
    -- | The settings for an SMS activity. This type of activity sends a text message to participants.
    sms :: Core.Maybe Types.SMSMessageActivity,
    -- | The settings for a wait activity. This type of activity waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
    wait :: Core.Maybe Types.WaitActivity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Activity' value with any optional fields omitted.
mkActivity ::
  Activity
mkActivity =
  Activity'
    { custom = Core.Nothing,
      conditionalSplit = Core.Nothing,
      description = Core.Nothing,
      email = Core.Nothing,
      holdout = Core.Nothing,
      multiCondition = Core.Nothing,
      push = Core.Nothing,
      randomSplit = Core.Nothing,
      sms = Core.Nothing,
      wait = Core.Nothing
    }

-- | The settings for a custom message activity. This type of activity calls an AWS Lambda function or web hook that sends messages to participants.
--
-- /Note:/ Consider using 'custom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCUSTOM :: Lens.Lens' Activity (Core.Maybe Types.CustomMessageActivity)
aCUSTOM = Lens.field @"custom"
{-# DEPRECATED aCUSTOM "Use generic-lens or generic-optics with 'custom' instead." #-}

-- | The settings for a yes/no split activity. This type of activity sends participants down one of two paths in a journey, based on conditions that you specify.
--
-- /Note:/ Consider using 'conditionalSplit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aConditionalSplit :: Lens.Lens' Activity (Core.Maybe Types.ConditionalSplitActivity)
aConditionalSplit = Lens.field @"conditionalSplit"
{-# DEPRECATED aConditionalSplit "Use generic-lens or generic-optics with 'conditionalSplit' instead." #-}

-- | The custom description of the activity.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Activity (Core.Maybe Core.Text)
aDescription = Lens.field @"description"
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The settings for an email activity. This type of activity sends an email message to participants.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEMAIL :: Lens.Lens' Activity (Core.Maybe Types.EmailMessageActivity)
aEMAIL = Lens.field @"email"
{-# DEPRECATED aEMAIL "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The settings for a holdout activity. This type of activity stops a journey for a specified percentage of participants.
--
-- /Note:/ Consider using 'holdout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aHoldout :: Lens.Lens' Activity (Core.Maybe Types.HoldoutActivity)
aHoldout = Lens.field @"holdout"
{-# DEPRECATED aHoldout "Use generic-lens or generic-optics with 'holdout' instead." #-}

-- | The settings for a multivariate split activity. This type of activity sends participants down one of as many as five paths (including a default /Else/ path) in a journey, based on conditions that you specify.
--
-- /Note:/ Consider using 'multiCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMultiCondition :: Lens.Lens' Activity (Core.Maybe Types.MultiConditionalSplitActivity)
aMultiCondition = Lens.field @"multiCondition"
{-# DEPRECATED aMultiCondition "Use generic-lens or generic-optics with 'multiCondition' instead." #-}

-- | The settings for a push notification activity. This type of activity sends a push notification to participants.
--
-- /Note:/ Consider using 'push' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPUSH :: Lens.Lens' Activity (Core.Maybe Types.PushMessageActivity)
aPUSH = Lens.field @"push"
{-# DEPRECATED aPUSH "Use generic-lens or generic-optics with 'push' instead." #-}

-- | The settings for a random split activity. This type of activity randomly sends specified percentages of participants down one of as many as five paths in a journey, based on conditions that you specify.
--
-- /Note:/ Consider using 'randomSplit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRandomSplit :: Lens.Lens' Activity (Core.Maybe Types.RandomSplitActivity)
aRandomSplit = Lens.field @"randomSplit"
{-# DEPRECATED aRandomSplit "Use generic-lens or generic-optics with 'randomSplit' instead." #-}

-- | The settings for an SMS activity. This type of activity sends a text message to participants.
--
-- /Note:/ Consider using 'sms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSMS :: Lens.Lens' Activity (Core.Maybe Types.SMSMessageActivity)
aSMS = Lens.field @"sms"
{-# DEPRECATED aSMS "Use generic-lens or generic-optics with 'sms' instead." #-}

-- | The settings for a wait activity. This type of activity waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
-- /Note:/ Consider using 'wait' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aWait :: Lens.Lens' Activity (Core.Maybe Types.WaitActivity)
aWait = Lens.field @"wait"
{-# DEPRECATED aWait "Use generic-lens or generic-optics with 'wait' instead." #-}

instance Core.FromJSON Activity where
  toJSON Activity {..} =
    Core.object
      ( Core.catMaybes
          [ ("CUSTOM" Core..=) Core.<$> custom,
            ("ConditionalSplit" Core..=) Core.<$> conditionalSplit,
            ("Description" Core..=) Core.<$> description,
            ("EMAIL" Core..=) Core.<$> email,
            ("Holdout" Core..=) Core.<$> holdout,
            ("MultiCondition" Core..=) Core.<$> multiCondition,
            ("PUSH" Core..=) Core.<$> push,
            ("RandomSplit" Core..=) Core.<$> randomSplit,
            ("SMS" Core..=) Core.<$> sms,
            ("Wait" Core..=) Core.<$> wait
          ]
      )

instance Core.FromJSON Activity where
  parseJSON =
    Core.withObject "Activity" Core.$
      \x ->
        Activity'
          Core.<$> (x Core..:? "CUSTOM")
          Core.<*> (x Core..:? "ConditionalSplit")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "EMAIL")
          Core.<*> (x Core..:? "Holdout")
          Core.<*> (x Core..:? "MultiCondition")
          Core.<*> (x Core..:? "PUSH")
          Core.<*> (x Core..:? "RandomSplit")
          Core.<*> (x Core..:? "SMS")
          Core.<*> (x Core..:? "Wait")
