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
-- Module      : Network.AWS.Pinpoint.Types.Activity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Activity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ConditionalSplitActivity
import Network.AWS.Pinpoint.Types.CustomMessageActivity
import Network.AWS.Pinpoint.Types.EmailMessageActivity
import Network.AWS.Pinpoint.Types.HoldoutActivity
import Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity
import Network.AWS.Pinpoint.Types.PushMessageActivity
import Network.AWS.Pinpoint.Types.RandomSplitActivity
import Network.AWS.Pinpoint.Types.SMSMessageActivity
import Network.AWS.Pinpoint.Types.WaitActivity

-- | Specifies the configuration and other settings for an activity in a
-- journey.
--
-- /See:/ 'newActivity' smart constructor.
data Activity = Activity'
  { -- | The settings for a yes\/no split activity. This type of activity sends
    -- participants down one of two paths in a journey, based on conditions
    -- that you specify.
    conditionalSplit :: Core.Maybe ConditionalSplitActivity,
    -- | The settings for a push notification activity. This type of activity
    -- sends a push notification to participants.
    push :: Core.Maybe PushMessageActivity,
    -- | The settings for a custom message activity. This type of activity calls
    -- an AWS Lambda function or web hook that sends messages to participants.
    custom :: Core.Maybe CustomMessageActivity,
    -- | The settings for a wait activity. This type of activity waits for a
    -- certain amount of time or until a specific date and time before moving
    -- participants to the next activity in a journey.
    wait :: Core.Maybe WaitActivity,
    -- | The settings for a multivariate split activity. This type of activity
    -- sends participants down one of as many as five paths (including a
    -- default /Else/ path) in a journey, based on conditions that you specify.
    multiCondition :: Core.Maybe MultiConditionalSplitActivity,
    -- | The settings for an email activity. This type of activity sends an email
    -- message to participants.
    email :: Core.Maybe EmailMessageActivity,
    -- | The settings for a holdout activity. This type of activity stops a
    -- journey for a specified percentage of participants.
    holdout :: Core.Maybe HoldoutActivity,
    -- | The settings for a random split activity. This type of activity randomly
    -- sends specified percentages of participants down one of as many as five
    -- paths in a journey, based on conditions that you specify.
    randomSplit :: Core.Maybe RandomSplitActivity,
    -- | The custom description of the activity.
    description :: Core.Maybe Core.Text,
    -- | The settings for an SMS activity. This type of activity sends a text
    -- message to participants.
    sms :: Core.Maybe SMSMessageActivity
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Activity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalSplit', 'activity_conditionalSplit' - The settings for a yes\/no split activity. This type of activity sends
-- participants down one of two paths in a journey, based on conditions
-- that you specify.
--
-- 'push', 'activity_push' - The settings for a push notification activity. This type of activity
-- sends a push notification to participants.
--
-- 'custom', 'activity_custom' - The settings for a custom message activity. This type of activity calls
-- an AWS Lambda function or web hook that sends messages to participants.
--
-- 'wait', 'activity_wait' - The settings for a wait activity. This type of activity waits for a
-- certain amount of time or until a specific date and time before moving
-- participants to the next activity in a journey.
--
-- 'multiCondition', 'activity_multiCondition' - The settings for a multivariate split activity. This type of activity
-- sends participants down one of as many as five paths (including a
-- default /Else/ path) in a journey, based on conditions that you specify.
--
-- 'email', 'activity_email' - The settings for an email activity. This type of activity sends an email
-- message to participants.
--
-- 'holdout', 'activity_holdout' - The settings for a holdout activity. This type of activity stops a
-- journey for a specified percentage of participants.
--
-- 'randomSplit', 'activity_randomSplit' - The settings for a random split activity. This type of activity randomly
-- sends specified percentages of participants down one of as many as five
-- paths in a journey, based on conditions that you specify.
--
-- 'description', 'activity_description' - The custom description of the activity.
--
-- 'sms', 'activity_sms' - The settings for an SMS activity. This type of activity sends a text
-- message to participants.
newActivity ::
  Activity
newActivity =
  Activity'
    { conditionalSplit = Core.Nothing,
      push = Core.Nothing,
      custom = Core.Nothing,
      wait = Core.Nothing,
      multiCondition = Core.Nothing,
      email = Core.Nothing,
      holdout = Core.Nothing,
      randomSplit = Core.Nothing,
      description = Core.Nothing,
      sms = Core.Nothing
    }

-- | The settings for a yes\/no split activity. This type of activity sends
-- participants down one of two paths in a journey, based on conditions
-- that you specify.
activity_conditionalSplit :: Lens.Lens' Activity (Core.Maybe ConditionalSplitActivity)
activity_conditionalSplit = Lens.lens (\Activity' {conditionalSplit} -> conditionalSplit) (\s@Activity' {} a -> s {conditionalSplit = a} :: Activity)

-- | The settings for a push notification activity. This type of activity
-- sends a push notification to participants.
activity_push :: Lens.Lens' Activity (Core.Maybe PushMessageActivity)
activity_push = Lens.lens (\Activity' {push} -> push) (\s@Activity' {} a -> s {push = a} :: Activity)

-- | The settings for a custom message activity. This type of activity calls
-- an AWS Lambda function or web hook that sends messages to participants.
activity_custom :: Lens.Lens' Activity (Core.Maybe CustomMessageActivity)
activity_custom = Lens.lens (\Activity' {custom} -> custom) (\s@Activity' {} a -> s {custom = a} :: Activity)

-- | The settings for a wait activity. This type of activity waits for a
-- certain amount of time or until a specific date and time before moving
-- participants to the next activity in a journey.
activity_wait :: Lens.Lens' Activity (Core.Maybe WaitActivity)
activity_wait = Lens.lens (\Activity' {wait} -> wait) (\s@Activity' {} a -> s {wait = a} :: Activity)

-- | The settings for a multivariate split activity. This type of activity
-- sends participants down one of as many as five paths (including a
-- default /Else/ path) in a journey, based on conditions that you specify.
activity_multiCondition :: Lens.Lens' Activity (Core.Maybe MultiConditionalSplitActivity)
activity_multiCondition = Lens.lens (\Activity' {multiCondition} -> multiCondition) (\s@Activity' {} a -> s {multiCondition = a} :: Activity)

-- | The settings for an email activity. This type of activity sends an email
-- message to participants.
activity_email :: Lens.Lens' Activity (Core.Maybe EmailMessageActivity)
activity_email = Lens.lens (\Activity' {email} -> email) (\s@Activity' {} a -> s {email = a} :: Activity)

-- | The settings for a holdout activity. This type of activity stops a
-- journey for a specified percentage of participants.
activity_holdout :: Lens.Lens' Activity (Core.Maybe HoldoutActivity)
activity_holdout = Lens.lens (\Activity' {holdout} -> holdout) (\s@Activity' {} a -> s {holdout = a} :: Activity)

-- | The settings for a random split activity. This type of activity randomly
-- sends specified percentages of participants down one of as many as five
-- paths in a journey, based on conditions that you specify.
activity_randomSplit :: Lens.Lens' Activity (Core.Maybe RandomSplitActivity)
activity_randomSplit = Lens.lens (\Activity' {randomSplit} -> randomSplit) (\s@Activity' {} a -> s {randomSplit = a} :: Activity)

-- | The custom description of the activity.
activity_description :: Lens.Lens' Activity (Core.Maybe Core.Text)
activity_description = Lens.lens (\Activity' {description} -> description) (\s@Activity' {} a -> s {description = a} :: Activity)

-- | The settings for an SMS activity. This type of activity sends a text
-- message to participants.
activity_sms :: Lens.Lens' Activity (Core.Maybe SMSMessageActivity)
activity_sms = Lens.lens (\Activity' {sms} -> sms) (\s@Activity' {} a -> s {sms = a} :: Activity)

instance Core.FromJSON Activity where
  parseJSON =
    Core.withObject
      "Activity"
      ( \x ->
          Activity'
            Core.<$> (x Core..:? "ConditionalSplit")
            Core.<*> (x Core..:? "PUSH")
            Core.<*> (x Core..:? "CUSTOM")
            Core.<*> (x Core..:? "Wait")
            Core.<*> (x Core..:? "MultiCondition")
            Core.<*> (x Core..:? "EMAIL")
            Core.<*> (x Core..:? "Holdout")
            Core.<*> (x Core..:? "RandomSplit")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SMS")
      )

instance Core.Hashable Activity

instance Core.NFData Activity

instance Core.ToJSON Activity where
  toJSON Activity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConditionalSplit" Core..=)
              Core.<$> conditionalSplit,
            ("PUSH" Core..=) Core.<$> push,
            ("CUSTOM" Core..=) Core.<$> custom,
            ("Wait" Core..=) Core.<$> wait,
            ("MultiCondition" Core..=) Core.<$> multiCondition,
            ("EMAIL" Core..=) Core.<$> email,
            ("Holdout" Core..=) Core.<$> holdout,
            ("RandomSplit" Core..=) Core.<$> randomSplit,
            ("Description" Core..=) Core.<$> description,
            ("SMS" Core..=) Core.<$> sms
          ]
      )
