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
-- Module      : Amazonka.Pinpoint.Types.Activity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.Activity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.ConditionalSplitActivity
import Amazonka.Pinpoint.Types.ContactCenterActivity
import Amazonka.Pinpoint.Types.CustomMessageActivity
import Amazonka.Pinpoint.Types.EmailMessageActivity
import Amazonka.Pinpoint.Types.HoldoutActivity
import Amazonka.Pinpoint.Types.MultiConditionalSplitActivity
import Amazonka.Pinpoint.Types.PushMessageActivity
import Amazonka.Pinpoint.Types.RandomSplitActivity
import Amazonka.Pinpoint.Types.SMSMessageActivity
import Amazonka.Pinpoint.Types.WaitActivity
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration and other settings for an activity in a
-- journey.
--
-- /See:/ 'newActivity' smart constructor.
data Activity = Activity'
  { -- | The settings for an SMS activity. This type of activity sends a text
    -- message to participants.
    sms :: Prelude.Maybe SMSMessageActivity,
    -- | The settings for a multivariate split activity. This type of activity
    -- sends participants down one of as many as five paths (including a
    -- default /Else/ path) in a journey, based on conditions that you specify.
    multiCondition :: Prelude.Maybe MultiConditionalSplitActivity,
    -- | The settings for an email activity. This type of activity sends an email
    -- message to participants.
    email :: Prelude.Maybe EmailMessageActivity,
    -- | The settings for a push notification activity. This type of activity
    -- sends a push notification to participants.
    push :: Prelude.Maybe PushMessageActivity,
    -- | The custom description of the activity.
    description :: Prelude.Maybe Prelude.Text,
    -- | The settings for a wait activity. This type of activity waits for a
    -- certain amount of time or until a specific date and time before moving
    -- participants to the next activity in a journey.
    wait :: Prelude.Maybe WaitActivity,
    -- | The settings for a random split activity. This type of activity randomly
    -- sends specified percentages of participants down one of as many as five
    -- paths in a journey, based on conditions that you specify.
    randomSplit :: Prelude.Maybe RandomSplitActivity,
    -- | The settings for a connect activity. This type of activity initiates a
    -- contact center call to participants.
    contactCenter :: Prelude.Maybe ContactCenterActivity,
    -- | The settings for a custom message activity. This type of activity calls
    -- an AWS Lambda function or web hook that sends messages to participants.
    custom :: Prelude.Maybe CustomMessageActivity,
    -- | The settings for a yes\/no split activity. This type of activity sends
    -- participants down one of two paths in a journey, based on conditions
    -- that you specify.
    conditionalSplit :: Prelude.Maybe ConditionalSplitActivity,
    -- | The settings for a holdout activity. This type of activity stops a
    -- journey for a specified percentage of participants.
    holdout :: Prelude.Maybe HoldoutActivity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Activity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sms', 'activity_sms' - The settings for an SMS activity. This type of activity sends a text
-- message to participants.
--
-- 'multiCondition', 'activity_multiCondition' - The settings for a multivariate split activity. This type of activity
-- sends participants down one of as many as five paths (including a
-- default /Else/ path) in a journey, based on conditions that you specify.
--
-- 'email', 'activity_email' - The settings for an email activity. This type of activity sends an email
-- message to participants.
--
-- 'push', 'activity_push' - The settings for a push notification activity. This type of activity
-- sends a push notification to participants.
--
-- 'description', 'activity_description' - The custom description of the activity.
--
-- 'wait', 'activity_wait' - The settings for a wait activity. This type of activity waits for a
-- certain amount of time or until a specific date and time before moving
-- participants to the next activity in a journey.
--
-- 'randomSplit', 'activity_randomSplit' - The settings for a random split activity. This type of activity randomly
-- sends specified percentages of participants down one of as many as five
-- paths in a journey, based on conditions that you specify.
--
-- 'contactCenter', 'activity_contactCenter' - The settings for a connect activity. This type of activity initiates a
-- contact center call to participants.
--
-- 'custom', 'activity_custom' - The settings for a custom message activity. This type of activity calls
-- an AWS Lambda function or web hook that sends messages to participants.
--
-- 'conditionalSplit', 'activity_conditionalSplit' - The settings for a yes\/no split activity. This type of activity sends
-- participants down one of two paths in a journey, based on conditions
-- that you specify.
--
-- 'holdout', 'activity_holdout' - The settings for a holdout activity. This type of activity stops a
-- journey for a specified percentage of participants.
newActivity ::
  Activity
newActivity =
  Activity'
    { sms = Prelude.Nothing,
      multiCondition = Prelude.Nothing,
      email = Prelude.Nothing,
      push = Prelude.Nothing,
      description = Prelude.Nothing,
      wait = Prelude.Nothing,
      randomSplit = Prelude.Nothing,
      contactCenter = Prelude.Nothing,
      custom = Prelude.Nothing,
      conditionalSplit = Prelude.Nothing,
      holdout = Prelude.Nothing
    }

-- | The settings for an SMS activity. This type of activity sends a text
-- message to participants.
activity_sms :: Lens.Lens' Activity (Prelude.Maybe SMSMessageActivity)
activity_sms = Lens.lens (\Activity' {sms} -> sms) (\s@Activity' {} a -> s {sms = a} :: Activity)

-- | The settings for a multivariate split activity. This type of activity
-- sends participants down one of as many as five paths (including a
-- default /Else/ path) in a journey, based on conditions that you specify.
activity_multiCondition :: Lens.Lens' Activity (Prelude.Maybe MultiConditionalSplitActivity)
activity_multiCondition = Lens.lens (\Activity' {multiCondition} -> multiCondition) (\s@Activity' {} a -> s {multiCondition = a} :: Activity)

-- | The settings for an email activity. This type of activity sends an email
-- message to participants.
activity_email :: Lens.Lens' Activity (Prelude.Maybe EmailMessageActivity)
activity_email = Lens.lens (\Activity' {email} -> email) (\s@Activity' {} a -> s {email = a} :: Activity)

-- | The settings for a push notification activity. This type of activity
-- sends a push notification to participants.
activity_push :: Lens.Lens' Activity (Prelude.Maybe PushMessageActivity)
activity_push = Lens.lens (\Activity' {push} -> push) (\s@Activity' {} a -> s {push = a} :: Activity)

-- | The custom description of the activity.
activity_description :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_description = Lens.lens (\Activity' {description} -> description) (\s@Activity' {} a -> s {description = a} :: Activity)

-- | The settings for a wait activity. This type of activity waits for a
-- certain amount of time or until a specific date and time before moving
-- participants to the next activity in a journey.
activity_wait :: Lens.Lens' Activity (Prelude.Maybe WaitActivity)
activity_wait = Lens.lens (\Activity' {wait} -> wait) (\s@Activity' {} a -> s {wait = a} :: Activity)

-- | The settings for a random split activity. This type of activity randomly
-- sends specified percentages of participants down one of as many as five
-- paths in a journey, based on conditions that you specify.
activity_randomSplit :: Lens.Lens' Activity (Prelude.Maybe RandomSplitActivity)
activity_randomSplit = Lens.lens (\Activity' {randomSplit} -> randomSplit) (\s@Activity' {} a -> s {randomSplit = a} :: Activity)

-- | The settings for a connect activity. This type of activity initiates a
-- contact center call to participants.
activity_contactCenter :: Lens.Lens' Activity (Prelude.Maybe ContactCenterActivity)
activity_contactCenter = Lens.lens (\Activity' {contactCenter} -> contactCenter) (\s@Activity' {} a -> s {contactCenter = a} :: Activity)

-- | The settings for a custom message activity. This type of activity calls
-- an AWS Lambda function or web hook that sends messages to participants.
activity_custom :: Lens.Lens' Activity (Prelude.Maybe CustomMessageActivity)
activity_custom = Lens.lens (\Activity' {custom} -> custom) (\s@Activity' {} a -> s {custom = a} :: Activity)

-- | The settings for a yes\/no split activity. This type of activity sends
-- participants down one of two paths in a journey, based on conditions
-- that you specify.
activity_conditionalSplit :: Lens.Lens' Activity (Prelude.Maybe ConditionalSplitActivity)
activity_conditionalSplit = Lens.lens (\Activity' {conditionalSplit} -> conditionalSplit) (\s@Activity' {} a -> s {conditionalSplit = a} :: Activity)

-- | The settings for a holdout activity. This type of activity stops a
-- journey for a specified percentage of participants.
activity_holdout :: Lens.Lens' Activity (Prelude.Maybe HoldoutActivity)
activity_holdout = Lens.lens (\Activity' {holdout} -> holdout) (\s@Activity' {} a -> s {holdout = a} :: Activity)

instance Core.FromJSON Activity where
  parseJSON =
    Core.withObject
      "Activity"
      ( \x ->
          Activity'
            Prelude.<$> (x Core..:? "SMS")
            Prelude.<*> (x Core..:? "MultiCondition")
            Prelude.<*> (x Core..:? "EMAIL")
            Prelude.<*> (x Core..:? "PUSH")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Wait")
            Prelude.<*> (x Core..:? "RandomSplit")
            Prelude.<*> (x Core..:? "ContactCenter")
            Prelude.<*> (x Core..:? "CUSTOM")
            Prelude.<*> (x Core..:? "ConditionalSplit")
            Prelude.<*> (x Core..:? "Holdout")
      )

instance Prelude.Hashable Activity where
  hashWithSalt _salt Activity' {..} =
    _salt `Prelude.hashWithSalt` sms
      `Prelude.hashWithSalt` multiCondition
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` push
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` wait
      `Prelude.hashWithSalt` randomSplit
      `Prelude.hashWithSalt` contactCenter
      `Prelude.hashWithSalt` custom
      `Prelude.hashWithSalt` conditionalSplit
      `Prelude.hashWithSalt` holdout

instance Prelude.NFData Activity where
  rnf Activity' {..} =
    Prelude.rnf sms
      `Prelude.seq` Prelude.rnf multiCondition
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf push
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf wait
      `Prelude.seq` Prelude.rnf randomSplit
      `Prelude.seq` Prelude.rnf contactCenter
      `Prelude.seq` Prelude.rnf custom
      `Prelude.seq` Prelude.rnf conditionalSplit
      `Prelude.seq` Prelude.rnf holdout

instance Core.ToJSON Activity where
  toJSON Activity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SMS" Core..=) Prelude.<$> sms,
            ("MultiCondition" Core..=)
              Prelude.<$> multiCondition,
            ("EMAIL" Core..=) Prelude.<$> email,
            ("PUSH" Core..=) Prelude.<$> push,
            ("Description" Core..=) Prelude.<$> description,
            ("Wait" Core..=) Prelude.<$> wait,
            ("RandomSplit" Core..=) Prelude.<$> randomSplit,
            ("ContactCenter" Core..=) Prelude.<$> contactCenter,
            ("CUSTOM" Core..=) Prelude.<$> custom,
            ("ConditionalSplit" Core..=)
              Prelude.<$> conditionalSplit,
            ("Holdout" Core..=) Prelude.<$> holdout
          ]
      )
