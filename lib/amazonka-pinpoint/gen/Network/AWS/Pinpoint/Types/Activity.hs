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
    aConditionalSplit,
    aEMAIL,
    aMultiCondition,
    aCUSTOM,
    aWait,
    aRandomSplit,
    aHoldout,
    aSMS,
    aPUSH,
    aDescription,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration and other settings for an activity in a journey.
--
-- /See:/ 'mkActivity' smart constructor.
data Activity = Activity'
  { conditionalSplit ::
      Lude.Maybe ConditionalSplitActivity,
    eMAIL :: Lude.Maybe EmailMessageActivity,
    multiCondition :: Lude.Maybe MultiConditionalSplitActivity,
    cUSTOM :: Lude.Maybe CustomMessageActivity,
    wait :: Lude.Maybe WaitActivity,
    randomSplit :: Lude.Maybe RandomSplitActivity,
    holdout :: Lude.Maybe HoldoutActivity,
    sMS :: Lude.Maybe SMSMessageActivity,
    pUSH :: Lude.Maybe PushMessageActivity,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Activity' with the minimum fields required to make a request.
--
-- * 'cUSTOM' - The settings for a custom message activity. This type of activity calls an AWS Lambda function or web hook that sends messages to participants.
-- * 'conditionalSplit' - The settings for a yes/no split activity. This type of activity sends participants down one of two paths in a journey, based on conditions that you specify.
-- * 'description' - The custom description of the activity.
-- * 'eMAIL' - The settings for an email activity. This type of activity sends an email message to participants.
-- * 'holdout' - The settings for a holdout activity. This type of activity stops a journey for a specified percentage of participants.
-- * 'multiCondition' - The settings for a multivariate split activity. This type of activity sends participants down one of as many as five paths (including a default /Else/ path) in a journey, based on conditions that you specify.
-- * 'pUSH' - The settings for a push notification activity. This type of activity sends a push notification to participants.
-- * 'randomSplit' - The settings for a random split activity. This type of activity randomly sends specified percentages of participants down one of as many as five paths in a journey, based on conditions that you specify.
-- * 'sMS' - The settings for an SMS activity. This type of activity sends a text message to participants.
-- * 'wait' - The settings for a wait activity. This type of activity waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
mkActivity ::
  Activity
mkActivity =
  Activity'
    { conditionalSplit = Lude.Nothing,
      eMAIL = Lude.Nothing,
      multiCondition = Lude.Nothing,
      cUSTOM = Lude.Nothing,
      wait = Lude.Nothing,
      randomSplit = Lude.Nothing,
      holdout = Lude.Nothing,
      sMS = Lude.Nothing,
      pUSH = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The settings for a yes/no split activity. This type of activity sends participants down one of two paths in a journey, based on conditions that you specify.
--
-- /Note:/ Consider using 'conditionalSplit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aConditionalSplit :: Lens.Lens' Activity (Lude.Maybe ConditionalSplitActivity)
aConditionalSplit = Lens.lens (conditionalSplit :: Activity -> Lude.Maybe ConditionalSplitActivity) (\s a -> s {conditionalSplit = a} :: Activity)
{-# DEPRECATED aConditionalSplit "Use generic-lens or generic-optics with 'conditionalSplit' instead." #-}

-- | The settings for an email activity. This type of activity sends an email message to participants.
--
-- /Note:/ Consider using 'eMAIL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEMAIL :: Lens.Lens' Activity (Lude.Maybe EmailMessageActivity)
aEMAIL = Lens.lens (eMAIL :: Activity -> Lude.Maybe EmailMessageActivity) (\s a -> s {eMAIL = a} :: Activity)
{-# DEPRECATED aEMAIL "Use generic-lens or generic-optics with 'eMAIL' instead." #-}

-- | The settings for a multivariate split activity. This type of activity sends participants down one of as many as five paths (including a default /Else/ path) in a journey, based on conditions that you specify.
--
-- /Note:/ Consider using 'multiCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMultiCondition :: Lens.Lens' Activity (Lude.Maybe MultiConditionalSplitActivity)
aMultiCondition = Lens.lens (multiCondition :: Activity -> Lude.Maybe MultiConditionalSplitActivity) (\s a -> s {multiCondition = a} :: Activity)
{-# DEPRECATED aMultiCondition "Use generic-lens or generic-optics with 'multiCondition' instead." #-}

-- | The settings for a custom message activity. This type of activity calls an AWS Lambda function or web hook that sends messages to participants.
--
-- /Note:/ Consider using 'cUSTOM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCUSTOM :: Lens.Lens' Activity (Lude.Maybe CustomMessageActivity)
aCUSTOM = Lens.lens (cUSTOM :: Activity -> Lude.Maybe CustomMessageActivity) (\s a -> s {cUSTOM = a} :: Activity)
{-# DEPRECATED aCUSTOM "Use generic-lens or generic-optics with 'cUSTOM' instead." #-}

-- | The settings for a wait activity. This type of activity waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
-- /Note:/ Consider using 'wait' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aWait :: Lens.Lens' Activity (Lude.Maybe WaitActivity)
aWait = Lens.lens (wait :: Activity -> Lude.Maybe WaitActivity) (\s a -> s {wait = a} :: Activity)
{-# DEPRECATED aWait "Use generic-lens or generic-optics with 'wait' instead." #-}

-- | The settings for a random split activity. This type of activity randomly sends specified percentages of participants down one of as many as five paths in a journey, based on conditions that you specify.
--
-- /Note:/ Consider using 'randomSplit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRandomSplit :: Lens.Lens' Activity (Lude.Maybe RandomSplitActivity)
aRandomSplit = Lens.lens (randomSplit :: Activity -> Lude.Maybe RandomSplitActivity) (\s a -> s {randomSplit = a} :: Activity)
{-# DEPRECATED aRandomSplit "Use generic-lens or generic-optics with 'randomSplit' instead." #-}

-- | The settings for a holdout activity. This type of activity stops a journey for a specified percentage of participants.
--
-- /Note:/ Consider using 'holdout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aHoldout :: Lens.Lens' Activity (Lude.Maybe HoldoutActivity)
aHoldout = Lens.lens (holdout :: Activity -> Lude.Maybe HoldoutActivity) (\s a -> s {holdout = a} :: Activity)
{-# DEPRECATED aHoldout "Use generic-lens or generic-optics with 'holdout' instead." #-}

-- | The settings for an SMS activity. This type of activity sends a text message to participants.
--
-- /Note:/ Consider using 'sMS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSMS :: Lens.Lens' Activity (Lude.Maybe SMSMessageActivity)
aSMS = Lens.lens (sMS :: Activity -> Lude.Maybe SMSMessageActivity) (\s a -> s {sMS = a} :: Activity)
{-# DEPRECATED aSMS "Use generic-lens or generic-optics with 'sMS' instead." #-}

-- | The settings for a push notification activity. This type of activity sends a push notification to participants.
--
-- /Note:/ Consider using 'pUSH' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPUSH :: Lens.Lens' Activity (Lude.Maybe PushMessageActivity)
aPUSH = Lens.lens (pUSH :: Activity -> Lude.Maybe PushMessageActivity) (\s a -> s {pUSH = a} :: Activity)
{-# DEPRECATED aPUSH "Use generic-lens or generic-optics with 'pUSH' instead." #-}

-- | The custom description of the activity.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Activity (Lude.Maybe Lude.Text)
aDescription = Lens.lens (description :: Activity -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Activity)
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Activity where
  parseJSON =
    Lude.withObject
      "Activity"
      ( \x ->
          Activity'
            Lude.<$> (x Lude..:? "ConditionalSplit")
            Lude.<*> (x Lude..:? "EMAIL")
            Lude.<*> (x Lude..:? "MultiCondition")
            Lude.<*> (x Lude..:? "CUSTOM")
            Lude.<*> (x Lude..:? "Wait")
            Lude.<*> (x Lude..:? "RandomSplit")
            Lude.<*> (x Lude..:? "Holdout")
            Lude.<*> (x Lude..:? "SMS")
            Lude.<*> (x Lude..:? "PUSH")
            Lude.<*> (x Lude..:? "Description")
      )

instance Lude.ToJSON Activity where
  toJSON Activity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConditionalSplit" Lude..=) Lude.<$> conditionalSplit,
            ("EMAIL" Lude..=) Lude.<$> eMAIL,
            ("MultiCondition" Lude..=) Lude.<$> multiCondition,
            ("CUSTOM" Lude..=) Lude.<$> cUSTOM,
            ("Wait" Lude..=) Lude.<$> wait,
            ("RandomSplit" Lude..=) Lude.<$> randomSplit,
            ("Holdout" Lude..=) Lude.<$> holdout,
            ("SMS" Lude..=) Lude.<$> sMS,
            ("PUSH" Lude..=) Lude.<$> pUSH,
            ("Description" Lude..=) Lude.<$> description
          ]
      )
