{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
  ( AutomaticInputFailoverSettings (..),

    -- * Smart constructor
    mkAutomaticInputFailoverSettings,

    -- * Lenses
    aifsFailoverConditions,
    aifsErrorClearTimeMsec,
    aifsInputPreference,
    aifsSecondaryInputId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FailoverCondition
import Network.AWS.MediaLive.Types.InputPreference
import qualified Network.AWS.Prelude as Lude

-- | The settings for Automatic Input Failover.
--
-- /See:/ 'mkAutomaticInputFailoverSettings' smart constructor.
data AutomaticInputFailoverSettings = AutomaticInputFailoverSettings'
  { failoverConditions ::
      Lude.Maybe
        [FailoverCondition],
    errorClearTimeMsec ::
      Lude.Maybe Lude.Natural,
    inputPreference ::
      Lude.Maybe InputPreference,
    secondaryInputId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomaticInputFailoverSettings' with the minimum fields required to make a request.
--
-- * 'errorClearTimeMsec' - This clear time defines the requirement a recovered input must meet to be considered healthy. The input must have no failover conditions for this length of time. Enter a time in milliseconds. This value is particularly important if the input_preference for the failover pair is set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will switch back to the primary input.
-- * 'failoverConditions' - A list of failover conditions. If any of these conditions occur, MediaLive will perform a failover to the other input.
-- * 'inputPreference' - Input preference when deciding which input to make active when a previously failed input has recovered.
-- * 'secondaryInputId' - The input ID of the secondary input in the automatic input failover pair.
mkAutomaticInputFailoverSettings ::
  -- | 'secondaryInputId'
  Lude.Text ->
  AutomaticInputFailoverSettings
mkAutomaticInputFailoverSettings pSecondaryInputId_ =
  AutomaticInputFailoverSettings'
    { failoverConditions =
        Lude.Nothing,
      errorClearTimeMsec = Lude.Nothing,
      inputPreference = Lude.Nothing,
      secondaryInputId = pSecondaryInputId_
    }

-- | A list of failover conditions. If any of these conditions occur, MediaLive will perform a failover to the other input.
--
-- /Note:/ Consider using 'failoverConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsFailoverConditions :: Lens.Lens' AutomaticInputFailoverSettings (Lude.Maybe [FailoverCondition])
aifsFailoverConditions = Lens.lens (failoverConditions :: AutomaticInputFailoverSettings -> Lude.Maybe [FailoverCondition]) (\s a -> s {failoverConditions = a} :: AutomaticInputFailoverSettings)
{-# DEPRECATED aifsFailoverConditions "Use generic-lens or generic-optics with 'failoverConditions' instead." #-}

-- | This clear time defines the requirement a recovered input must meet to be considered healthy. The input must have no failover conditions for this length of time. Enter a time in milliseconds. This value is particularly important if the input_preference for the failover pair is set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will switch back to the primary input.
--
-- /Note:/ Consider using 'errorClearTimeMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsErrorClearTimeMsec :: Lens.Lens' AutomaticInputFailoverSettings (Lude.Maybe Lude.Natural)
aifsErrorClearTimeMsec = Lens.lens (errorClearTimeMsec :: AutomaticInputFailoverSettings -> Lude.Maybe Lude.Natural) (\s a -> s {errorClearTimeMsec = a} :: AutomaticInputFailoverSettings)
{-# DEPRECATED aifsErrorClearTimeMsec "Use generic-lens or generic-optics with 'errorClearTimeMsec' instead." #-}

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- /Note:/ Consider using 'inputPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsInputPreference :: Lens.Lens' AutomaticInputFailoverSettings (Lude.Maybe InputPreference)
aifsInputPreference = Lens.lens (inputPreference :: AutomaticInputFailoverSettings -> Lude.Maybe InputPreference) (\s a -> s {inputPreference = a} :: AutomaticInputFailoverSettings)
{-# DEPRECATED aifsInputPreference "Use generic-lens or generic-optics with 'inputPreference' instead." #-}

-- | The input ID of the secondary input in the automatic input failover pair.
--
-- /Note:/ Consider using 'secondaryInputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsSecondaryInputId :: Lens.Lens' AutomaticInputFailoverSettings Lude.Text
aifsSecondaryInputId = Lens.lens (secondaryInputId :: AutomaticInputFailoverSettings -> Lude.Text) (\s a -> s {secondaryInputId = a} :: AutomaticInputFailoverSettings)
{-# DEPRECATED aifsSecondaryInputId "Use generic-lens or generic-optics with 'secondaryInputId' instead." #-}

instance Lude.FromJSON AutomaticInputFailoverSettings where
  parseJSON =
    Lude.withObject
      "AutomaticInputFailoverSettings"
      ( \x ->
          AutomaticInputFailoverSettings'
            Lude.<$> (x Lude..:? "failoverConditions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "errorClearTimeMsec")
            Lude.<*> (x Lude..:? "inputPreference")
            Lude.<*> (x Lude..: "secondaryInputId")
      )

instance Lude.ToJSON AutomaticInputFailoverSettings where
  toJSON AutomaticInputFailoverSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("failoverConditions" Lude..=) Lude.<$> failoverConditions,
            ("errorClearTimeMsec" Lude..=) Lude.<$> errorClearTimeMsec,
            ("inputPreference" Lude..=) Lude.<$> inputPreference,
            Lude.Just ("secondaryInputId" Lude..= secondaryInputId)
          ]
      )
