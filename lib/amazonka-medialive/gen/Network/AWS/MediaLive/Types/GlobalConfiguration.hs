{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfiguration
  ( GlobalConfiguration (..),

    -- * Smart constructor
    mkGlobalConfiguration,

    -- * Lenses
    gcOutputLockingMode,
    gcInputLossBehavior,
    gcInitialAudioGain,
    gcSupportLowFramerateInputs,
    gcInputEndAction,
    gcOutputTimingSource,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
import Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputLockingMode
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
import Network.AWS.MediaLive.Types.InputLossBehavior
import qualified Network.AWS.Prelude as Lude

-- | Global Configuration
--
-- /See:/ 'mkGlobalConfiguration' smart constructor.
data GlobalConfiguration = GlobalConfiguration'
  { outputLockingMode ::
      Lude.Maybe GlobalConfigurationOutputLockingMode,
    inputLossBehavior :: Lude.Maybe InputLossBehavior,
    initialAudioGain :: Lude.Maybe Lude.Int,
    supportLowFramerateInputs ::
      Lude.Maybe GlobalConfigurationLowFramerateInputs,
    inputEndAction ::
      Lude.Maybe GlobalConfigurationInputEndAction,
    outputTimingSource ::
      Lude.Maybe GlobalConfigurationOutputTimingSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalConfiguration' with the minimum fields required to make a request.
--
-- * 'initialAudioGain' - Value to set the initial audio gain for the Live Event.
-- * 'inputEndAction' - Indicates the action to take when the current input completes (e.g. end-of-file). When switchAndLoopInputs is configured the encoder will restart at the beginning of the first input.  When "none" is configured the encoder will transcode either black, a solid color, or a user specified slate images per the "Input Loss Behavior" configuration until the next input switch occurs (which is controlled through the Channel Schedule API).
-- * 'inputLossBehavior' - Settings for system actions when input is lost.
-- * 'outputLockingMode' - Indicates how MediaLive pipelines are synchronized.
--
--
-- PIPELINE_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the other.
-- EPOCH_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the Unix epoch.
-- * 'outputTimingSource' - Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
-- * 'supportLowFramerateInputs' - Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
mkGlobalConfiguration ::
  GlobalConfiguration
mkGlobalConfiguration =
  GlobalConfiguration'
    { outputLockingMode = Lude.Nothing,
      inputLossBehavior = Lude.Nothing,
      initialAudioGain = Lude.Nothing,
      supportLowFramerateInputs = Lude.Nothing,
      inputEndAction = Lude.Nothing,
      outputTimingSource = Lude.Nothing
    }

-- | Indicates how MediaLive pipelines are synchronized.
--
--
-- PIPELINE_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the other.
-- EPOCH_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the Unix epoch.
--
-- /Note:/ Consider using 'outputLockingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcOutputLockingMode :: Lens.Lens' GlobalConfiguration (Lude.Maybe GlobalConfigurationOutputLockingMode)
gcOutputLockingMode = Lens.lens (outputLockingMode :: GlobalConfiguration -> Lude.Maybe GlobalConfigurationOutputLockingMode) (\s a -> s {outputLockingMode = a} :: GlobalConfiguration)
{-# DEPRECATED gcOutputLockingMode "Use generic-lens or generic-optics with 'outputLockingMode' instead." #-}

-- | Settings for system actions when input is lost.
--
-- /Note:/ Consider using 'inputLossBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcInputLossBehavior :: Lens.Lens' GlobalConfiguration (Lude.Maybe InputLossBehavior)
gcInputLossBehavior = Lens.lens (inputLossBehavior :: GlobalConfiguration -> Lude.Maybe InputLossBehavior) (\s a -> s {inputLossBehavior = a} :: GlobalConfiguration)
{-# DEPRECATED gcInputLossBehavior "Use generic-lens or generic-optics with 'inputLossBehavior' instead." #-}

-- | Value to set the initial audio gain for the Live Event.
--
-- /Note:/ Consider using 'initialAudioGain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcInitialAudioGain :: Lens.Lens' GlobalConfiguration (Lude.Maybe Lude.Int)
gcInitialAudioGain = Lens.lens (initialAudioGain :: GlobalConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {initialAudioGain = a} :: GlobalConfiguration)
{-# DEPRECATED gcInitialAudioGain "Use generic-lens or generic-optics with 'initialAudioGain' instead." #-}

-- | Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
--
-- /Note:/ Consider using 'supportLowFramerateInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcSupportLowFramerateInputs :: Lens.Lens' GlobalConfiguration (Lude.Maybe GlobalConfigurationLowFramerateInputs)
gcSupportLowFramerateInputs = Lens.lens (supportLowFramerateInputs :: GlobalConfiguration -> Lude.Maybe GlobalConfigurationLowFramerateInputs) (\s a -> s {supportLowFramerateInputs = a} :: GlobalConfiguration)
{-# DEPRECATED gcSupportLowFramerateInputs "Use generic-lens or generic-optics with 'supportLowFramerateInputs' instead." #-}

-- | Indicates the action to take when the current input completes (e.g. end-of-file). When switchAndLoopInputs is configured the encoder will restart at the beginning of the first input.  When "none" is configured the encoder will transcode either black, a solid color, or a user specified slate images per the "Input Loss Behavior" configuration until the next input switch occurs (which is controlled through the Channel Schedule API).
--
-- /Note:/ Consider using 'inputEndAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcInputEndAction :: Lens.Lens' GlobalConfiguration (Lude.Maybe GlobalConfigurationInputEndAction)
gcInputEndAction = Lens.lens (inputEndAction :: GlobalConfiguration -> Lude.Maybe GlobalConfigurationInputEndAction) (\s a -> s {inputEndAction = a} :: GlobalConfiguration)
{-# DEPRECATED gcInputEndAction "Use generic-lens or generic-optics with 'inputEndAction' instead." #-}

-- | Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
--
-- /Note:/ Consider using 'outputTimingSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcOutputTimingSource :: Lens.Lens' GlobalConfiguration (Lude.Maybe GlobalConfigurationOutputTimingSource)
gcOutputTimingSource = Lens.lens (outputTimingSource :: GlobalConfiguration -> Lude.Maybe GlobalConfigurationOutputTimingSource) (\s a -> s {outputTimingSource = a} :: GlobalConfiguration)
{-# DEPRECATED gcOutputTimingSource "Use generic-lens or generic-optics with 'outputTimingSource' instead." #-}

instance Lude.FromJSON GlobalConfiguration where
  parseJSON =
    Lude.withObject
      "GlobalConfiguration"
      ( \x ->
          GlobalConfiguration'
            Lude.<$> (x Lude..:? "outputLockingMode")
            Lude.<*> (x Lude..:? "inputLossBehavior")
            Lude.<*> (x Lude..:? "initialAudioGain")
            Lude.<*> (x Lude..:? "supportLowFramerateInputs")
            Lude.<*> (x Lude..:? "inputEndAction")
            Lude.<*> (x Lude..:? "outputTimingSource")
      )

instance Lude.ToJSON GlobalConfiguration where
  toJSON GlobalConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("outputLockingMode" Lude..=) Lude.<$> outputLockingMode,
            ("inputLossBehavior" Lude..=) Lude.<$> inputLossBehavior,
            ("initialAudioGain" Lude..=) Lude.<$> initialAudioGain,
            ("supportLowFramerateInputs" Lude..=)
              Lude.<$> supportLowFramerateInputs,
            ("inputEndAction" Lude..=) Lude.<$> inputEndAction,
            ("outputTimingSource" Lude..=) Lude.<$> outputTimingSource
          ]
      )
