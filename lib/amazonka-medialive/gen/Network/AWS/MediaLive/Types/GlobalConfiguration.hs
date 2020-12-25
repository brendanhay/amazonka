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
    gcInitialAudioGain,
    gcInputEndAction,
    gcInputLossBehavior,
    gcOutputLockingMode,
    gcOutputTimingSource,
    gcSupportLowFramerateInputs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction as Types
import qualified Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs as Types
import qualified Network.AWS.MediaLive.Types.GlobalConfigurationOutputLockingMode as Types
import qualified Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource as Types
import qualified Network.AWS.MediaLive.Types.InputLossBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Global Configuration
--
-- /See:/ 'mkGlobalConfiguration' smart constructor.
data GlobalConfiguration = GlobalConfiguration'
  { -- | Value to set the initial audio gain for the Live Event.
    initialAudioGain :: Core.Maybe Core.Int,
    -- | Indicates the action to take when the current input completes (e.g. end-of-file). When switchAndLoopInputs is configured the encoder will restart at the beginning of the first input.  When "none" is configured the encoder will transcode either black, a solid color, or a user specified slate images per the "Input Loss Behavior" configuration until the next input switch occurs (which is controlled through the Channel Schedule API).
    inputEndAction :: Core.Maybe Types.GlobalConfigurationInputEndAction,
    -- | Settings for system actions when input is lost.
    inputLossBehavior :: Core.Maybe Types.InputLossBehavior,
    -- | Indicates how MediaLive pipelines are synchronized.
    --
    --
    -- PIPELINE_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the other.
    -- EPOCH_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the Unix epoch.
    outputLockingMode :: Core.Maybe Types.GlobalConfigurationOutputLockingMode,
    -- | Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
    outputTimingSource :: Core.Maybe Types.GlobalConfigurationOutputTimingSource,
    -- | Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
    supportLowFramerateInputs :: Core.Maybe Types.GlobalConfigurationLowFramerateInputs
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalConfiguration' value with any optional fields omitted.
mkGlobalConfiguration ::
  GlobalConfiguration
mkGlobalConfiguration =
  GlobalConfiguration'
    { initialAudioGain = Core.Nothing,
      inputEndAction = Core.Nothing,
      inputLossBehavior = Core.Nothing,
      outputLockingMode = Core.Nothing,
      outputTimingSource = Core.Nothing,
      supportLowFramerateInputs = Core.Nothing
    }

-- | Value to set the initial audio gain for the Live Event.
--
-- /Note:/ Consider using 'initialAudioGain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcInitialAudioGain :: Lens.Lens' GlobalConfiguration (Core.Maybe Core.Int)
gcInitialAudioGain = Lens.field @"initialAudioGain"
{-# DEPRECATED gcInitialAudioGain "Use generic-lens or generic-optics with 'initialAudioGain' instead." #-}

-- | Indicates the action to take when the current input completes (e.g. end-of-file). When switchAndLoopInputs is configured the encoder will restart at the beginning of the first input.  When "none" is configured the encoder will transcode either black, a solid color, or a user specified slate images per the "Input Loss Behavior" configuration until the next input switch occurs (which is controlled through the Channel Schedule API).
--
-- /Note:/ Consider using 'inputEndAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcInputEndAction :: Lens.Lens' GlobalConfiguration (Core.Maybe Types.GlobalConfigurationInputEndAction)
gcInputEndAction = Lens.field @"inputEndAction"
{-# DEPRECATED gcInputEndAction "Use generic-lens or generic-optics with 'inputEndAction' instead." #-}

-- | Settings for system actions when input is lost.
--
-- /Note:/ Consider using 'inputLossBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcInputLossBehavior :: Lens.Lens' GlobalConfiguration (Core.Maybe Types.InputLossBehavior)
gcInputLossBehavior = Lens.field @"inputLossBehavior"
{-# DEPRECATED gcInputLossBehavior "Use generic-lens or generic-optics with 'inputLossBehavior' instead." #-}

-- | Indicates how MediaLive pipelines are synchronized.
--
--
-- PIPELINE_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the other.
-- EPOCH_LOCKING - MediaLive will attempt to synchronize the output of each pipeline to the Unix epoch.
--
-- /Note:/ Consider using 'outputLockingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcOutputLockingMode :: Lens.Lens' GlobalConfiguration (Core.Maybe Types.GlobalConfigurationOutputLockingMode)
gcOutputLockingMode = Lens.field @"outputLockingMode"
{-# DEPRECATED gcOutputLockingMode "Use generic-lens or generic-optics with 'outputLockingMode' instead." #-}

-- | Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
--
-- /Note:/ Consider using 'outputTimingSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcOutputTimingSource :: Lens.Lens' GlobalConfiguration (Core.Maybe Types.GlobalConfigurationOutputTimingSource)
gcOutputTimingSource = Lens.field @"outputTimingSource"
{-# DEPRECATED gcOutputTimingSource "Use generic-lens or generic-optics with 'outputTimingSource' instead." #-}

-- | Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
--
-- /Note:/ Consider using 'supportLowFramerateInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcSupportLowFramerateInputs :: Lens.Lens' GlobalConfiguration (Core.Maybe Types.GlobalConfigurationLowFramerateInputs)
gcSupportLowFramerateInputs = Lens.field @"supportLowFramerateInputs"
{-# DEPRECATED gcSupportLowFramerateInputs "Use generic-lens or generic-optics with 'supportLowFramerateInputs' instead." #-}

instance Core.FromJSON GlobalConfiguration where
  toJSON GlobalConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("initialAudioGain" Core..=) Core.<$> initialAudioGain,
            ("inputEndAction" Core..=) Core.<$> inputEndAction,
            ("inputLossBehavior" Core..=) Core.<$> inputLossBehavior,
            ("outputLockingMode" Core..=) Core.<$> outputLockingMode,
            ("outputTimingSource" Core..=) Core.<$> outputTimingSource,
            ("supportLowFramerateInputs" Core..=)
              Core.<$> supportLowFramerateInputs
          ]
      )

instance Core.FromJSON GlobalConfiguration where
  parseJSON =
    Core.withObject "GlobalConfiguration" Core.$
      \x ->
        GlobalConfiguration'
          Core.<$> (x Core..:? "initialAudioGain")
          Core.<*> (x Core..:? "inputEndAction")
          Core.<*> (x Core..:? "inputLossBehavior")
          Core.<*> (x Core..:? "outputLockingMode")
          Core.<*> (x Core..:? "outputTimingSource")
          Core.<*> (x Core..:? "supportLowFramerateInputs")
