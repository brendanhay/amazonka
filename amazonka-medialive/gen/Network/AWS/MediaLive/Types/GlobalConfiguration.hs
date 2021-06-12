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
-- Module      : Network.AWS.MediaLive.Types.GlobalConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
import Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputLockingMode
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
import Network.AWS.MediaLive.Types.InputLossBehavior

-- | Global Configuration
--
-- /See:/ 'newGlobalConfiguration' smart constructor.
data GlobalConfiguration = GlobalConfiguration'
  { -- | Value to set the initial audio gain for the Live Event.
    initialAudioGain :: Core.Maybe Core.Int,
    -- | Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
    -- MediaLive will attempt to synchronize the output of each pipeline to the
    -- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
    -- of each pipeline to the Unix epoch.
    outputLockingMode :: Core.Maybe GlobalConfigurationOutputLockingMode,
    -- | Indicates the action to take when the current input completes (e.g.
    -- end-of-file). When switchAndLoopInputs is configured the encoder will
    -- restart at the beginning of the first input. When \"none\" is configured
    -- the encoder will transcode either black, a solid color, or a user
    -- specified slate images per the \"Input Loss Behavior\" configuration
    -- until the next input switch occurs (which is controlled through the
    -- Channel Schedule API).
    inputEndAction :: Core.Maybe GlobalConfigurationInputEndAction,
    -- | Settings for system actions when input is lost.
    inputLossBehavior :: Core.Maybe InputLossBehavior,
    -- | Adjusts video input buffer for streams with very low video framerates.
    -- This is commonly set to enabled for music channels with less than one
    -- video frame per second.
    supportLowFramerateInputs :: Core.Maybe GlobalConfigurationLowFramerateInputs,
    -- | Indicates whether the rate of frames emitted by the Live encoder should
    -- be paced by its system clock (which optionally may be locked to another
    -- source via NTP) or should be locked to the clock of the source that is
    -- providing the input stream.
    outputTimingSource :: Core.Maybe GlobalConfigurationOutputTimingSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GlobalConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialAudioGain', 'globalConfiguration_initialAudioGain' - Value to set the initial audio gain for the Live Event.
--
-- 'outputLockingMode', 'globalConfiguration_outputLockingMode' - Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
-- MediaLive will attempt to synchronize the output of each pipeline to the
-- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
-- of each pipeline to the Unix epoch.
--
-- 'inputEndAction', 'globalConfiguration_inputEndAction' - Indicates the action to take when the current input completes (e.g.
-- end-of-file). When switchAndLoopInputs is configured the encoder will
-- restart at the beginning of the first input. When \"none\" is configured
-- the encoder will transcode either black, a solid color, or a user
-- specified slate images per the \"Input Loss Behavior\" configuration
-- until the next input switch occurs (which is controlled through the
-- Channel Schedule API).
--
-- 'inputLossBehavior', 'globalConfiguration_inputLossBehavior' - Settings for system actions when input is lost.
--
-- 'supportLowFramerateInputs', 'globalConfiguration_supportLowFramerateInputs' - Adjusts video input buffer for streams with very low video framerates.
-- This is commonly set to enabled for music channels with less than one
-- video frame per second.
--
-- 'outputTimingSource', 'globalConfiguration_outputTimingSource' - Indicates whether the rate of frames emitted by the Live encoder should
-- be paced by its system clock (which optionally may be locked to another
-- source via NTP) or should be locked to the clock of the source that is
-- providing the input stream.
newGlobalConfiguration ::
  GlobalConfiguration
newGlobalConfiguration =
  GlobalConfiguration'
    { initialAudioGain =
        Core.Nothing,
      outputLockingMode = Core.Nothing,
      inputEndAction = Core.Nothing,
      inputLossBehavior = Core.Nothing,
      supportLowFramerateInputs = Core.Nothing,
      outputTimingSource = Core.Nothing
    }

-- | Value to set the initial audio gain for the Live Event.
globalConfiguration_initialAudioGain :: Lens.Lens' GlobalConfiguration (Core.Maybe Core.Int)
globalConfiguration_initialAudioGain = Lens.lens (\GlobalConfiguration' {initialAudioGain} -> initialAudioGain) (\s@GlobalConfiguration' {} a -> s {initialAudioGain = a} :: GlobalConfiguration)

-- | Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
-- MediaLive will attempt to synchronize the output of each pipeline to the
-- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
-- of each pipeline to the Unix epoch.
globalConfiguration_outputLockingMode :: Lens.Lens' GlobalConfiguration (Core.Maybe GlobalConfigurationOutputLockingMode)
globalConfiguration_outputLockingMode = Lens.lens (\GlobalConfiguration' {outputLockingMode} -> outputLockingMode) (\s@GlobalConfiguration' {} a -> s {outputLockingMode = a} :: GlobalConfiguration)

-- | Indicates the action to take when the current input completes (e.g.
-- end-of-file). When switchAndLoopInputs is configured the encoder will
-- restart at the beginning of the first input. When \"none\" is configured
-- the encoder will transcode either black, a solid color, or a user
-- specified slate images per the \"Input Loss Behavior\" configuration
-- until the next input switch occurs (which is controlled through the
-- Channel Schedule API).
globalConfiguration_inputEndAction :: Lens.Lens' GlobalConfiguration (Core.Maybe GlobalConfigurationInputEndAction)
globalConfiguration_inputEndAction = Lens.lens (\GlobalConfiguration' {inputEndAction} -> inputEndAction) (\s@GlobalConfiguration' {} a -> s {inputEndAction = a} :: GlobalConfiguration)

-- | Settings for system actions when input is lost.
globalConfiguration_inputLossBehavior :: Lens.Lens' GlobalConfiguration (Core.Maybe InputLossBehavior)
globalConfiguration_inputLossBehavior = Lens.lens (\GlobalConfiguration' {inputLossBehavior} -> inputLossBehavior) (\s@GlobalConfiguration' {} a -> s {inputLossBehavior = a} :: GlobalConfiguration)

-- | Adjusts video input buffer for streams with very low video framerates.
-- This is commonly set to enabled for music channels with less than one
-- video frame per second.
globalConfiguration_supportLowFramerateInputs :: Lens.Lens' GlobalConfiguration (Core.Maybe GlobalConfigurationLowFramerateInputs)
globalConfiguration_supportLowFramerateInputs = Lens.lens (\GlobalConfiguration' {supportLowFramerateInputs} -> supportLowFramerateInputs) (\s@GlobalConfiguration' {} a -> s {supportLowFramerateInputs = a} :: GlobalConfiguration)

-- | Indicates whether the rate of frames emitted by the Live encoder should
-- be paced by its system clock (which optionally may be locked to another
-- source via NTP) or should be locked to the clock of the source that is
-- providing the input stream.
globalConfiguration_outputTimingSource :: Lens.Lens' GlobalConfiguration (Core.Maybe GlobalConfigurationOutputTimingSource)
globalConfiguration_outputTimingSource = Lens.lens (\GlobalConfiguration' {outputTimingSource} -> outputTimingSource) (\s@GlobalConfiguration' {} a -> s {outputTimingSource = a} :: GlobalConfiguration)

instance Core.FromJSON GlobalConfiguration where
  parseJSON =
    Core.withObject
      "GlobalConfiguration"
      ( \x ->
          GlobalConfiguration'
            Core.<$> (x Core..:? "initialAudioGain")
            Core.<*> (x Core..:? "outputLockingMode")
            Core.<*> (x Core..:? "inputEndAction")
            Core.<*> (x Core..:? "inputLossBehavior")
            Core.<*> (x Core..:? "supportLowFramerateInputs")
            Core.<*> (x Core..:? "outputTimingSource")
      )

instance Core.Hashable GlobalConfiguration

instance Core.NFData GlobalConfiguration

instance Core.ToJSON GlobalConfiguration where
  toJSON GlobalConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("initialAudioGain" Core..=)
              Core.<$> initialAudioGain,
            ("outputLockingMode" Core..=)
              Core.<$> outputLockingMode,
            ("inputEndAction" Core..=) Core.<$> inputEndAction,
            ("inputLossBehavior" Core..=)
              Core.<$> inputLossBehavior,
            ("supportLowFramerateInputs" Core..=)
              Core.<$> supportLowFramerateInputs,
            ("outputTimingSource" Core..=)
              Core.<$> outputTimingSource
          ]
      )
