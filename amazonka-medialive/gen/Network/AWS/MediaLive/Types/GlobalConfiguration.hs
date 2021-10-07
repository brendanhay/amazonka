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
import qualified Network.AWS.Prelude as Prelude

-- | Global Configuration
--
-- /See:/ 'newGlobalConfiguration' smart constructor.
data GlobalConfiguration = GlobalConfiguration'
  { -- | Value to set the initial audio gain for the Live Event.
    initialAudioGain :: Prelude.Maybe Prelude.Int,
    -- | Indicates the action to take when the current input completes (e.g.
    -- end-of-file). When switchAndLoopInputs is configured the encoder will
    -- restart at the beginning of the first input. When \"none\" is configured
    -- the encoder will transcode either black, a solid color, or a user
    -- specified slate images per the \"Input Loss Behavior\" configuration
    -- until the next input switch occurs (which is controlled through the
    -- Channel Schedule API).
    inputEndAction :: Prelude.Maybe GlobalConfigurationInputEndAction,
    -- | Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
    -- MediaLive will attempt to synchronize the output of each pipeline to the
    -- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
    -- of each pipeline to the Unix epoch.
    outputLockingMode :: Prelude.Maybe GlobalConfigurationOutputLockingMode,
    -- | Adjusts video input buffer for streams with very low video framerates.
    -- This is commonly set to enabled for music channels with less than one
    -- video frame per second.
    supportLowFramerateInputs :: Prelude.Maybe GlobalConfigurationLowFramerateInputs,
    -- | Settings for system actions when input is lost.
    inputLossBehavior :: Prelude.Maybe InputLossBehavior,
    -- | Indicates whether the rate of frames emitted by the Live encoder should
    -- be paced by its system clock (which optionally may be locked to another
    -- source via NTP) or should be locked to the clock of the source that is
    -- providing the input stream.
    outputTimingSource :: Prelude.Maybe GlobalConfigurationOutputTimingSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'inputEndAction', 'globalConfiguration_inputEndAction' - Indicates the action to take when the current input completes (e.g.
-- end-of-file). When switchAndLoopInputs is configured the encoder will
-- restart at the beginning of the first input. When \"none\" is configured
-- the encoder will transcode either black, a solid color, or a user
-- specified slate images per the \"Input Loss Behavior\" configuration
-- until the next input switch occurs (which is controlled through the
-- Channel Schedule API).
--
-- 'outputLockingMode', 'globalConfiguration_outputLockingMode' - Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
-- MediaLive will attempt to synchronize the output of each pipeline to the
-- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
-- of each pipeline to the Unix epoch.
--
-- 'supportLowFramerateInputs', 'globalConfiguration_supportLowFramerateInputs' - Adjusts video input buffer for streams with very low video framerates.
-- This is commonly set to enabled for music channels with less than one
-- video frame per second.
--
-- 'inputLossBehavior', 'globalConfiguration_inputLossBehavior' - Settings for system actions when input is lost.
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
        Prelude.Nothing,
      inputEndAction = Prelude.Nothing,
      outputLockingMode = Prelude.Nothing,
      supportLowFramerateInputs = Prelude.Nothing,
      inputLossBehavior = Prelude.Nothing,
      outputTimingSource = Prelude.Nothing
    }

-- | Value to set the initial audio gain for the Live Event.
globalConfiguration_initialAudioGain :: Lens.Lens' GlobalConfiguration (Prelude.Maybe Prelude.Int)
globalConfiguration_initialAudioGain = Lens.lens (\GlobalConfiguration' {initialAudioGain} -> initialAudioGain) (\s@GlobalConfiguration' {} a -> s {initialAudioGain = a} :: GlobalConfiguration)

-- | Indicates the action to take when the current input completes (e.g.
-- end-of-file). When switchAndLoopInputs is configured the encoder will
-- restart at the beginning of the first input. When \"none\" is configured
-- the encoder will transcode either black, a solid color, or a user
-- specified slate images per the \"Input Loss Behavior\" configuration
-- until the next input switch occurs (which is controlled through the
-- Channel Schedule API).
globalConfiguration_inputEndAction :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationInputEndAction)
globalConfiguration_inputEndAction = Lens.lens (\GlobalConfiguration' {inputEndAction} -> inputEndAction) (\s@GlobalConfiguration' {} a -> s {inputEndAction = a} :: GlobalConfiguration)

-- | Indicates how MediaLive pipelines are synchronized. PIPELINE_LOCKING -
-- MediaLive will attempt to synchronize the output of each pipeline to the
-- other. EPOCH_LOCKING - MediaLive will attempt to synchronize the output
-- of each pipeline to the Unix epoch.
globalConfiguration_outputLockingMode :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationOutputLockingMode)
globalConfiguration_outputLockingMode = Lens.lens (\GlobalConfiguration' {outputLockingMode} -> outputLockingMode) (\s@GlobalConfiguration' {} a -> s {outputLockingMode = a} :: GlobalConfiguration)

-- | Adjusts video input buffer for streams with very low video framerates.
-- This is commonly set to enabled for music channels with less than one
-- video frame per second.
globalConfiguration_supportLowFramerateInputs :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationLowFramerateInputs)
globalConfiguration_supportLowFramerateInputs = Lens.lens (\GlobalConfiguration' {supportLowFramerateInputs} -> supportLowFramerateInputs) (\s@GlobalConfiguration' {} a -> s {supportLowFramerateInputs = a} :: GlobalConfiguration)

-- | Settings for system actions when input is lost.
globalConfiguration_inputLossBehavior :: Lens.Lens' GlobalConfiguration (Prelude.Maybe InputLossBehavior)
globalConfiguration_inputLossBehavior = Lens.lens (\GlobalConfiguration' {inputLossBehavior} -> inputLossBehavior) (\s@GlobalConfiguration' {} a -> s {inputLossBehavior = a} :: GlobalConfiguration)

-- | Indicates whether the rate of frames emitted by the Live encoder should
-- be paced by its system clock (which optionally may be locked to another
-- source via NTP) or should be locked to the clock of the source that is
-- providing the input stream.
globalConfiguration_outputTimingSource :: Lens.Lens' GlobalConfiguration (Prelude.Maybe GlobalConfigurationOutputTimingSource)
globalConfiguration_outputTimingSource = Lens.lens (\GlobalConfiguration' {outputTimingSource} -> outputTimingSource) (\s@GlobalConfiguration' {} a -> s {outputTimingSource = a} :: GlobalConfiguration)

instance Core.FromJSON GlobalConfiguration where
  parseJSON =
    Core.withObject
      "GlobalConfiguration"
      ( \x ->
          GlobalConfiguration'
            Prelude.<$> (x Core..:? "initialAudioGain")
            Prelude.<*> (x Core..:? "inputEndAction")
            Prelude.<*> (x Core..:? "outputLockingMode")
            Prelude.<*> (x Core..:? "supportLowFramerateInputs")
            Prelude.<*> (x Core..:? "inputLossBehavior")
            Prelude.<*> (x Core..:? "outputTimingSource")
      )

instance Prelude.Hashable GlobalConfiguration

instance Prelude.NFData GlobalConfiguration

instance Core.ToJSON GlobalConfiguration where
  toJSON GlobalConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("initialAudioGain" Core..=)
              Prelude.<$> initialAudioGain,
            ("inputEndAction" Core..=)
              Prelude.<$> inputEndAction,
            ("outputLockingMode" Core..=)
              Prelude.<$> outputLockingMode,
            ("supportLowFramerateInputs" Core..=)
              Prelude.<$> supportLowFramerateInputs,
            ("inputLossBehavior" Core..=)
              Prelude.<$> inputLossBehavior,
            ("outputTimingSource" Core..=)
              Prelude.<$> outputTimingSource
          ]
      )
