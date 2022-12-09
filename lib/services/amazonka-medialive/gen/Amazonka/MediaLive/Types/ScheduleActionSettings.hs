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
-- Module      : Amazonka.MediaLive.Types.ScheduleActionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
import Amazonka.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Amazonka.MediaLive.Types.InputPrepareScheduleActionSettings
import Amazonka.MediaLive.Types.InputSwitchScheduleActionSettings
import Amazonka.MediaLive.Types.MotionGraphicsActivateScheduleActionSettings
import Amazonka.MediaLive.Types.MotionGraphicsDeactivateScheduleActionSettings
import Amazonka.MediaLive.Types.PauseStateScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35InputScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
import Amazonka.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
import Amazonka.MediaLive.Types.StaticImageActivateScheduleActionSettings
import Amazonka.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
import qualified Amazonka.Prelude as Prelude

-- | Holds the settings for a single schedule action.
--
-- /See:/ 'newScheduleActionSettings' smart constructor.
data ScheduleActionSettings = ScheduleActionSettings'
  { -- | Action to insert HLS ID3 segment tagging
    hlsId3SegmentTaggingSettings :: Prelude.Maybe HlsId3SegmentTaggingScheduleActionSettings,
    -- | Action to insert HLS metadata
    hlsTimedMetadataSettings :: Prelude.Maybe HlsTimedMetadataScheduleActionSettings,
    -- | Action to prepare an input for a future immediate input switch
    inputPrepareSettings :: Prelude.Maybe InputPrepareScheduleActionSettings,
    -- | Action to switch the input
    inputSwitchSettings :: Prelude.Maybe InputSwitchScheduleActionSettings,
    -- | Action to activate a motion graphics image overlay
    motionGraphicsImageActivateSettings :: Prelude.Maybe MotionGraphicsActivateScheduleActionSettings,
    -- | Action to deactivate a motion graphics image overlay
    motionGraphicsImageDeactivateSettings :: Prelude.Maybe MotionGraphicsDeactivateScheduleActionSettings,
    -- | Action to pause or unpause one or both channel pipelines
    pauseStateSettings :: Prelude.Maybe PauseStateScheduleActionSettings,
    -- | Action to specify scte35 input
    scte35InputSettings :: Prelude.Maybe Scte35InputScheduleActionSettings,
    -- | Action to insert SCTE-35 return_to_network message
    scte35ReturnToNetworkSettings :: Prelude.Maybe Scte35ReturnToNetworkScheduleActionSettings,
    -- | Action to insert SCTE-35 splice_insert message
    scte35SpliceInsertSettings :: Prelude.Maybe Scte35SpliceInsertScheduleActionSettings,
    -- | Action to insert SCTE-35 time_signal message
    scte35TimeSignalSettings :: Prelude.Maybe Scte35TimeSignalScheduleActionSettings,
    -- | Action to activate a static image overlay
    staticImageActivateSettings :: Prelude.Maybe StaticImageActivateScheduleActionSettings,
    -- | Action to deactivate a static image overlay
    staticImageDeactivateSettings :: Prelude.Maybe StaticImageDeactivateScheduleActionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsId3SegmentTaggingSettings', 'scheduleActionSettings_hlsId3SegmentTaggingSettings' - Action to insert HLS ID3 segment tagging
--
-- 'hlsTimedMetadataSettings', 'scheduleActionSettings_hlsTimedMetadataSettings' - Action to insert HLS metadata
--
-- 'inputPrepareSettings', 'scheduleActionSettings_inputPrepareSettings' - Action to prepare an input for a future immediate input switch
--
-- 'inputSwitchSettings', 'scheduleActionSettings_inputSwitchSettings' - Action to switch the input
--
-- 'motionGraphicsImageActivateSettings', 'scheduleActionSettings_motionGraphicsImageActivateSettings' - Action to activate a motion graphics image overlay
--
-- 'motionGraphicsImageDeactivateSettings', 'scheduleActionSettings_motionGraphicsImageDeactivateSettings' - Action to deactivate a motion graphics image overlay
--
-- 'pauseStateSettings', 'scheduleActionSettings_pauseStateSettings' - Action to pause or unpause one or both channel pipelines
--
-- 'scte35InputSettings', 'scheduleActionSettings_scte35InputSettings' - Action to specify scte35 input
--
-- 'scte35ReturnToNetworkSettings', 'scheduleActionSettings_scte35ReturnToNetworkSettings' - Action to insert SCTE-35 return_to_network message
--
-- 'scte35SpliceInsertSettings', 'scheduleActionSettings_scte35SpliceInsertSettings' - Action to insert SCTE-35 splice_insert message
--
-- 'scte35TimeSignalSettings', 'scheduleActionSettings_scte35TimeSignalSettings' - Action to insert SCTE-35 time_signal message
--
-- 'staticImageActivateSettings', 'scheduleActionSettings_staticImageActivateSettings' - Action to activate a static image overlay
--
-- 'staticImageDeactivateSettings', 'scheduleActionSettings_staticImageDeactivateSettings' - Action to deactivate a static image overlay
newScheduleActionSettings ::
  ScheduleActionSettings
newScheduleActionSettings =
  ScheduleActionSettings'
    { hlsId3SegmentTaggingSettings =
        Prelude.Nothing,
      hlsTimedMetadataSettings = Prelude.Nothing,
      inputPrepareSettings = Prelude.Nothing,
      inputSwitchSettings = Prelude.Nothing,
      motionGraphicsImageActivateSettings =
        Prelude.Nothing,
      motionGraphicsImageDeactivateSettings =
        Prelude.Nothing,
      pauseStateSettings = Prelude.Nothing,
      scte35InputSettings = Prelude.Nothing,
      scte35ReturnToNetworkSettings = Prelude.Nothing,
      scte35SpliceInsertSettings = Prelude.Nothing,
      scte35TimeSignalSettings = Prelude.Nothing,
      staticImageActivateSettings = Prelude.Nothing,
      staticImageDeactivateSettings = Prelude.Nothing
    }

-- | Action to insert HLS ID3 segment tagging
scheduleActionSettings_hlsId3SegmentTaggingSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe HlsId3SegmentTaggingScheduleActionSettings)
scheduleActionSettings_hlsId3SegmentTaggingSettings = Lens.lens (\ScheduleActionSettings' {hlsId3SegmentTaggingSettings} -> hlsId3SegmentTaggingSettings) (\s@ScheduleActionSettings' {} a -> s {hlsId3SegmentTaggingSettings = a} :: ScheduleActionSettings)

-- | Action to insert HLS metadata
scheduleActionSettings_hlsTimedMetadataSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe HlsTimedMetadataScheduleActionSettings)
scheduleActionSettings_hlsTimedMetadataSettings = Lens.lens (\ScheduleActionSettings' {hlsTimedMetadataSettings} -> hlsTimedMetadataSettings) (\s@ScheduleActionSettings' {} a -> s {hlsTimedMetadataSettings = a} :: ScheduleActionSettings)

-- | Action to prepare an input for a future immediate input switch
scheduleActionSettings_inputPrepareSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe InputPrepareScheduleActionSettings)
scheduleActionSettings_inputPrepareSettings = Lens.lens (\ScheduleActionSettings' {inputPrepareSettings} -> inputPrepareSettings) (\s@ScheduleActionSettings' {} a -> s {inputPrepareSettings = a} :: ScheduleActionSettings)

-- | Action to switch the input
scheduleActionSettings_inputSwitchSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe InputSwitchScheduleActionSettings)
scheduleActionSettings_inputSwitchSettings = Lens.lens (\ScheduleActionSettings' {inputSwitchSettings} -> inputSwitchSettings) (\s@ScheduleActionSettings' {} a -> s {inputSwitchSettings = a} :: ScheduleActionSettings)

-- | Action to activate a motion graphics image overlay
scheduleActionSettings_motionGraphicsImageActivateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe MotionGraphicsActivateScheduleActionSettings)
scheduleActionSettings_motionGraphicsImageActivateSettings = Lens.lens (\ScheduleActionSettings' {motionGraphicsImageActivateSettings} -> motionGraphicsImageActivateSettings) (\s@ScheduleActionSettings' {} a -> s {motionGraphicsImageActivateSettings = a} :: ScheduleActionSettings)

-- | Action to deactivate a motion graphics image overlay
scheduleActionSettings_motionGraphicsImageDeactivateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe MotionGraphicsDeactivateScheduleActionSettings)
scheduleActionSettings_motionGraphicsImageDeactivateSettings = Lens.lens (\ScheduleActionSettings' {motionGraphicsImageDeactivateSettings} -> motionGraphicsImageDeactivateSettings) (\s@ScheduleActionSettings' {} a -> s {motionGraphicsImageDeactivateSettings = a} :: ScheduleActionSettings)

-- | Action to pause or unpause one or both channel pipelines
scheduleActionSettings_pauseStateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe PauseStateScheduleActionSettings)
scheduleActionSettings_pauseStateSettings = Lens.lens (\ScheduleActionSettings' {pauseStateSettings} -> pauseStateSettings) (\s@ScheduleActionSettings' {} a -> s {pauseStateSettings = a} :: ScheduleActionSettings)

-- | Action to specify scte35 input
scheduleActionSettings_scte35InputSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe Scte35InputScheduleActionSettings)
scheduleActionSettings_scte35InputSettings = Lens.lens (\ScheduleActionSettings' {scte35InputSettings} -> scte35InputSettings) (\s@ScheduleActionSettings' {} a -> s {scte35InputSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 return_to_network message
scheduleActionSettings_scte35ReturnToNetworkSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe Scte35ReturnToNetworkScheduleActionSettings)
scheduleActionSettings_scte35ReturnToNetworkSettings = Lens.lens (\ScheduleActionSettings' {scte35ReturnToNetworkSettings} -> scte35ReturnToNetworkSettings) (\s@ScheduleActionSettings' {} a -> s {scte35ReturnToNetworkSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 splice_insert message
scheduleActionSettings_scte35SpliceInsertSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe Scte35SpliceInsertScheduleActionSettings)
scheduleActionSettings_scte35SpliceInsertSettings = Lens.lens (\ScheduleActionSettings' {scte35SpliceInsertSettings} -> scte35SpliceInsertSettings) (\s@ScheduleActionSettings' {} a -> s {scte35SpliceInsertSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 time_signal message
scheduleActionSettings_scte35TimeSignalSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe Scte35TimeSignalScheduleActionSettings)
scheduleActionSettings_scte35TimeSignalSettings = Lens.lens (\ScheduleActionSettings' {scte35TimeSignalSettings} -> scte35TimeSignalSettings) (\s@ScheduleActionSettings' {} a -> s {scte35TimeSignalSettings = a} :: ScheduleActionSettings)

-- | Action to activate a static image overlay
scheduleActionSettings_staticImageActivateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe StaticImageActivateScheduleActionSettings)
scheduleActionSettings_staticImageActivateSettings = Lens.lens (\ScheduleActionSettings' {staticImageActivateSettings} -> staticImageActivateSettings) (\s@ScheduleActionSettings' {} a -> s {staticImageActivateSettings = a} :: ScheduleActionSettings)

-- | Action to deactivate a static image overlay
scheduleActionSettings_staticImageDeactivateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe StaticImageDeactivateScheduleActionSettings)
scheduleActionSettings_staticImageDeactivateSettings = Lens.lens (\ScheduleActionSettings' {staticImageDeactivateSettings} -> staticImageDeactivateSettings) (\s@ScheduleActionSettings' {} a -> s {staticImageDeactivateSettings = a} :: ScheduleActionSettings)

instance Data.FromJSON ScheduleActionSettings where
  parseJSON =
    Data.withObject
      "ScheduleActionSettings"
      ( \x ->
          ScheduleActionSettings'
            Prelude.<$> (x Data..:? "hlsId3SegmentTaggingSettings")
            Prelude.<*> (x Data..:? "hlsTimedMetadataSettings")
            Prelude.<*> (x Data..:? "inputPrepareSettings")
            Prelude.<*> (x Data..:? "inputSwitchSettings")
            Prelude.<*> (x Data..:? "motionGraphicsImageActivateSettings")
            Prelude.<*> (x Data..:? "motionGraphicsImageDeactivateSettings")
            Prelude.<*> (x Data..:? "pauseStateSettings")
            Prelude.<*> (x Data..:? "scte35InputSettings")
            Prelude.<*> (x Data..:? "scte35ReturnToNetworkSettings")
            Prelude.<*> (x Data..:? "scte35SpliceInsertSettings")
            Prelude.<*> (x Data..:? "scte35TimeSignalSettings")
            Prelude.<*> (x Data..:? "staticImageActivateSettings")
            Prelude.<*> (x Data..:? "staticImageDeactivateSettings")
      )

instance Prelude.Hashable ScheduleActionSettings where
  hashWithSalt _salt ScheduleActionSettings' {..} =
    _salt
      `Prelude.hashWithSalt` hlsId3SegmentTaggingSettings
      `Prelude.hashWithSalt` hlsTimedMetadataSettings
      `Prelude.hashWithSalt` inputPrepareSettings
      `Prelude.hashWithSalt` inputSwitchSettings
      `Prelude.hashWithSalt` motionGraphicsImageActivateSettings
      `Prelude.hashWithSalt` motionGraphicsImageDeactivateSettings
      `Prelude.hashWithSalt` pauseStateSettings
      `Prelude.hashWithSalt` scte35InputSettings
      `Prelude.hashWithSalt` scte35ReturnToNetworkSettings
      `Prelude.hashWithSalt` scte35SpliceInsertSettings
      `Prelude.hashWithSalt` scte35TimeSignalSettings
      `Prelude.hashWithSalt` staticImageActivateSettings
      `Prelude.hashWithSalt` staticImageDeactivateSettings

instance Prelude.NFData ScheduleActionSettings where
  rnf ScheduleActionSettings' {..} =
    Prelude.rnf hlsId3SegmentTaggingSettings
      `Prelude.seq` Prelude.rnf hlsTimedMetadataSettings
      `Prelude.seq` Prelude.rnf inputPrepareSettings
      `Prelude.seq` Prelude.rnf inputSwitchSettings
      `Prelude.seq` Prelude.rnf motionGraphicsImageActivateSettings
      `Prelude.seq` Prelude.rnf motionGraphicsImageDeactivateSettings
      `Prelude.seq` Prelude.rnf pauseStateSettings
      `Prelude.seq` Prelude.rnf scte35InputSettings
      `Prelude.seq` Prelude.rnf scte35ReturnToNetworkSettings
      `Prelude.seq` Prelude.rnf scte35SpliceInsertSettings
      `Prelude.seq` Prelude.rnf scte35TimeSignalSettings
      `Prelude.seq` Prelude.rnf staticImageActivateSettings
      `Prelude.seq` Prelude.rnf staticImageDeactivateSettings

instance Data.ToJSON ScheduleActionSettings where
  toJSON ScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hlsId3SegmentTaggingSettings" Data..=)
              Prelude.<$> hlsId3SegmentTaggingSettings,
            ("hlsTimedMetadataSettings" Data..=)
              Prelude.<$> hlsTimedMetadataSettings,
            ("inputPrepareSettings" Data..=)
              Prelude.<$> inputPrepareSettings,
            ("inputSwitchSettings" Data..=)
              Prelude.<$> inputSwitchSettings,
            ("motionGraphicsImageActivateSettings" Data..=)
              Prelude.<$> motionGraphicsImageActivateSettings,
            ("motionGraphicsImageDeactivateSettings" Data..=)
              Prelude.<$> motionGraphicsImageDeactivateSettings,
            ("pauseStateSettings" Data..=)
              Prelude.<$> pauseStateSettings,
            ("scte35InputSettings" Data..=)
              Prelude.<$> scte35InputSettings,
            ("scte35ReturnToNetworkSettings" Data..=)
              Prelude.<$> scte35ReturnToNetworkSettings,
            ("scte35SpliceInsertSettings" Data..=)
              Prelude.<$> scte35SpliceInsertSettings,
            ("scte35TimeSignalSettings" Data..=)
              Prelude.<$> scte35TimeSignalSettings,
            ("staticImageActivateSettings" Data..=)
              Prelude.<$> staticImageActivateSettings,
            ("staticImageDeactivateSettings" Data..=)
              Prelude.<$> staticImageDeactivateSettings
          ]
      )
