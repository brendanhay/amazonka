{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
import Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
import Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
import Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
import qualified Network.AWS.Prelude as Prelude

-- | Holds the settings for a single schedule action.
--
-- /See:/ 'newScheduleActionSettings' smart constructor.
data ScheduleActionSettings = ScheduleActionSettings'
  { -- | Action to switch the input
    inputSwitchSettings :: Prelude.Maybe InputSwitchScheduleActionSettings,
    -- | Action to insert SCTE-35 time_signal message
    scte35TimeSignalSettings :: Prelude.Maybe Scte35TimeSignalScheduleActionSettings,
    -- | Action to insert HLS metadata
    hlsTimedMetadataSettings :: Prelude.Maybe HlsTimedMetadataScheduleActionSettings,
    -- | Action to activate a static image overlay
    staticImageActivateSettings :: Prelude.Maybe StaticImageActivateScheduleActionSettings,
    -- | Action to pause or unpause one or both channel pipelines
    pauseStateSettings :: Prelude.Maybe PauseStateScheduleActionSettings,
    -- | Action to insert SCTE-35 splice_insert message
    scte35SpliceInsertSettings :: Prelude.Maybe Scte35SpliceInsertScheduleActionSettings,
    -- | Action to insert SCTE-35 return_to_network message
    scte35ReturnToNetworkSettings :: Prelude.Maybe Scte35ReturnToNetworkScheduleActionSettings,
    -- | Action to insert HLS ID3 segment tagging
    hlsId3SegmentTaggingSettings :: Prelude.Maybe HlsId3SegmentTaggingScheduleActionSettings,
    -- | Action to deactivate a static image overlay
    staticImageDeactivateSettings :: Prelude.Maybe StaticImageDeactivateScheduleActionSettings,
    -- | Action to prepare an input for a future immediate input switch
    inputPrepareSettings :: Prelude.Maybe InputPrepareScheduleActionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSwitchSettings', 'scheduleActionSettings_inputSwitchSettings' - Action to switch the input
--
-- 'scte35TimeSignalSettings', 'scheduleActionSettings_scte35TimeSignalSettings' - Action to insert SCTE-35 time_signal message
--
-- 'hlsTimedMetadataSettings', 'scheduleActionSettings_hlsTimedMetadataSettings' - Action to insert HLS metadata
--
-- 'staticImageActivateSettings', 'scheduleActionSettings_staticImageActivateSettings' - Action to activate a static image overlay
--
-- 'pauseStateSettings', 'scheduleActionSettings_pauseStateSettings' - Action to pause or unpause one or both channel pipelines
--
-- 'scte35SpliceInsertSettings', 'scheduleActionSettings_scte35SpliceInsertSettings' - Action to insert SCTE-35 splice_insert message
--
-- 'scte35ReturnToNetworkSettings', 'scheduleActionSettings_scte35ReturnToNetworkSettings' - Action to insert SCTE-35 return_to_network message
--
-- 'hlsId3SegmentTaggingSettings', 'scheduleActionSettings_hlsId3SegmentTaggingSettings' - Action to insert HLS ID3 segment tagging
--
-- 'staticImageDeactivateSettings', 'scheduleActionSettings_staticImageDeactivateSettings' - Action to deactivate a static image overlay
--
-- 'inputPrepareSettings', 'scheduleActionSettings_inputPrepareSettings' - Action to prepare an input for a future immediate input switch
newScheduleActionSettings ::
  ScheduleActionSettings
newScheduleActionSettings =
  ScheduleActionSettings'
    { inputSwitchSettings =
        Prelude.Nothing,
      scte35TimeSignalSettings = Prelude.Nothing,
      hlsTimedMetadataSettings = Prelude.Nothing,
      staticImageActivateSettings = Prelude.Nothing,
      pauseStateSettings = Prelude.Nothing,
      scte35SpliceInsertSettings = Prelude.Nothing,
      scte35ReturnToNetworkSettings = Prelude.Nothing,
      hlsId3SegmentTaggingSettings = Prelude.Nothing,
      staticImageDeactivateSettings = Prelude.Nothing,
      inputPrepareSettings = Prelude.Nothing
    }

-- | Action to switch the input
scheduleActionSettings_inputSwitchSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe InputSwitchScheduleActionSettings)
scheduleActionSettings_inputSwitchSettings = Lens.lens (\ScheduleActionSettings' {inputSwitchSettings} -> inputSwitchSettings) (\s@ScheduleActionSettings' {} a -> s {inputSwitchSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 time_signal message
scheduleActionSettings_scte35TimeSignalSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe Scte35TimeSignalScheduleActionSettings)
scheduleActionSettings_scte35TimeSignalSettings = Lens.lens (\ScheduleActionSettings' {scte35TimeSignalSettings} -> scte35TimeSignalSettings) (\s@ScheduleActionSettings' {} a -> s {scte35TimeSignalSettings = a} :: ScheduleActionSettings)

-- | Action to insert HLS metadata
scheduleActionSettings_hlsTimedMetadataSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe HlsTimedMetadataScheduleActionSettings)
scheduleActionSettings_hlsTimedMetadataSettings = Lens.lens (\ScheduleActionSettings' {hlsTimedMetadataSettings} -> hlsTimedMetadataSettings) (\s@ScheduleActionSettings' {} a -> s {hlsTimedMetadataSettings = a} :: ScheduleActionSettings)

-- | Action to activate a static image overlay
scheduleActionSettings_staticImageActivateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe StaticImageActivateScheduleActionSettings)
scheduleActionSettings_staticImageActivateSettings = Lens.lens (\ScheduleActionSettings' {staticImageActivateSettings} -> staticImageActivateSettings) (\s@ScheduleActionSettings' {} a -> s {staticImageActivateSettings = a} :: ScheduleActionSettings)

-- | Action to pause or unpause one or both channel pipelines
scheduleActionSettings_pauseStateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe PauseStateScheduleActionSettings)
scheduleActionSettings_pauseStateSettings = Lens.lens (\ScheduleActionSettings' {pauseStateSettings} -> pauseStateSettings) (\s@ScheduleActionSettings' {} a -> s {pauseStateSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 splice_insert message
scheduleActionSettings_scte35SpliceInsertSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe Scte35SpliceInsertScheduleActionSettings)
scheduleActionSettings_scte35SpliceInsertSettings = Lens.lens (\ScheduleActionSettings' {scte35SpliceInsertSettings} -> scte35SpliceInsertSettings) (\s@ScheduleActionSettings' {} a -> s {scte35SpliceInsertSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 return_to_network message
scheduleActionSettings_scte35ReturnToNetworkSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe Scte35ReturnToNetworkScheduleActionSettings)
scheduleActionSettings_scte35ReturnToNetworkSettings = Lens.lens (\ScheduleActionSettings' {scte35ReturnToNetworkSettings} -> scte35ReturnToNetworkSettings) (\s@ScheduleActionSettings' {} a -> s {scte35ReturnToNetworkSettings = a} :: ScheduleActionSettings)

-- | Action to insert HLS ID3 segment tagging
scheduleActionSettings_hlsId3SegmentTaggingSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe HlsId3SegmentTaggingScheduleActionSettings)
scheduleActionSettings_hlsId3SegmentTaggingSettings = Lens.lens (\ScheduleActionSettings' {hlsId3SegmentTaggingSettings} -> hlsId3SegmentTaggingSettings) (\s@ScheduleActionSettings' {} a -> s {hlsId3SegmentTaggingSettings = a} :: ScheduleActionSettings)

-- | Action to deactivate a static image overlay
scheduleActionSettings_staticImageDeactivateSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe StaticImageDeactivateScheduleActionSettings)
scheduleActionSettings_staticImageDeactivateSettings = Lens.lens (\ScheduleActionSettings' {staticImageDeactivateSettings} -> staticImageDeactivateSettings) (\s@ScheduleActionSettings' {} a -> s {staticImageDeactivateSettings = a} :: ScheduleActionSettings)

-- | Action to prepare an input for a future immediate input switch
scheduleActionSettings_inputPrepareSettings :: Lens.Lens' ScheduleActionSettings (Prelude.Maybe InputPrepareScheduleActionSettings)
scheduleActionSettings_inputPrepareSettings = Lens.lens (\ScheduleActionSettings' {inputPrepareSettings} -> inputPrepareSettings) (\s@ScheduleActionSettings' {} a -> s {inputPrepareSettings = a} :: ScheduleActionSettings)

instance Prelude.FromJSON ScheduleActionSettings where
  parseJSON =
    Prelude.withObject
      "ScheduleActionSettings"
      ( \x ->
          ScheduleActionSettings'
            Prelude.<$> (x Prelude..:? "inputSwitchSettings")
            Prelude.<*> (x Prelude..:? "scte35TimeSignalSettings")
            Prelude.<*> (x Prelude..:? "hlsTimedMetadataSettings")
            Prelude.<*> (x Prelude..:? "staticImageActivateSettings")
            Prelude.<*> (x Prelude..:? "pauseStateSettings")
            Prelude.<*> (x Prelude..:? "scte35SpliceInsertSettings")
            Prelude.<*> (x Prelude..:? "scte35ReturnToNetworkSettings")
            Prelude.<*> (x Prelude..:? "hlsId3SegmentTaggingSettings")
            Prelude.<*> (x Prelude..:? "staticImageDeactivateSettings")
            Prelude.<*> (x Prelude..:? "inputPrepareSettings")
      )

instance Prelude.Hashable ScheduleActionSettings

instance Prelude.NFData ScheduleActionSettings

instance Prelude.ToJSON ScheduleActionSettings where
  toJSON ScheduleActionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("inputSwitchSettings" Prelude..=)
              Prelude.<$> inputSwitchSettings,
            ("scte35TimeSignalSettings" Prelude..=)
              Prelude.<$> scte35TimeSignalSettings,
            ("hlsTimedMetadataSettings" Prelude..=)
              Prelude.<$> hlsTimedMetadataSettings,
            ("staticImageActivateSettings" Prelude..=)
              Prelude.<$> staticImageActivateSettings,
            ("pauseStateSettings" Prelude..=)
              Prelude.<$> pauseStateSettings,
            ("scte35SpliceInsertSettings" Prelude..=)
              Prelude.<$> scte35SpliceInsertSettings,
            ("scte35ReturnToNetworkSettings" Prelude..=)
              Prelude.<$> scte35ReturnToNetworkSettings,
            ("hlsId3SegmentTaggingSettings" Prelude..=)
              Prelude.<$> hlsId3SegmentTaggingSettings,
            ("staticImageDeactivateSettings" Prelude..=)
              Prelude.<$> staticImageDeactivateSettings,
            ("inputPrepareSettings" Prelude..=)
              Prelude.<$> inputPrepareSettings
          ]
      )
