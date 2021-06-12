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

import qualified Network.AWS.Core as Core
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

-- | Holds the settings for a single schedule action.
--
-- /See:/ 'newScheduleActionSettings' smart constructor.
data ScheduleActionSettings = ScheduleActionSettings'
  { -- | Action to switch the input
    inputSwitchSettings :: Core.Maybe InputSwitchScheduleActionSettings,
    -- | Action to insert SCTE-35 time_signal message
    scte35TimeSignalSettings :: Core.Maybe Scte35TimeSignalScheduleActionSettings,
    -- | Action to insert HLS metadata
    hlsTimedMetadataSettings :: Core.Maybe HlsTimedMetadataScheduleActionSettings,
    -- | Action to activate a static image overlay
    staticImageActivateSettings :: Core.Maybe StaticImageActivateScheduleActionSettings,
    -- | Action to pause or unpause one or both channel pipelines
    pauseStateSettings :: Core.Maybe PauseStateScheduleActionSettings,
    -- | Action to insert SCTE-35 splice_insert message
    scte35SpliceInsertSettings :: Core.Maybe Scte35SpliceInsertScheduleActionSettings,
    -- | Action to insert SCTE-35 return_to_network message
    scte35ReturnToNetworkSettings :: Core.Maybe Scte35ReturnToNetworkScheduleActionSettings,
    -- | Action to insert HLS ID3 segment tagging
    hlsId3SegmentTaggingSettings :: Core.Maybe HlsId3SegmentTaggingScheduleActionSettings,
    -- | Action to deactivate a static image overlay
    staticImageDeactivateSettings :: Core.Maybe StaticImageDeactivateScheduleActionSettings,
    -- | Action to prepare an input for a future immediate input switch
    inputPrepareSettings :: Core.Maybe InputPrepareScheduleActionSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      scte35TimeSignalSettings = Core.Nothing,
      hlsTimedMetadataSettings = Core.Nothing,
      staticImageActivateSettings = Core.Nothing,
      pauseStateSettings = Core.Nothing,
      scte35SpliceInsertSettings = Core.Nothing,
      scte35ReturnToNetworkSettings = Core.Nothing,
      hlsId3SegmentTaggingSettings = Core.Nothing,
      staticImageDeactivateSettings = Core.Nothing,
      inputPrepareSettings = Core.Nothing
    }

-- | Action to switch the input
scheduleActionSettings_inputSwitchSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe InputSwitchScheduleActionSettings)
scheduleActionSettings_inputSwitchSettings = Lens.lens (\ScheduleActionSettings' {inputSwitchSettings} -> inputSwitchSettings) (\s@ScheduleActionSettings' {} a -> s {inputSwitchSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 time_signal message
scheduleActionSettings_scte35TimeSignalSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Scte35TimeSignalScheduleActionSettings)
scheduleActionSettings_scte35TimeSignalSettings = Lens.lens (\ScheduleActionSettings' {scte35TimeSignalSettings} -> scte35TimeSignalSettings) (\s@ScheduleActionSettings' {} a -> s {scte35TimeSignalSettings = a} :: ScheduleActionSettings)

-- | Action to insert HLS metadata
scheduleActionSettings_hlsTimedMetadataSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe HlsTimedMetadataScheduleActionSettings)
scheduleActionSettings_hlsTimedMetadataSettings = Lens.lens (\ScheduleActionSettings' {hlsTimedMetadataSettings} -> hlsTimedMetadataSettings) (\s@ScheduleActionSettings' {} a -> s {hlsTimedMetadataSettings = a} :: ScheduleActionSettings)

-- | Action to activate a static image overlay
scheduleActionSettings_staticImageActivateSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe StaticImageActivateScheduleActionSettings)
scheduleActionSettings_staticImageActivateSettings = Lens.lens (\ScheduleActionSettings' {staticImageActivateSettings} -> staticImageActivateSettings) (\s@ScheduleActionSettings' {} a -> s {staticImageActivateSettings = a} :: ScheduleActionSettings)

-- | Action to pause or unpause one or both channel pipelines
scheduleActionSettings_pauseStateSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe PauseStateScheduleActionSettings)
scheduleActionSettings_pauseStateSettings = Lens.lens (\ScheduleActionSettings' {pauseStateSettings} -> pauseStateSettings) (\s@ScheduleActionSettings' {} a -> s {pauseStateSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 splice_insert message
scheduleActionSettings_scte35SpliceInsertSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Scte35SpliceInsertScheduleActionSettings)
scheduleActionSettings_scte35SpliceInsertSettings = Lens.lens (\ScheduleActionSettings' {scte35SpliceInsertSettings} -> scte35SpliceInsertSettings) (\s@ScheduleActionSettings' {} a -> s {scte35SpliceInsertSettings = a} :: ScheduleActionSettings)

-- | Action to insert SCTE-35 return_to_network message
scheduleActionSettings_scte35ReturnToNetworkSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe Scte35ReturnToNetworkScheduleActionSettings)
scheduleActionSettings_scte35ReturnToNetworkSettings = Lens.lens (\ScheduleActionSettings' {scte35ReturnToNetworkSettings} -> scte35ReturnToNetworkSettings) (\s@ScheduleActionSettings' {} a -> s {scte35ReturnToNetworkSettings = a} :: ScheduleActionSettings)

-- | Action to insert HLS ID3 segment tagging
scheduleActionSettings_hlsId3SegmentTaggingSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe HlsId3SegmentTaggingScheduleActionSettings)
scheduleActionSettings_hlsId3SegmentTaggingSettings = Lens.lens (\ScheduleActionSettings' {hlsId3SegmentTaggingSettings} -> hlsId3SegmentTaggingSettings) (\s@ScheduleActionSettings' {} a -> s {hlsId3SegmentTaggingSettings = a} :: ScheduleActionSettings)

-- | Action to deactivate a static image overlay
scheduleActionSettings_staticImageDeactivateSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe StaticImageDeactivateScheduleActionSettings)
scheduleActionSettings_staticImageDeactivateSettings = Lens.lens (\ScheduleActionSettings' {staticImageDeactivateSettings} -> staticImageDeactivateSettings) (\s@ScheduleActionSettings' {} a -> s {staticImageDeactivateSettings = a} :: ScheduleActionSettings)

-- | Action to prepare an input for a future immediate input switch
scheduleActionSettings_inputPrepareSettings :: Lens.Lens' ScheduleActionSettings (Core.Maybe InputPrepareScheduleActionSettings)
scheduleActionSettings_inputPrepareSettings = Lens.lens (\ScheduleActionSettings' {inputPrepareSettings} -> inputPrepareSettings) (\s@ScheduleActionSettings' {} a -> s {inputPrepareSettings = a} :: ScheduleActionSettings)

instance Core.FromJSON ScheduleActionSettings where
  parseJSON =
    Core.withObject
      "ScheduleActionSettings"
      ( \x ->
          ScheduleActionSettings'
            Core.<$> (x Core..:? "inputSwitchSettings")
            Core.<*> (x Core..:? "scte35TimeSignalSettings")
            Core.<*> (x Core..:? "hlsTimedMetadataSettings")
            Core.<*> (x Core..:? "staticImageActivateSettings")
            Core.<*> (x Core..:? "pauseStateSettings")
            Core.<*> (x Core..:? "scte35SpliceInsertSettings")
            Core.<*> (x Core..:? "scte35ReturnToNetworkSettings")
            Core.<*> (x Core..:? "hlsId3SegmentTaggingSettings")
            Core.<*> (x Core..:? "staticImageDeactivateSettings")
            Core.<*> (x Core..:? "inputPrepareSettings")
      )

instance Core.Hashable ScheduleActionSettings

instance Core.NFData ScheduleActionSettings

instance Core.ToJSON ScheduleActionSettings where
  toJSON ScheduleActionSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("inputSwitchSettings" Core..=)
              Core.<$> inputSwitchSettings,
            ("scte35TimeSignalSettings" Core..=)
              Core.<$> scte35TimeSignalSettings,
            ("hlsTimedMetadataSettings" Core..=)
              Core.<$> hlsTimedMetadataSettings,
            ("staticImageActivateSettings" Core..=)
              Core.<$> staticImageActivateSettings,
            ("pauseStateSettings" Core..=)
              Core.<$> pauseStateSettings,
            ("scte35SpliceInsertSettings" Core..=)
              Core.<$> scte35SpliceInsertSettings,
            ("scte35ReturnToNetworkSettings" Core..=)
              Core.<$> scte35ReturnToNetworkSettings,
            ("hlsId3SegmentTaggingSettings" Core..=)
              Core.<$> hlsId3SegmentTaggingSettings,
            ("staticImageDeactivateSettings" Core..=)
              Core.<$> staticImageDeactivateSettings,
            ("inputPrepareSettings" Core..=)
              Core.<$> inputPrepareSettings
          ]
      )
