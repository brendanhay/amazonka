{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleActionSettings
  ( ScheduleActionSettings (..),

    -- * Smart constructor
    mkScheduleActionSettings,

    -- * Lenses
    sasStaticImageDeactivateSettings,
    sasScte35SpliceInsertSettings,
    sasStaticImageActivateSettings,
    sasScte35TimeSignalSettings,
    sasInputPrepareSettings,
    sasHlsId3SegmentTaggingSettings,
    sasScte35ReturnToNetworkSettings,
    sasPauseStateSettings,
    sasHlsTimedMetadataSettings,
    sasInputSwitchSettings,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Holds the settings for a single schedule action.
--
-- /See:/ 'mkScheduleActionSettings' smart constructor.
data ScheduleActionSettings = ScheduleActionSettings'
  { -- | Action to deactivate a static image overlay
    staticImageDeactivateSettings :: Lude.Maybe StaticImageDeactivateScheduleActionSettings,
    -- | Action to insert SCTE-35 splice_insert message
    scte35SpliceInsertSettings :: Lude.Maybe Scte35SpliceInsertScheduleActionSettings,
    -- | Action to activate a static image overlay
    staticImageActivateSettings :: Lude.Maybe StaticImageActivateScheduleActionSettings,
    -- | Action to insert SCTE-35 time_signal message
    scte35TimeSignalSettings :: Lude.Maybe Scte35TimeSignalScheduleActionSettings,
    -- | Action to prepare an input for a future immediate input switch
    inputPrepareSettings :: Lude.Maybe InputPrepareScheduleActionSettings,
    -- | Action to insert HLS ID3 segment tagging
    hlsId3SegmentTaggingSettings :: Lude.Maybe HlsId3SegmentTaggingScheduleActionSettings,
    -- | Action to insert SCTE-35 return_to_network message
    scte35ReturnToNetworkSettings :: Lude.Maybe Scte35ReturnToNetworkScheduleActionSettings,
    -- | Action to pause or unpause one or both channel pipelines
    pauseStateSettings :: Lude.Maybe PauseStateScheduleActionSettings,
    -- | Action to insert HLS metadata
    hlsTimedMetadataSettings :: Lude.Maybe HlsTimedMetadataScheduleActionSettings,
    -- | Action to switch the input
    inputSwitchSettings :: Lude.Maybe InputSwitchScheduleActionSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'staticImageDeactivateSettings' - Action to deactivate a static image overlay
-- * 'scte35SpliceInsertSettings' - Action to insert SCTE-35 splice_insert message
-- * 'staticImageActivateSettings' - Action to activate a static image overlay
-- * 'scte35TimeSignalSettings' - Action to insert SCTE-35 time_signal message
-- * 'inputPrepareSettings' - Action to prepare an input for a future immediate input switch
-- * 'hlsId3SegmentTaggingSettings' - Action to insert HLS ID3 segment tagging
-- * 'scte35ReturnToNetworkSettings' - Action to insert SCTE-35 return_to_network message
-- * 'pauseStateSettings' - Action to pause or unpause one or both channel pipelines
-- * 'hlsTimedMetadataSettings' - Action to insert HLS metadata
-- * 'inputSwitchSettings' - Action to switch the input
mkScheduleActionSettings ::
  ScheduleActionSettings
mkScheduleActionSettings =
  ScheduleActionSettings'
    { staticImageDeactivateSettings =
        Lude.Nothing,
      scte35SpliceInsertSettings = Lude.Nothing,
      staticImageActivateSettings = Lude.Nothing,
      scte35TimeSignalSettings = Lude.Nothing,
      inputPrepareSettings = Lude.Nothing,
      hlsId3SegmentTaggingSettings = Lude.Nothing,
      scte35ReturnToNetworkSettings = Lude.Nothing,
      pauseStateSettings = Lude.Nothing,
      hlsTimedMetadataSettings = Lude.Nothing,
      inputSwitchSettings = Lude.Nothing
    }

-- | Action to deactivate a static image overlay
--
-- /Note:/ Consider using 'staticImageDeactivateSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStaticImageDeactivateSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe StaticImageDeactivateScheduleActionSettings)
sasStaticImageDeactivateSettings = Lens.lens (staticImageDeactivateSettings :: ScheduleActionSettings -> Lude.Maybe StaticImageDeactivateScheduleActionSettings) (\s a -> s {staticImageDeactivateSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasStaticImageDeactivateSettings "Use generic-lens or generic-optics with 'staticImageDeactivateSettings' instead." #-}

-- | Action to insert SCTE-35 splice_insert message
--
-- /Note:/ Consider using 'scte35SpliceInsertSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasScte35SpliceInsertSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe Scte35SpliceInsertScheduleActionSettings)
sasScte35SpliceInsertSettings = Lens.lens (scte35SpliceInsertSettings :: ScheduleActionSettings -> Lude.Maybe Scte35SpliceInsertScheduleActionSettings) (\s a -> s {scte35SpliceInsertSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasScte35SpliceInsertSettings "Use generic-lens or generic-optics with 'scte35SpliceInsertSettings' instead." #-}

-- | Action to activate a static image overlay
--
-- /Note:/ Consider using 'staticImageActivateSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStaticImageActivateSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe StaticImageActivateScheduleActionSettings)
sasStaticImageActivateSettings = Lens.lens (staticImageActivateSettings :: ScheduleActionSettings -> Lude.Maybe StaticImageActivateScheduleActionSettings) (\s a -> s {staticImageActivateSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasStaticImageActivateSettings "Use generic-lens or generic-optics with 'staticImageActivateSettings' instead." #-}

-- | Action to insert SCTE-35 time_signal message
--
-- /Note:/ Consider using 'scte35TimeSignalSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasScte35TimeSignalSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe Scte35TimeSignalScheduleActionSettings)
sasScte35TimeSignalSettings = Lens.lens (scte35TimeSignalSettings :: ScheduleActionSettings -> Lude.Maybe Scte35TimeSignalScheduleActionSettings) (\s a -> s {scte35TimeSignalSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasScte35TimeSignalSettings "Use generic-lens or generic-optics with 'scte35TimeSignalSettings' instead." #-}

-- | Action to prepare an input for a future immediate input switch
--
-- /Note:/ Consider using 'inputPrepareSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasInputPrepareSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe InputPrepareScheduleActionSettings)
sasInputPrepareSettings = Lens.lens (inputPrepareSettings :: ScheduleActionSettings -> Lude.Maybe InputPrepareScheduleActionSettings) (\s a -> s {inputPrepareSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasInputPrepareSettings "Use generic-lens or generic-optics with 'inputPrepareSettings' instead." #-}

-- | Action to insert HLS ID3 segment tagging
--
-- /Note:/ Consider using 'hlsId3SegmentTaggingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasHlsId3SegmentTaggingSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe HlsId3SegmentTaggingScheduleActionSettings)
sasHlsId3SegmentTaggingSettings = Lens.lens (hlsId3SegmentTaggingSettings :: ScheduleActionSettings -> Lude.Maybe HlsId3SegmentTaggingScheduleActionSettings) (\s a -> s {hlsId3SegmentTaggingSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasHlsId3SegmentTaggingSettings "Use generic-lens or generic-optics with 'hlsId3SegmentTaggingSettings' instead." #-}

-- | Action to insert SCTE-35 return_to_network message
--
-- /Note:/ Consider using 'scte35ReturnToNetworkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasScte35ReturnToNetworkSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe Scte35ReturnToNetworkScheduleActionSettings)
sasScte35ReturnToNetworkSettings = Lens.lens (scte35ReturnToNetworkSettings :: ScheduleActionSettings -> Lude.Maybe Scte35ReturnToNetworkScheduleActionSettings) (\s a -> s {scte35ReturnToNetworkSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasScte35ReturnToNetworkSettings "Use generic-lens or generic-optics with 'scte35ReturnToNetworkSettings' instead." #-}

-- | Action to pause or unpause one or both channel pipelines
--
-- /Note:/ Consider using 'pauseStateSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasPauseStateSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe PauseStateScheduleActionSettings)
sasPauseStateSettings = Lens.lens (pauseStateSettings :: ScheduleActionSettings -> Lude.Maybe PauseStateScheduleActionSettings) (\s a -> s {pauseStateSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasPauseStateSettings "Use generic-lens or generic-optics with 'pauseStateSettings' instead." #-}

-- | Action to insert HLS metadata
--
-- /Note:/ Consider using 'hlsTimedMetadataSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasHlsTimedMetadataSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe HlsTimedMetadataScheduleActionSettings)
sasHlsTimedMetadataSettings = Lens.lens (hlsTimedMetadataSettings :: ScheduleActionSettings -> Lude.Maybe HlsTimedMetadataScheduleActionSettings) (\s a -> s {hlsTimedMetadataSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasHlsTimedMetadataSettings "Use generic-lens or generic-optics with 'hlsTimedMetadataSettings' instead." #-}

-- | Action to switch the input
--
-- /Note:/ Consider using 'inputSwitchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasInputSwitchSettings :: Lens.Lens' ScheduleActionSettings (Lude.Maybe InputSwitchScheduleActionSettings)
sasInputSwitchSettings = Lens.lens (inputSwitchSettings :: ScheduleActionSettings -> Lude.Maybe InputSwitchScheduleActionSettings) (\s a -> s {inputSwitchSettings = a} :: ScheduleActionSettings)
{-# DEPRECATED sasInputSwitchSettings "Use generic-lens or generic-optics with 'inputSwitchSettings' instead." #-}

instance Lude.FromJSON ScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "ScheduleActionSettings"
      ( \x ->
          ScheduleActionSettings'
            Lude.<$> (x Lude..:? "staticImageDeactivateSettings")
            Lude.<*> (x Lude..:? "scte35SpliceInsertSettings")
            Lude.<*> (x Lude..:? "staticImageActivateSettings")
            Lude.<*> (x Lude..:? "scte35TimeSignalSettings")
            Lude.<*> (x Lude..:? "inputPrepareSettings")
            Lude.<*> (x Lude..:? "hlsId3SegmentTaggingSettings")
            Lude.<*> (x Lude..:? "scte35ReturnToNetworkSettings")
            Lude.<*> (x Lude..:? "pauseStateSettings")
            Lude.<*> (x Lude..:? "hlsTimedMetadataSettings")
            Lude.<*> (x Lude..:? "inputSwitchSettings")
      )

instance Lude.ToJSON ScheduleActionSettings where
  toJSON ScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("staticImageDeactivateSettings" Lude..=)
              Lude.<$> staticImageDeactivateSettings,
            ("scte35SpliceInsertSettings" Lude..=)
              Lude.<$> scte35SpliceInsertSettings,
            ("staticImageActivateSettings" Lude..=)
              Lude.<$> staticImageActivateSettings,
            ("scte35TimeSignalSettings" Lude..=)
              Lude.<$> scte35TimeSignalSettings,
            ("inputPrepareSettings" Lude..=) Lude.<$> inputPrepareSettings,
            ("hlsId3SegmentTaggingSettings" Lude..=)
              Lude.<$> hlsId3SegmentTaggingSettings,
            ("scte35ReturnToNetworkSettings" Lude..=)
              Lude.<$> scte35ReturnToNetworkSettings,
            ("pauseStateSettings" Lude..=) Lude.<$> pauseStateSettings,
            ("hlsTimedMetadataSettings" Lude..=)
              Lude.<$> hlsTimedMetadataSettings,
            ("inputSwitchSettings" Lude..=) Lude.<$> inputSwitchSettings
          ]
      )
