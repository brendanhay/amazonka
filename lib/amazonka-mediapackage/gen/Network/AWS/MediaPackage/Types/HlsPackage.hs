{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsPackage
  ( HlsPackage (..),

    -- * Smart constructor
    mkHlsPackage,

    -- * Lenses
    hpAdMarkers,
    hpAdTriggers,
    hpAdsOnDeliveryRestrictions,
    hpEncryption,
    hpIncludeIframeOnlyStream,
    hpPlaylistType,
    hpPlaylistWindowSeconds,
    hpProgramDateTimeIntervalSeconds,
    hpSegmentDurationSeconds,
    hpStreamSelection,
    hpUseAudioRenditionGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.AdMarkers as Types
import qualified Network.AWS.MediaPackage.Types.AdTriggersElement as Types
import qualified Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions as Types
import qualified Network.AWS.MediaPackage.Types.HlsEncryption as Types
import qualified Network.AWS.MediaPackage.Types.PlaylistType as Types
import qualified Network.AWS.MediaPackage.Types.StreamSelection as Types
import qualified Network.AWS.Prelude as Core

-- | An HTTP Live Streaming (HLS) packaging configuration.
--
-- /See:/ 'mkHlsPackage' smart constructor.
data HlsPackage = HlsPackage'
  { -- | This setting controls how ad markers are included in the packaged OriginEndpoint.
    --
    -- "NONE" will omit all SCTE-35 ad markers from the output.
    -- "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad
    -- markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest.
    -- "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35
    -- messages in the input source.
    -- "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events
    -- in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value
    -- that is greater than 0.
    adMarkers :: Core.Maybe Types.AdMarkers,
    adTriggers :: Core.Maybe [Types.AdTriggersElement],
    adsOnDeliveryRestrictions :: Core.Maybe Types.AdsOnDeliveryRestrictions,
    encryption :: Core.Maybe Types.HlsEncryption,
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Core.Maybe Core.Bool,
    -- | The HTTP Live Streaming (HLS) playlist type.
    --
    -- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
    -- entry will be included in the media playlist.
    playlistType :: Core.Maybe Types.PlaylistType,
    -- | Time window (in seconds) contained in each parent manifest.
    playlistWindowSeconds :: Core.Maybe Core.Int,
    -- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
    --
    -- inserted into manifests. Additionally, when an interval is specified
    -- ID3Timed Metadata messages will be generated every 5 seconds using the
    -- ingest time of the content.
    -- If the interval is not specified, or set to 0, then
    -- no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no
    -- ID3Timed Metadata messages will be generated. Note that irrespective
    -- of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input,
    -- it will be passed through to HLS output.
    programDateTimeIntervalSeconds :: Core.Maybe Core.Int,
    -- | Duration (in seconds) of each fragment. Actual fragments will be
    --
    -- rounded to the nearest multiple of the source fragment duration.
    segmentDurationSeconds :: Core.Maybe Core.Int,
    streamSelection :: Core.Maybe Types.StreamSelection,
    -- | When enabled, audio streams will be placed in rendition groups in the output.
    useAudioRenditionGroup :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsPackage' value with any optional fields omitted.
mkHlsPackage ::
  HlsPackage
mkHlsPackage =
  HlsPackage'
    { adMarkers = Core.Nothing,
      adTriggers = Core.Nothing,
      adsOnDeliveryRestrictions = Core.Nothing,
      encryption = Core.Nothing,
      includeIframeOnlyStream = Core.Nothing,
      playlistType = Core.Nothing,
      playlistWindowSeconds = Core.Nothing,
      programDateTimeIntervalSeconds = Core.Nothing,
      segmentDurationSeconds = Core.Nothing,
      streamSelection = Core.Nothing,
      useAudioRenditionGroup = Core.Nothing
    }

-- | This setting controls how ad markers are included in the packaged OriginEndpoint.
--
-- "NONE" will omit all SCTE-35 ad markers from the output.
-- "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad
-- markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest.
-- "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35
-- messages in the input source.
-- "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events
-- in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value
-- that is greater than 0.
--
-- /Note:/ Consider using 'adMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpAdMarkers :: Lens.Lens' HlsPackage (Core.Maybe Types.AdMarkers)
hpAdMarkers = Lens.field @"adMarkers"
{-# DEPRECATED hpAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpAdTriggers :: Lens.Lens' HlsPackage (Core.Maybe [Types.AdTriggersElement])
hpAdTriggers = Lens.field @"adTriggers"
{-# DEPRECATED hpAdTriggers "Use generic-lens or generic-optics with 'adTriggers' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adsOnDeliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpAdsOnDeliveryRestrictions :: Lens.Lens' HlsPackage (Core.Maybe Types.AdsOnDeliveryRestrictions)
hpAdsOnDeliveryRestrictions = Lens.field @"adsOnDeliveryRestrictions"
{-# DEPRECATED hpAdsOnDeliveryRestrictions "Use generic-lens or generic-optics with 'adsOnDeliveryRestrictions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpEncryption :: Lens.Lens' HlsPackage (Core.Maybe Types.HlsEncryption)
hpEncryption = Lens.field @"encryption"
{-# DEPRECATED hpEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | When enabled, an I-Frame only stream will be included in the output.
--
-- /Note:/ Consider using 'includeIframeOnlyStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpIncludeIframeOnlyStream :: Lens.Lens' HlsPackage (Core.Maybe Core.Bool)
hpIncludeIframeOnlyStream = Lens.field @"includeIframeOnlyStream"
{-# DEPRECATED hpIncludeIframeOnlyStream "Use generic-lens or generic-optics with 'includeIframeOnlyStream' instead." #-}

-- | The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
--
-- /Note:/ Consider using 'playlistType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpPlaylistType :: Lens.Lens' HlsPackage (Core.Maybe Types.PlaylistType)
hpPlaylistType = Lens.field @"playlistType"
{-# DEPRECATED hpPlaylistType "Use generic-lens or generic-optics with 'playlistType' instead." #-}

-- | Time window (in seconds) contained in each parent manifest.
--
-- /Note:/ Consider using 'playlistWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpPlaylistWindowSeconds :: Lens.Lens' HlsPackage (Core.Maybe Core.Int)
hpPlaylistWindowSeconds = Lens.field @"playlistWindowSeconds"
{-# DEPRECATED hpPlaylistWindowSeconds "Use generic-lens or generic-optics with 'playlistWindowSeconds' instead." #-}

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
--
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content.
-- If the interval is not specified, or set to 0, then
-- no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no
-- ID3Timed Metadata messages will be generated. Note that irrespective
-- of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input,
-- it will be passed through to HLS output.
--
-- /Note:/ Consider using 'programDateTimeIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpProgramDateTimeIntervalSeconds :: Lens.Lens' HlsPackage (Core.Maybe Core.Int)
hpProgramDateTimeIntervalSeconds = Lens.field @"programDateTimeIntervalSeconds"
{-# DEPRECATED hpProgramDateTimeIntervalSeconds "Use generic-lens or generic-optics with 'programDateTimeIntervalSeconds' instead." #-}

-- | Duration (in seconds) of each fragment. Actual fragments will be
--
-- rounded to the nearest multiple of the source fragment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpSegmentDurationSeconds :: Lens.Lens' HlsPackage (Core.Maybe Core.Int)
hpSegmentDurationSeconds = Lens.field @"segmentDurationSeconds"
{-# DEPRECATED hpSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpStreamSelection :: Lens.Lens' HlsPackage (Core.Maybe Types.StreamSelection)
hpStreamSelection = Lens.field @"streamSelection"
{-# DEPRECATED hpStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

-- | When enabled, audio streams will be placed in rendition groups in the output.
--
-- /Note:/ Consider using 'useAudioRenditionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpUseAudioRenditionGroup :: Lens.Lens' HlsPackage (Core.Maybe Core.Bool)
hpUseAudioRenditionGroup = Lens.field @"useAudioRenditionGroup"
{-# DEPRECATED hpUseAudioRenditionGroup "Use generic-lens or generic-optics with 'useAudioRenditionGroup' instead." #-}

instance Core.FromJSON HlsPackage where
  toJSON HlsPackage {..} =
    Core.object
      ( Core.catMaybes
          [ ("adMarkers" Core..=) Core.<$> adMarkers,
            ("adTriggers" Core..=) Core.<$> adTriggers,
            ("adsOnDeliveryRestrictions" Core..=)
              Core.<$> adsOnDeliveryRestrictions,
            ("encryption" Core..=) Core.<$> encryption,
            ("includeIframeOnlyStream" Core..=)
              Core.<$> includeIframeOnlyStream,
            ("playlistType" Core..=) Core.<$> playlistType,
            ("playlistWindowSeconds" Core..=) Core.<$> playlistWindowSeconds,
            ("programDateTimeIntervalSeconds" Core..=)
              Core.<$> programDateTimeIntervalSeconds,
            ("segmentDurationSeconds" Core..=) Core.<$> segmentDurationSeconds,
            ("streamSelection" Core..=) Core.<$> streamSelection,
            ("useAudioRenditionGroup" Core..=)
              Core.<$> useAudioRenditionGroup
          ]
      )

instance Core.FromJSON HlsPackage where
  parseJSON =
    Core.withObject "HlsPackage" Core.$
      \x ->
        HlsPackage'
          Core.<$> (x Core..:? "adMarkers")
          Core.<*> (x Core..:? "adTriggers")
          Core.<*> (x Core..:? "adsOnDeliveryRestrictions")
          Core.<*> (x Core..:? "encryption")
          Core.<*> (x Core..:? "includeIframeOnlyStream")
          Core.<*> (x Core..:? "playlistType")
          Core.<*> (x Core..:? "playlistWindowSeconds")
          Core.<*> (x Core..:? "programDateTimeIntervalSeconds")
          Core.<*> (x Core..:? "segmentDurationSeconds")
          Core.<*> (x Core..:? "streamSelection")
          Core.<*> (x Core..:? "useAudioRenditionGroup")
