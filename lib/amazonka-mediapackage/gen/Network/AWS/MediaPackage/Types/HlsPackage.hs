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
    hpAdsOnDeliveryRestrictions,
    hpUseAudioRenditionGroup,
    hpPlaylistType,
    hpSegmentDurationSeconds,
    hpProgramDateTimeIntervalSeconds,
    hpStreamSelection,
    hpAdMarkers,
    hpEncryption,
    hpIncludeIframeOnlyStream,
    hpAdTriggers,
    hpPlaylistWindowSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.AdTriggersElement
import Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
import Network.AWS.MediaPackage.Types.HlsEncryption
import Network.AWS.MediaPackage.Types.PlaylistType
import Network.AWS.MediaPackage.Types.StreamSelection
import qualified Network.AWS.Prelude as Lude

-- | An HTTP Live Streaming (HLS) packaging configuration.
--
-- /See:/ 'mkHlsPackage' smart constructor.
data HlsPackage = HlsPackage'
  { adsOnDeliveryRestrictions ::
      Lude.Maybe AdsOnDeliveryRestrictions,
    useAudioRenditionGroup :: Lude.Maybe Lude.Bool,
    playlistType :: Lude.Maybe PlaylistType,
    segmentDurationSeconds :: Lude.Maybe Lude.Int,
    programDateTimeIntervalSeconds :: Lude.Maybe Lude.Int,
    streamSelection :: Lude.Maybe StreamSelection,
    adMarkers :: Lude.Maybe AdMarkers,
    encryption :: Lude.Maybe HlsEncryption,
    includeIframeOnlyStream :: Lude.Maybe Lude.Bool,
    adTriggers :: Lude.Maybe [AdTriggersElement],
    playlistWindowSeconds :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsPackage' with the minimum fields required to make a request.
--
-- * 'adMarkers' - This setting controls how ad markers are included in the packaged OriginEndpoint.
--
-- "NONE" will omit all SCTE-35 ad markers from the output.
-- "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad
-- markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest.
-- "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35
-- messages in the input source.
-- "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events
-- in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value
-- that is greater than 0.
-- * 'adTriggers' - Undocumented field.
-- * 'adsOnDeliveryRestrictions' - Undocumented field.
-- * 'encryption' - Undocumented field.
-- * 'includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
-- * 'playlistType' - The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
-- * 'playlistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
-- * 'programDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
--
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content.
-- If the interval is not specified, or set to 0, then
-- no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no
-- ID3Timed Metadata messages will be generated. Note that irrespective
-- of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input,
-- it will be passed through to HLS output.
-- * 'segmentDurationSeconds' - Duration (in seconds) of each fragment. Actual fragments will be
--
-- rounded to the nearest multiple of the source fragment duration.
-- * 'streamSelection' - Undocumented field.
-- * 'useAudioRenditionGroup' - When enabled, audio streams will be placed in rendition groups in the output.
mkHlsPackage ::
  HlsPackage
mkHlsPackage =
  HlsPackage'
    { adsOnDeliveryRestrictions = Lude.Nothing,
      useAudioRenditionGroup = Lude.Nothing,
      playlistType = Lude.Nothing,
      segmentDurationSeconds = Lude.Nothing,
      programDateTimeIntervalSeconds = Lude.Nothing,
      streamSelection = Lude.Nothing,
      adMarkers = Lude.Nothing,
      encryption = Lude.Nothing,
      includeIframeOnlyStream = Lude.Nothing,
      adTriggers = Lude.Nothing,
      playlistWindowSeconds = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'adsOnDeliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpAdsOnDeliveryRestrictions :: Lens.Lens' HlsPackage (Lude.Maybe AdsOnDeliveryRestrictions)
hpAdsOnDeliveryRestrictions = Lens.lens (adsOnDeliveryRestrictions :: HlsPackage -> Lude.Maybe AdsOnDeliveryRestrictions) (\s a -> s {adsOnDeliveryRestrictions = a} :: HlsPackage)
{-# DEPRECATED hpAdsOnDeliveryRestrictions "Use generic-lens or generic-optics with 'adsOnDeliveryRestrictions' instead." #-}

-- | When enabled, audio streams will be placed in rendition groups in the output.
--
-- /Note:/ Consider using 'useAudioRenditionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpUseAudioRenditionGroup :: Lens.Lens' HlsPackage (Lude.Maybe Lude.Bool)
hpUseAudioRenditionGroup = Lens.lens (useAudioRenditionGroup :: HlsPackage -> Lude.Maybe Lude.Bool) (\s a -> s {useAudioRenditionGroup = a} :: HlsPackage)
{-# DEPRECATED hpUseAudioRenditionGroup "Use generic-lens or generic-optics with 'useAudioRenditionGroup' instead." #-}

-- | The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
--
-- /Note:/ Consider using 'playlistType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpPlaylistType :: Lens.Lens' HlsPackage (Lude.Maybe PlaylistType)
hpPlaylistType = Lens.lens (playlistType :: HlsPackage -> Lude.Maybe PlaylistType) (\s a -> s {playlistType = a} :: HlsPackage)
{-# DEPRECATED hpPlaylistType "Use generic-lens or generic-optics with 'playlistType' instead." #-}

-- | Duration (in seconds) of each fragment. Actual fragments will be
--
-- rounded to the nearest multiple of the source fragment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpSegmentDurationSeconds :: Lens.Lens' HlsPackage (Lude.Maybe Lude.Int)
hpSegmentDurationSeconds = Lens.lens (segmentDurationSeconds :: HlsPackage -> Lude.Maybe Lude.Int) (\s a -> s {segmentDurationSeconds = a} :: HlsPackage)
{-# DEPRECATED hpSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

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
hpProgramDateTimeIntervalSeconds :: Lens.Lens' HlsPackage (Lude.Maybe Lude.Int)
hpProgramDateTimeIntervalSeconds = Lens.lens (programDateTimeIntervalSeconds :: HlsPackage -> Lude.Maybe Lude.Int) (\s a -> s {programDateTimeIntervalSeconds = a} :: HlsPackage)
{-# DEPRECATED hpProgramDateTimeIntervalSeconds "Use generic-lens or generic-optics with 'programDateTimeIntervalSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpStreamSelection :: Lens.Lens' HlsPackage (Lude.Maybe StreamSelection)
hpStreamSelection = Lens.lens (streamSelection :: HlsPackage -> Lude.Maybe StreamSelection) (\s a -> s {streamSelection = a} :: HlsPackage)
{-# DEPRECATED hpStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

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
hpAdMarkers :: Lens.Lens' HlsPackage (Lude.Maybe AdMarkers)
hpAdMarkers = Lens.lens (adMarkers :: HlsPackage -> Lude.Maybe AdMarkers) (\s a -> s {adMarkers = a} :: HlsPackage)
{-# DEPRECATED hpAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpEncryption :: Lens.Lens' HlsPackage (Lude.Maybe HlsEncryption)
hpEncryption = Lens.lens (encryption :: HlsPackage -> Lude.Maybe HlsEncryption) (\s a -> s {encryption = a} :: HlsPackage)
{-# DEPRECATED hpEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | When enabled, an I-Frame only stream will be included in the output.
--
-- /Note:/ Consider using 'includeIframeOnlyStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpIncludeIframeOnlyStream :: Lens.Lens' HlsPackage (Lude.Maybe Lude.Bool)
hpIncludeIframeOnlyStream = Lens.lens (includeIframeOnlyStream :: HlsPackage -> Lude.Maybe Lude.Bool) (\s a -> s {includeIframeOnlyStream = a} :: HlsPackage)
{-# DEPRECATED hpIncludeIframeOnlyStream "Use generic-lens or generic-optics with 'includeIframeOnlyStream' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpAdTriggers :: Lens.Lens' HlsPackage (Lude.Maybe [AdTriggersElement])
hpAdTriggers = Lens.lens (adTriggers :: HlsPackage -> Lude.Maybe [AdTriggersElement]) (\s a -> s {adTriggers = a} :: HlsPackage)
{-# DEPRECATED hpAdTriggers "Use generic-lens or generic-optics with 'adTriggers' instead." #-}

-- | Time window (in seconds) contained in each parent manifest.
--
-- /Note:/ Consider using 'playlistWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpPlaylistWindowSeconds :: Lens.Lens' HlsPackage (Lude.Maybe Lude.Int)
hpPlaylistWindowSeconds = Lens.lens (playlistWindowSeconds :: HlsPackage -> Lude.Maybe Lude.Int) (\s a -> s {playlistWindowSeconds = a} :: HlsPackage)
{-# DEPRECATED hpPlaylistWindowSeconds "Use generic-lens or generic-optics with 'playlistWindowSeconds' instead." #-}

instance Lude.FromJSON HlsPackage where
  parseJSON =
    Lude.withObject
      "HlsPackage"
      ( \x ->
          HlsPackage'
            Lude.<$> (x Lude..:? "adsOnDeliveryRestrictions")
            Lude.<*> (x Lude..:? "useAudioRenditionGroup")
            Lude.<*> (x Lude..:? "playlistType")
            Lude.<*> (x Lude..:? "segmentDurationSeconds")
            Lude.<*> (x Lude..:? "programDateTimeIntervalSeconds")
            Lude.<*> (x Lude..:? "streamSelection")
            Lude.<*> (x Lude..:? "adMarkers")
            Lude.<*> (x Lude..:? "encryption")
            Lude.<*> (x Lude..:? "includeIframeOnlyStream")
            Lude.<*> (x Lude..:? "adTriggers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "playlistWindowSeconds")
      )

instance Lude.ToJSON HlsPackage where
  toJSON HlsPackage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("adsOnDeliveryRestrictions" Lude..=)
              Lude.<$> adsOnDeliveryRestrictions,
            ("useAudioRenditionGroup" Lude..=) Lude.<$> useAudioRenditionGroup,
            ("playlistType" Lude..=) Lude.<$> playlistType,
            ("segmentDurationSeconds" Lude..=) Lude.<$> segmentDurationSeconds,
            ("programDateTimeIntervalSeconds" Lude..=)
              Lude.<$> programDateTimeIntervalSeconds,
            ("streamSelection" Lude..=) Lude.<$> streamSelection,
            ("adMarkers" Lude..=) Lude.<$> adMarkers,
            ("encryption" Lude..=) Lude.<$> encryption,
            ("includeIframeOnlyStream" Lude..=)
              Lude.<$> includeIframeOnlyStream,
            ("adTriggers" Lude..=) Lude.<$> adTriggers,
            ("playlistWindowSeconds" Lude..=) Lude.<$> playlistWindowSeconds
          ]
      )
