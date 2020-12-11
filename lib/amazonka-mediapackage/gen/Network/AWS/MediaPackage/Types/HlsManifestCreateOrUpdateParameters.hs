-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
  ( HlsManifestCreateOrUpdateParameters (..),

    -- * Smart constructor
    mkHlsManifestCreateOrUpdateParameters,

    -- * Lenses
    hmcoupAdsOnDeliveryRestrictions,
    hmcoupManifestName,
    hmcoupPlaylistType,
    hmcoupProgramDateTimeIntervalSeconds,
    hmcoupAdMarkers,
    hmcoupIncludeIframeOnlyStream,
    hmcoupAdTriggers,
    hmcoupPlaylistWindowSeconds,
    hmcoupId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.AdTriggersElement
import Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
import Network.AWS.MediaPackage.Types.PlaylistType
import qualified Network.AWS.Prelude as Lude

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'mkHlsManifestCreateOrUpdateParameters' smart constructor.
data HlsManifestCreateOrUpdateParameters = HlsManifestCreateOrUpdateParameters'
  { adsOnDeliveryRestrictions ::
      Lude.Maybe
        AdsOnDeliveryRestrictions,
    manifestName ::
      Lude.Maybe
        Lude.Text,
    playlistType ::
      Lude.Maybe
        PlaylistType,
    programDateTimeIntervalSeconds ::
      Lude.Maybe Lude.Int,
    adMarkers ::
      Lude.Maybe
        AdMarkers,
    includeIframeOnlyStream ::
      Lude.Maybe
        Lude.Bool,
    adTriggers ::
      Lude.Maybe
        [AdTriggersElement],
    playlistWindowSeconds ::
      Lude.Maybe Lude.Int,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsManifestCreateOrUpdateParameters' with the minimum fields required to make a request.
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
-- * 'id' - The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
-- * 'includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
-- * 'manifestName' - An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
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
mkHlsManifestCreateOrUpdateParameters ::
  -- | 'id'
  Lude.Text ->
  HlsManifestCreateOrUpdateParameters
mkHlsManifestCreateOrUpdateParameters pId_ =
  HlsManifestCreateOrUpdateParameters'
    { adsOnDeliveryRestrictions =
        Lude.Nothing,
      manifestName = Lude.Nothing,
      playlistType = Lude.Nothing,
      programDateTimeIntervalSeconds = Lude.Nothing,
      adMarkers = Lude.Nothing,
      includeIframeOnlyStream = Lude.Nothing,
      adTriggers = Lude.Nothing,
      playlistWindowSeconds = Lude.Nothing,
      id = pId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'adsOnDeliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupAdsOnDeliveryRestrictions :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe AdsOnDeliveryRestrictions)
hmcoupAdsOnDeliveryRestrictions = Lens.lens (adsOnDeliveryRestrictions :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe AdsOnDeliveryRestrictions) (\s a -> s {adsOnDeliveryRestrictions = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupAdsOnDeliveryRestrictions "Use generic-lens or generic-optics with 'adsOnDeliveryRestrictions' instead." #-}

-- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupManifestName :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe Lude.Text)
hmcoupManifestName = Lens.lens (manifestName :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
--
-- /Note:/ Consider using 'playlistType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupPlaylistType :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe PlaylistType)
hmcoupPlaylistType = Lens.lens (playlistType :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe PlaylistType) (\s a -> s {playlistType = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupPlaylistType "Use generic-lens or generic-optics with 'playlistType' instead." #-}

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
hmcoupProgramDateTimeIntervalSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe Lude.Int)
hmcoupProgramDateTimeIntervalSeconds = Lens.lens (programDateTimeIntervalSeconds :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe Lude.Int) (\s a -> s {programDateTimeIntervalSeconds = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupProgramDateTimeIntervalSeconds "Use generic-lens or generic-optics with 'programDateTimeIntervalSeconds' instead." #-}

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
hmcoupAdMarkers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe AdMarkers)
hmcoupAdMarkers = Lens.lens (adMarkers :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe AdMarkers) (\s a -> s {adMarkers = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | When enabled, an I-Frame only stream will be included in the output.
--
-- /Note:/ Consider using 'includeIframeOnlyStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupIncludeIframeOnlyStream :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe Lude.Bool)
hmcoupIncludeIframeOnlyStream = Lens.lens (includeIframeOnlyStream :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe Lude.Bool) (\s a -> s {includeIframeOnlyStream = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupIncludeIframeOnlyStream "Use generic-lens or generic-optics with 'includeIframeOnlyStream' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupAdTriggers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe [AdTriggersElement])
hmcoupAdTriggers = Lens.lens (adTriggers :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe [AdTriggersElement]) (\s a -> s {adTriggers = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupAdTriggers "Use generic-lens or generic-optics with 'adTriggers' instead." #-}

-- | Time window (in seconds) contained in each parent manifest.
--
-- /Note:/ Consider using 'playlistWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupPlaylistWindowSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Lude.Maybe Lude.Int)
hmcoupPlaylistWindowSeconds = Lens.lens (playlistWindowSeconds :: HlsManifestCreateOrUpdateParameters -> Lude.Maybe Lude.Int) (\s a -> s {playlistWindowSeconds = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupPlaylistWindowSeconds "Use generic-lens or generic-optics with 'playlistWindowSeconds' instead." #-}

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupId :: Lens.Lens' HlsManifestCreateOrUpdateParameters Lude.Text
hmcoupId = Lens.lens (id :: HlsManifestCreateOrUpdateParameters -> Lude.Text) (\s a -> s {id = a} :: HlsManifestCreateOrUpdateParameters)
{-# DEPRECATED hmcoupId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.ToJSON HlsManifestCreateOrUpdateParameters where
  toJSON HlsManifestCreateOrUpdateParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("adsOnDeliveryRestrictions" Lude..=)
              Lude.<$> adsOnDeliveryRestrictions,
            ("manifestName" Lude..=) Lude.<$> manifestName,
            ("playlistType" Lude..=) Lude.<$> playlistType,
            ("programDateTimeIntervalSeconds" Lude..=)
              Lude.<$> programDateTimeIntervalSeconds,
            ("adMarkers" Lude..=) Lude.<$> adMarkers,
            ("includeIframeOnlyStream" Lude..=)
              Lude.<$> includeIframeOnlyStream,
            ("adTriggers" Lude..=) Lude.<$> adTriggers,
            ("playlistWindowSeconds" Lude..=) Lude.<$> playlistWindowSeconds,
            Lude.Just ("id" Lude..= id)
          ]
      )
