-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsManifest
  ( HlsManifest (..),

    -- * Smart constructor
    mkHlsManifest,

    -- * Lenses
    hmManifestName,
    hmURL,
    hmPlaylistType,
    hmProgramDateTimeIntervalSeconds,
    hmAdMarkers,
    hmIncludeIframeOnlyStream,
    hmPlaylistWindowSeconds,
    hmId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.PlaylistType
import qualified Network.AWS.Prelude as Lude

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'mkHlsManifest' smart constructor.
data HlsManifest = HlsManifest'
  { manifestName ::
      Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    playlistType :: Lude.Maybe PlaylistType,
    programDateTimeIntervalSeconds :: Lude.Maybe Lude.Int,
    adMarkers :: Lude.Maybe AdMarkers,
    includeIframeOnlyStream :: Lude.Maybe Lude.Bool,
    playlistWindowSeconds :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'HlsManifest' with the minimum fields required to make a request.
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
-- * 'url' - The URL of the packaged OriginEndpoint for consumption.
mkHlsManifest ::
  -- | 'id'
  Lude.Text ->
  HlsManifest
mkHlsManifest pId_ =
  HlsManifest'
    { manifestName = Lude.Nothing,
      url = Lude.Nothing,
      playlistType = Lude.Nothing,
      programDateTimeIntervalSeconds = Lude.Nothing,
      adMarkers = Lude.Nothing,
      includeIframeOnlyStream = Lude.Nothing,
      playlistWindowSeconds = Lude.Nothing,
      id = pId_
    }

-- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmManifestName :: Lens.Lens' HlsManifest (Lude.Maybe Lude.Text)
hmManifestName = Lens.lens (manifestName :: HlsManifest -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: HlsManifest)
{-# DEPRECATED hmManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmURL :: Lens.Lens' HlsManifest (Lude.Maybe Lude.Text)
hmURL = Lens.lens (url :: HlsManifest -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: HlsManifest)
{-# DEPRECATED hmURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
--
-- /Note:/ Consider using 'playlistType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmPlaylistType :: Lens.Lens' HlsManifest (Lude.Maybe PlaylistType)
hmPlaylistType = Lens.lens (playlistType :: HlsManifest -> Lude.Maybe PlaylistType) (\s a -> s {playlistType = a} :: HlsManifest)
{-# DEPRECATED hmPlaylistType "Use generic-lens or generic-optics with 'playlistType' instead." #-}

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
hmProgramDateTimeIntervalSeconds :: Lens.Lens' HlsManifest (Lude.Maybe Lude.Int)
hmProgramDateTimeIntervalSeconds = Lens.lens (programDateTimeIntervalSeconds :: HlsManifest -> Lude.Maybe Lude.Int) (\s a -> s {programDateTimeIntervalSeconds = a} :: HlsManifest)
{-# DEPRECATED hmProgramDateTimeIntervalSeconds "Use generic-lens or generic-optics with 'programDateTimeIntervalSeconds' instead." #-}

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
hmAdMarkers :: Lens.Lens' HlsManifest (Lude.Maybe AdMarkers)
hmAdMarkers = Lens.lens (adMarkers :: HlsManifest -> Lude.Maybe AdMarkers) (\s a -> s {adMarkers = a} :: HlsManifest)
{-# DEPRECATED hmAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | When enabled, an I-Frame only stream will be included in the output.
--
-- /Note:/ Consider using 'includeIframeOnlyStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmIncludeIframeOnlyStream :: Lens.Lens' HlsManifest (Lude.Maybe Lude.Bool)
hmIncludeIframeOnlyStream = Lens.lens (includeIframeOnlyStream :: HlsManifest -> Lude.Maybe Lude.Bool) (\s a -> s {includeIframeOnlyStream = a} :: HlsManifest)
{-# DEPRECATED hmIncludeIframeOnlyStream "Use generic-lens or generic-optics with 'includeIframeOnlyStream' instead." #-}

-- | Time window (in seconds) contained in each parent manifest.
--
-- /Note:/ Consider using 'playlistWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmPlaylistWindowSeconds :: Lens.Lens' HlsManifest (Lude.Maybe Lude.Int)
hmPlaylistWindowSeconds = Lens.lens (playlistWindowSeconds :: HlsManifest -> Lude.Maybe Lude.Int) (\s a -> s {playlistWindowSeconds = a} :: HlsManifest)
{-# DEPRECATED hmPlaylistWindowSeconds "Use generic-lens or generic-optics with 'playlistWindowSeconds' instead." #-}

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmId :: Lens.Lens' HlsManifest Lude.Text
hmId = Lens.lens (id :: HlsManifest -> Lude.Text) (\s a -> s {id = a} :: HlsManifest)
{-# DEPRECATED hmId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON HlsManifest where
  parseJSON =
    Lude.withObject
      "HlsManifest"
      ( \x ->
          HlsManifest'
            Lude.<$> (x Lude..:? "manifestName")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "playlistType")
            Lude.<*> (x Lude..:? "programDateTimeIntervalSeconds")
            Lude.<*> (x Lude..:? "adMarkers")
            Lude.<*> (x Lude..:? "includeIframeOnlyStream")
            Lude.<*> (x Lude..:? "playlistWindowSeconds")
            Lude.<*> (x Lude..: "id")
      )
