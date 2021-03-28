{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.HlsManifest
  ( HlsManifest (..)
  -- * Smart constructor
  , mkHlsManifest
  -- * Lenses
  , hmId
  , hmAdMarkers
  , hmIncludeIframeOnlyStream
  , hmManifestName
  , hmPlaylistType
  , hmPlaylistWindowSeconds
  , hmProgramDateTimeIntervalSeconds
  , hmUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.AdMarkers as Types
import qualified Network.AWS.MediaPackage.Types.PlaylistType as Types
import qualified Network.AWS.Prelude as Core

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'mkHlsManifest' smart constructor.
data HlsManifest = HlsManifest'
  { id :: Core.Text
    -- ^ The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
  , adMarkers :: Core.Maybe Types.AdMarkers
    -- ^ This setting controls how ad markers are included in the packaged OriginEndpoint.
--
-- "NONE" will omit all SCTE-35 ad markers from the output.
-- "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad
-- markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest.
-- "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35
-- messages in the input source.
-- "DATERANGE" inserts EXT-X-DATERANGE tags to signal ad and program transition events 
-- in HLS and CMAF manifests. For this option, you must set a programDateTimeIntervalSeconds value 
-- that is greater than 0.
  , includeIframeOnlyStream :: Core.Maybe Core.Bool
    -- ^ When enabled, an I-Frame only stream will be included in the output.
  , manifestName :: Core.Maybe Core.Text
    -- ^ An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
  , playlistType :: Core.Maybe Types.PlaylistType
    -- ^ The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
  , playlistWindowSeconds :: Core.Maybe Core.Int
    -- ^ Time window (in seconds) contained in each parent manifest.
  , programDateTimeIntervalSeconds :: Core.Maybe Core.Int
    -- ^ The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
--
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content.
-- If the interval is not specified, or set to 0, then
-- no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no
-- ID3Timed Metadata messages will be generated. Note that irrespective
-- of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input,
-- it will be passed through to HLS output.
  , url :: Core.Maybe Core.Text
    -- ^ The URL of the packaged OriginEndpoint for consumption.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsManifest' value with any optional fields omitted.
mkHlsManifest
    :: Core.Text -- ^ 'id'
    -> HlsManifest
mkHlsManifest id
  = HlsManifest'{id, adMarkers = Core.Nothing,
                 includeIframeOnlyStream = Core.Nothing,
                 manifestName = Core.Nothing, playlistType = Core.Nothing,
                 playlistWindowSeconds = Core.Nothing,
                 programDateTimeIntervalSeconds = Core.Nothing, url = Core.Nothing}

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmId :: Lens.Lens' HlsManifest Core.Text
hmId = Lens.field @"id"
{-# INLINEABLE hmId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

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
hmAdMarkers :: Lens.Lens' HlsManifest (Core.Maybe Types.AdMarkers)
hmAdMarkers = Lens.field @"adMarkers"
{-# INLINEABLE hmAdMarkers #-}
{-# DEPRECATED adMarkers "Use generic-lens or generic-optics with 'adMarkers' instead"  #-}

-- | When enabled, an I-Frame only stream will be included in the output.
--
-- /Note:/ Consider using 'includeIframeOnlyStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmIncludeIframeOnlyStream :: Lens.Lens' HlsManifest (Core.Maybe Core.Bool)
hmIncludeIframeOnlyStream = Lens.field @"includeIframeOnlyStream"
{-# INLINEABLE hmIncludeIframeOnlyStream #-}
{-# DEPRECATED includeIframeOnlyStream "Use generic-lens or generic-optics with 'includeIframeOnlyStream' instead"  #-}

-- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmManifestName :: Lens.Lens' HlsManifest (Core.Maybe Core.Text)
hmManifestName = Lens.field @"manifestName"
{-# INLINEABLE hmManifestName #-}
{-# DEPRECATED manifestName "Use generic-lens or generic-optics with 'manifestName' instead"  #-}

-- | The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
--
-- /Note:/ Consider using 'playlistType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmPlaylistType :: Lens.Lens' HlsManifest (Core.Maybe Types.PlaylistType)
hmPlaylistType = Lens.field @"playlistType"
{-# INLINEABLE hmPlaylistType #-}
{-# DEPRECATED playlistType "Use generic-lens or generic-optics with 'playlistType' instead"  #-}

-- | Time window (in seconds) contained in each parent manifest.
--
-- /Note:/ Consider using 'playlistWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmPlaylistWindowSeconds :: Lens.Lens' HlsManifest (Core.Maybe Core.Int)
hmPlaylistWindowSeconds = Lens.field @"playlistWindowSeconds"
{-# INLINEABLE hmPlaylistWindowSeconds #-}
{-# DEPRECATED playlistWindowSeconds "Use generic-lens or generic-optics with 'playlistWindowSeconds' instead"  #-}

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
hmProgramDateTimeIntervalSeconds :: Lens.Lens' HlsManifest (Core.Maybe Core.Int)
hmProgramDateTimeIntervalSeconds = Lens.field @"programDateTimeIntervalSeconds"
{-# INLINEABLE hmProgramDateTimeIntervalSeconds #-}
{-# DEPRECATED programDateTimeIntervalSeconds "Use generic-lens or generic-optics with 'programDateTimeIntervalSeconds' instead"  #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmUrl :: Lens.Lens' HlsManifest (Core.Maybe Core.Text)
hmUrl = Lens.field @"url"
{-# INLINEABLE hmUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON HlsManifest where
        parseJSON
          = Core.withObject "HlsManifest" Core.$
              \ x ->
                HlsManifest' Core.<$>
                  (x Core..: "id") Core.<*> x Core..:? "adMarkers" Core.<*>
                    x Core..:? "includeIframeOnlyStream"
                    Core.<*> x Core..:? "manifestName"
                    Core.<*> x Core..:? "playlistType"
                    Core.<*> x Core..:? "playlistWindowSeconds"
                    Core.<*> x Core..:? "programDateTimeIntervalSeconds"
                    Core.<*> x Core..:? "url"
