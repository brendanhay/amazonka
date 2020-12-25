{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    hmcoupId,
    hmcoupAdMarkers,
    hmcoupAdTriggers,
    hmcoupAdsOnDeliveryRestrictions,
    hmcoupIncludeIframeOnlyStream,
    hmcoupManifestName,
    hmcoupPlaylistType,
    hmcoupPlaylistWindowSeconds,
    hmcoupProgramDateTimeIntervalSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.AdMarkers as Types
import qualified Network.AWS.MediaPackage.Types.AdTriggersElement as Types
import qualified Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions as Types
import qualified Network.AWS.MediaPackage.Types.PlaylistType as Types
import qualified Network.AWS.Prelude as Core

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'mkHlsManifestCreateOrUpdateParameters' smart constructor.
data HlsManifestCreateOrUpdateParameters = HlsManifestCreateOrUpdateParameters'
  { -- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
    id :: Core.Text,
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
    adMarkers :: Core.Maybe Types.AdMarkers,
    adTriggers :: Core.Maybe [Types.AdTriggersElement],
    adsOnDeliveryRestrictions :: Core.Maybe Types.AdsOnDeliveryRestrictions,
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Core.Maybe Core.Bool,
    -- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
    manifestName :: Core.Maybe Core.Text,
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
    programDateTimeIntervalSeconds :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsManifestCreateOrUpdateParameters' value with any optional fields omitted.
mkHlsManifestCreateOrUpdateParameters ::
  -- | 'id'
  Core.Text ->
  HlsManifestCreateOrUpdateParameters
mkHlsManifestCreateOrUpdateParameters id =
  HlsManifestCreateOrUpdateParameters'
    { id,
      adMarkers = Core.Nothing,
      adTriggers = Core.Nothing,
      adsOnDeliveryRestrictions = Core.Nothing,
      includeIframeOnlyStream = Core.Nothing,
      manifestName = Core.Nothing,
      playlistType = Core.Nothing,
      playlistWindowSeconds = Core.Nothing,
      programDateTimeIntervalSeconds = Core.Nothing
    }

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupId :: Lens.Lens' HlsManifestCreateOrUpdateParameters Core.Text
hmcoupId = Lens.field @"id"
{-# DEPRECATED hmcoupId "Use generic-lens or generic-optics with 'id' instead." #-}

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
hmcoupAdMarkers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Types.AdMarkers)
hmcoupAdMarkers = Lens.field @"adMarkers"
{-# DEPRECATED hmcoupAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupAdTriggers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe [Types.AdTriggersElement])
hmcoupAdTriggers = Lens.field @"adTriggers"
{-# DEPRECATED hmcoupAdTriggers "Use generic-lens or generic-optics with 'adTriggers' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'adsOnDeliveryRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupAdsOnDeliveryRestrictions :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Types.AdsOnDeliveryRestrictions)
hmcoupAdsOnDeliveryRestrictions = Lens.field @"adsOnDeliveryRestrictions"
{-# DEPRECATED hmcoupAdsOnDeliveryRestrictions "Use generic-lens or generic-optics with 'adsOnDeliveryRestrictions' instead." #-}

-- | When enabled, an I-Frame only stream will be included in the output.
--
-- /Note:/ Consider using 'includeIframeOnlyStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupIncludeIframeOnlyStream :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Bool)
hmcoupIncludeIframeOnlyStream = Lens.field @"includeIframeOnlyStream"
{-# DEPRECATED hmcoupIncludeIframeOnlyStream "Use generic-lens or generic-optics with 'includeIframeOnlyStream' instead." #-}

-- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupManifestName :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Text)
hmcoupManifestName = Lens.field @"manifestName"
{-# DEPRECATED hmcoupManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The HTTP Live Streaming (HLS) playlist type.
--
-- When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE
-- entry will be included in the media playlist.
--
-- /Note:/ Consider using 'playlistType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupPlaylistType :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Types.PlaylistType)
hmcoupPlaylistType = Lens.field @"playlistType"
{-# DEPRECATED hmcoupPlaylistType "Use generic-lens or generic-optics with 'playlistType' instead." #-}

-- | Time window (in seconds) contained in each parent manifest.
--
-- /Note:/ Consider using 'playlistWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmcoupPlaylistWindowSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Int)
hmcoupPlaylistWindowSeconds = Lens.field @"playlistWindowSeconds"
{-# DEPRECATED hmcoupPlaylistWindowSeconds "Use generic-lens or generic-optics with 'playlistWindowSeconds' instead." #-}

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
hmcoupProgramDateTimeIntervalSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Int)
hmcoupProgramDateTimeIntervalSeconds = Lens.field @"programDateTimeIntervalSeconds"
{-# DEPRECATED hmcoupProgramDateTimeIntervalSeconds "Use generic-lens or generic-optics with 'programDateTimeIntervalSeconds' instead." #-}

instance Core.FromJSON HlsManifestCreateOrUpdateParameters where
  toJSON HlsManifestCreateOrUpdateParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("id" Core..= id),
            ("adMarkers" Core..=) Core.<$> adMarkers,
            ("adTriggers" Core..=) Core.<$> adTriggers,
            ("adsOnDeliveryRestrictions" Core..=)
              Core.<$> adsOnDeliveryRestrictions,
            ("includeIframeOnlyStream" Core..=)
              Core.<$> includeIframeOnlyStream,
            ("manifestName" Core..=) Core.<$> manifestName,
            ("playlistType" Core..=) Core.<$> playlistType,
            ("playlistWindowSeconds" Core..=) Core.<$> playlistWindowSeconds,
            ("programDateTimeIntervalSeconds" Core..=)
              Core.<$> programDateTimeIntervalSeconds
          ]
      )
