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
-- Module      : Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.AdTriggersElement
import Network.AWS.MediaPackage.Types.AdsOnDeliveryRestrictions
import Network.AWS.MediaPackage.Types.PlaylistType

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'newHlsManifestCreateOrUpdateParameters' smart constructor.
data HlsManifestCreateOrUpdateParameters = HlsManifestCreateOrUpdateParameters'
  { -- | This setting controls how ad markers are included in the packaged
    -- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
    -- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
    -- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
    -- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
    -- blackout tags based on SCTE-35 messages in the input source.
    -- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
    -- transition events in HLS and CMAF manifests. For this option, you must
    -- set a programDateTimeIntervalSeconds value that is greater than 0.
    adMarkers :: Core.Maybe AdMarkers,
    -- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
    -- inserted into manifests. Additionally, when an interval is specified
    -- ID3Timed Metadata messages will be generated every 5 seconds using the
    -- ingest time of the content. If the interval is not specified, or set to
    -- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
    -- and no ID3Timed Metadata messages will be generated. Note that
    -- irrespective of this parameter, if any ID3 Timed Metadata is found in
    -- HTTP Live Streaming (HLS) input, it will be passed through to HLS
    -- output.
    programDateTimeIntervalSeconds :: Core.Maybe Core.Int,
    -- | Time window (in seconds) contained in each parent manifest.
    playlistWindowSeconds :: Core.Maybe Core.Int,
    adTriggers :: Core.Maybe [AdTriggersElement],
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Core.Maybe Core.Bool,
    -- | An optional short string appended to the end of the OriginEndpoint URL.
    -- If not specified, defaults to the manifestName for the OriginEndpoint.
    manifestName :: Core.Maybe Core.Text,
    adsOnDeliveryRestrictions :: Core.Maybe AdsOnDeliveryRestrictions,
    -- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
    -- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
    -- included in the media playlist.
    playlistType :: Core.Maybe PlaylistType,
    -- | The ID of the manifest. The ID must be unique within the OriginEndpoint
    -- and it cannot be changed after it is created.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsManifestCreateOrUpdateParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adMarkers', 'hlsManifestCreateOrUpdateParameters_adMarkers' - This setting controls how ad markers are included in the packaged
-- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
-- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
-- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
-- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
-- blackout tags based on SCTE-35 messages in the input source.
-- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
-- transition events in HLS and CMAF manifests. For this option, you must
-- set a programDateTimeIntervalSeconds value that is greater than 0.
--
-- 'programDateTimeIntervalSeconds', 'hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content. If the interval is not specified, or set to
-- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
-- and no ID3Timed Metadata messages will be generated. Note that
-- irrespective of this parameter, if any ID3 Timed Metadata is found in
-- HTTP Live Streaming (HLS) input, it will be passed through to HLS
-- output.
--
-- 'playlistWindowSeconds', 'hlsManifestCreateOrUpdateParameters_playlistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
--
-- 'adTriggers', 'hlsManifestCreateOrUpdateParameters_adTriggers' - Undocumented member.
--
-- 'includeIframeOnlyStream', 'hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- 'manifestName', 'hlsManifestCreateOrUpdateParameters_manifestName' - An optional short string appended to the end of the OriginEndpoint URL.
-- If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- 'adsOnDeliveryRestrictions', 'hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions' - Undocumented member.
--
-- 'playlistType', 'hlsManifestCreateOrUpdateParameters_playlistType' - The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
--
-- 'id', 'hlsManifestCreateOrUpdateParameters_id' - The ID of the manifest. The ID must be unique within the OriginEndpoint
-- and it cannot be changed after it is created.
newHlsManifestCreateOrUpdateParameters ::
  -- | 'id'
  Core.Text ->
  HlsManifestCreateOrUpdateParameters
newHlsManifestCreateOrUpdateParameters pId_ =
  HlsManifestCreateOrUpdateParameters'
    { adMarkers =
        Core.Nothing,
      programDateTimeIntervalSeconds =
        Core.Nothing,
      playlistWindowSeconds = Core.Nothing,
      adTriggers = Core.Nothing,
      includeIframeOnlyStream = Core.Nothing,
      manifestName = Core.Nothing,
      adsOnDeliveryRestrictions =
        Core.Nothing,
      playlistType = Core.Nothing,
      id = pId_
    }

-- | This setting controls how ad markers are included in the packaged
-- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
-- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
-- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
-- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
-- blackout tags based on SCTE-35 messages in the input source.
-- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
-- transition events in HLS and CMAF manifests. For this option, you must
-- set a programDateTimeIntervalSeconds value that is greater than 0.
hlsManifestCreateOrUpdateParameters_adMarkers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe AdMarkers)
hlsManifestCreateOrUpdateParameters_adMarkers = Lens.lens (\HlsManifestCreateOrUpdateParameters' {adMarkers} -> adMarkers) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {adMarkers = a} :: HlsManifestCreateOrUpdateParameters)

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content. If the interval is not specified, or set to
-- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
-- and no ID3Timed Metadata messages will be generated. Note that
-- irrespective of this parameter, if any ID3 Timed Metadata is found in
-- HTTP Live Streaming (HLS) input, it will be passed through to HLS
-- output.
hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Int)
hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds = Lens.lens (\HlsManifestCreateOrUpdateParameters' {programDateTimeIntervalSeconds} -> programDateTimeIntervalSeconds) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {programDateTimeIntervalSeconds = a} :: HlsManifestCreateOrUpdateParameters)

-- | Time window (in seconds) contained in each parent manifest.
hlsManifestCreateOrUpdateParameters_playlistWindowSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Int)
hlsManifestCreateOrUpdateParameters_playlistWindowSeconds = Lens.lens (\HlsManifestCreateOrUpdateParameters' {playlistWindowSeconds} -> playlistWindowSeconds) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {playlistWindowSeconds = a} :: HlsManifestCreateOrUpdateParameters)

-- | Undocumented member.
hlsManifestCreateOrUpdateParameters_adTriggers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe [AdTriggersElement])
hlsManifestCreateOrUpdateParameters_adTriggers = Lens.lens (\HlsManifestCreateOrUpdateParameters' {adTriggers} -> adTriggers) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {adTriggers = a} :: HlsManifestCreateOrUpdateParameters) Core.. Lens.mapping Lens._Coerce

-- | When enabled, an I-Frame only stream will be included in the output.
hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Bool)
hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream = Lens.lens (\HlsManifestCreateOrUpdateParameters' {includeIframeOnlyStream} -> includeIframeOnlyStream) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {includeIframeOnlyStream = a} :: HlsManifestCreateOrUpdateParameters)

-- | An optional short string appended to the end of the OriginEndpoint URL.
-- If not specified, defaults to the manifestName for the OriginEndpoint.
hlsManifestCreateOrUpdateParameters_manifestName :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe Core.Text)
hlsManifestCreateOrUpdateParameters_manifestName = Lens.lens (\HlsManifestCreateOrUpdateParameters' {manifestName} -> manifestName) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {manifestName = a} :: HlsManifestCreateOrUpdateParameters)

-- | Undocumented member.
hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe AdsOnDeliveryRestrictions)
hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions = Lens.lens (\HlsManifestCreateOrUpdateParameters' {adsOnDeliveryRestrictions} -> adsOnDeliveryRestrictions) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {adsOnDeliveryRestrictions = a} :: HlsManifestCreateOrUpdateParameters)

-- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
hlsManifestCreateOrUpdateParameters_playlistType :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Core.Maybe PlaylistType)
hlsManifestCreateOrUpdateParameters_playlistType = Lens.lens (\HlsManifestCreateOrUpdateParameters' {playlistType} -> playlistType) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {playlistType = a} :: HlsManifestCreateOrUpdateParameters)

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint
-- and it cannot be changed after it is created.
hlsManifestCreateOrUpdateParameters_id :: Lens.Lens' HlsManifestCreateOrUpdateParameters Core.Text
hlsManifestCreateOrUpdateParameters_id = Lens.lens (\HlsManifestCreateOrUpdateParameters' {id} -> id) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {id = a} :: HlsManifestCreateOrUpdateParameters)

instance
  Core.Hashable
    HlsManifestCreateOrUpdateParameters

instance
  Core.NFData
    HlsManifestCreateOrUpdateParameters

instance
  Core.ToJSON
    HlsManifestCreateOrUpdateParameters
  where
  toJSON HlsManifestCreateOrUpdateParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("adMarkers" Core..=) Core.<$> adMarkers,
            ("programDateTimeIntervalSeconds" Core..=)
              Core.<$> programDateTimeIntervalSeconds,
            ("playlistWindowSeconds" Core..=)
              Core.<$> playlistWindowSeconds,
            ("adTriggers" Core..=) Core.<$> adTriggers,
            ("includeIframeOnlyStream" Core..=)
              Core.<$> includeIframeOnlyStream,
            ("manifestName" Core..=) Core.<$> manifestName,
            ("adsOnDeliveryRestrictions" Core..=)
              Core.<$> adsOnDeliveryRestrictions,
            ("playlistType" Core..=) Core.<$> playlistType,
            Core.Just ("id" Core..= id)
          ]
      )
