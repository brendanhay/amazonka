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
-- Module      : Amazonka.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.HlsManifestCreateOrUpdateParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackage.Types.AdMarkers
import Amazonka.MediaPackage.Types.AdTriggersElement
import Amazonka.MediaPackage.Types.AdsOnDeliveryRestrictions
import Amazonka.MediaPackage.Types.PlaylistType
import qualified Amazonka.Prelude as Prelude

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'newHlsManifestCreateOrUpdateParameters' smart constructor.
data HlsManifestCreateOrUpdateParameters = HlsManifestCreateOrUpdateParameters'
  { adsOnDeliveryRestrictions :: Prelude.Maybe AdsOnDeliveryRestrictions,
    -- | An optional short string appended to the end of the OriginEndpoint URL.
    -- If not specified, defaults to the manifestName for the OriginEndpoint.
    manifestName :: Prelude.Maybe Prelude.Text,
    -- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
    -- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
    -- included in the media playlist.
    playlistType :: Prelude.Maybe PlaylistType,
    -- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
    -- inserted into manifests. Additionally, when an interval is specified
    -- ID3Timed Metadata messages will be generated every 5 seconds using the
    -- ingest time of the content. If the interval is not specified, or set to
    -- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
    -- and no ID3Timed Metadata messages will be generated. Note that
    -- irrespective of this parameter, if any ID3 Timed Metadata is found in
    -- HTTP Live Streaming (HLS) input, it will be passed through to HLS
    -- output.
    programDateTimeIntervalSeconds :: Prelude.Maybe Prelude.Int,
    -- | This setting controls how ad markers are included in the packaged
    -- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
    -- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
    -- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
    -- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
    -- blackout tags based on SCTE-35 messages in the input source.
    -- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
    -- transition events in HLS and CMAF manifests. For this option, you must
    -- set a programDateTimeIntervalSeconds value that is greater than 0.
    adMarkers :: Prelude.Maybe AdMarkers,
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Prelude.Maybe Prelude.Bool,
    adTriggers :: Prelude.Maybe [AdTriggersElement],
    -- | Time window (in seconds) contained in each parent manifest.
    playlistWindowSeconds :: Prelude.Maybe Prelude.Int,
    -- | The ID of the manifest. The ID must be unique within the OriginEndpoint
    -- and it cannot be changed after it is created.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsManifestCreateOrUpdateParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adsOnDeliveryRestrictions', 'hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions' - Undocumented member.
--
-- 'manifestName', 'hlsManifestCreateOrUpdateParameters_manifestName' - An optional short string appended to the end of the OriginEndpoint URL.
-- If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- 'playlistType', 'hlsManifestCreateOrUpdateParameters_playlistType' - The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
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
-- 'includeIframeOnlyStream', 'hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- 'adTriggers', 'hlsManifestCreateOrUpdateParameters_adTriggers' - Undocumented member.
--
-- 'playlistWindowSeconds', 'hlsManifestCreateOrUpdateParameters_playlistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
--
-- 'id', 'hlsManifestCreateOrUpdateParameters_id' - The ID of the manifest. The ID must be unique within the OriginEndpoint
-- and it cannot be changed after it is created.
newHlsManifestCreateOrUpdateParameters ::
  -- | 'id'
  Prelude.Text ->
  HlsManifestCreateOrUpdateParameters
newHlsManifestCreateOrUpdateParameters pId_ =
  HlsManifestCreateOrUpdateParameters'
    { adsOnDeliveryRestrictions =
        Prelude.Nothing,
      manifestName = Prelude.Nothing,
      playlistType = Prelude.Nothing,
      programDateTimeIntervalSeconds =
        Prelude.Nothing,
      adMarkers = Prelude.Nothing,
      includeIframeOnlyStream =
        Prelude.Nothing,
      adTriggers = Prelude.Nothing,
      playlistWindowSeconds =
        Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe AdsOnDeliveryRestrictions)
hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions = Lens.lens (\HlsManifestCreateOrUpdateParameters' {adsOnDeliveryRestrictions} -> adsOnDeliveryRestrictions) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {adsOnDeliveryRestrictions = a} :: HlsManifestCreateOrUpdateParameters)

-- | An optional short string appended to the end of the OriginEndpoint URL.
-- If not specified, defaults to the manifestName for the OriginEndpoint.
hlsManifestCreateOrUpdateParameters_manifestName :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe Prelude.Text)
hlsManifestCreateOrUpdateParameters_manifestName = Lens.lens (\HlsManifestCreateOrUpdateParameters' {manifestName} -> manifestName) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {manifestName = a} :: HlsManifestCreateOrUpdateParameters)

-- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
hlsManifestCreateOrUpdateParameters_playlistType :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe PlaylistType)
hlsManifestCreateOrUpdateParameters_playlistType = Lens.lens (\HlsManifestCreateOrUpdateParameters' {playlistType} -> playlistType) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {playlistType = a} :: HlsManifestCreateOrUpdateParameters)

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content. If the interval is not specified, or set to
-- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
-- and no ID3Timed Metadata messages will be generated. Note that
-- irrespective of this parameter, if any ID3 Timed Metadata is found in
-- HTTP Live Streaming (HLS) input, it will be passed through to HLS
-- output.
hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe Prelude.Int)
hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds = Lens.lens (\HlsManifestCreateOrUpdateParameters' {programDateTimeIntervalSeconds} -> programDateTimeIntervalSeconds) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {programDateTimeIntervalSeconds = a} :: HlsManifestCreateOrUpdateParameters)

-- | This setting controls how ad markers are included in the packaged
-- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
-- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
-- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
-- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
-- blackout tags based on SCTE-35 messages in the input source.
-- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
-- transition events in HLS and CMAF manifests. For this option, you must
-- set a programDateTimeIntervalSeconds value that is greater than 0.
hlsManifestCreateOrUpdateParameters_adMarkers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe AdMarkers)
hlsManifestCreateOrUpdateParameters_adMarkers = Lens.lens (\HlsManifestCreateOrUpdateParameters' {adMarkers} -> adMarkers) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {adMarkers = a} :: HlsManifestCreateOrUpdateParameters)

-- | When enabled, an I-Frame only stream will be included in the output.
hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe Prelude.Bool)
hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream = Lens.lens (\HlsManifestCreateOrUpdateParameters' {includeIframeOnlyStream} -> includeIframeOnlyStream) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {includeIframeOnlyStream = a} :: HlsManifestCreateOrUpdateParameters)

-- | Undocumented member.
hlsManifestCreateOrUpdateParameters_adTriggers :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe [AdTriggersElement])
hlsManifestCreateOrUpdateParameters_adTriggers = Lens.lens (\HlsManifestCreateOrUpdateParameters' {adTriggers} -> adTriggers) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {adTriggers = a} :: HlsManifestCreateOrUpdateParameters) Prelude.. Lens.mapping Lens.coerced

-- | Time window (in seconds) contained in each parent manifest.
hlsManifestCreateOrUpdateParameters_playlistWindowSeconds :: Lens.Lens' HlsManifestCreateOrUpdateParameters (Prelude.Maybe Prelude.Int)
hlsManifestCreateOrUpdateParameters_playlistWindowSeconds = Lens.lens (\HlsManifestCreateOrUpdateParameters' {playlistWindowSeconds} -> playlistWindowSeconds) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {playlistWindowSeconds = a} :: HlsManifestCreateOrUpdateParameters)

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint
-- and it cannot be changed after it is created.
hlsManifestCreateOrUpdateParameters_id :: Lens.Lens' HlsManifestCreateOrUpdateParameters Prelude.Text
hlsManifestCreateOrUpdateParameters_id = Lens.lens (\HlsManifestCreateOrUpdateParameters' {id} -> id) (\s@HlsManifestCreateOrUpdateParameters' {} a -> s {id = a} :: HlsManifestCreateOrUpdateParameters)

instance
  Prelude.Hashable
    HlsManifestCreateOrUpdateParameters
  where
  hashWithSalt
    salt'
    HlsManifestCreateOrUpdateParameters' {..} =
      salt' `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` playlistWindowSeconds
        `Prelude.hashWithSalt` adTriggers
        `Prelude.hashWithSalt` includeIframeOnlyStream
        `Prelude.hashWithSalt` adMarkers
        `Prelude.hashWithSalt` programDateTimeIntervalSeconds
        `Prelude.hashWithSalt` playlistType
        `Prelude.hashWithSalt` manifestName
        `Prelude.hashWithSalt` adsOnDeliveryRestrictions

instance
  Prelude.NFData
    HlsManifestCreateOrUpdateParameters
  where
  rnf HlsManifestCreateOrUpdateParameters' {..} =
    Prelude.rnf adsOnDeliveryRestrictions
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf playlistWindowSeconds
      `Prelude.seq` Prelude.rnf adTriggers
      `Prelude.seq` Prelude.rnf includeIframeOnlyStream
      `Prelude.seq` Prelude.rnf adMarkers
      `Prelude.seq` Prelude.rnf programDateTimeIntervalSeconds
      `Prelude.seq` Prelude.rnf playlistType
      `Prelude.seq` Prelude.rnf manifestName

instance
  Core.ToJSON
    HlsManifestCreateOrUpdateParameters
  where
  toJSON HlsManifestCreateOrUpdateParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("adsOnDeliveryRestrictions" Core..=)
              Prelude.<$> adsOnDeliveryRestrictions,
            ("manifestName" Core..=) Prelude.<$> manifestName,
            ("playlistType" Core..=) Prelude.<$> playlistType,
            ("programDateTimeIntervalSeconds" Core..=)
              Prelude.<$> programDateTimeIntervalSeconds,
            ("adMarkers" Core..=) Prelude.<$> adMarkers,
            ("includeIframeOnlyStream" Core..=)
              Prelude.<$> includeIframeOnlyStream,
            ("adTriggers" Core..=) Prelude.<$> adTriggers,
            ("playlistWindowSeconds" Core..=)
              Prelude.<$> playlistWindowSeconds,
            Prelude.Just ("id" Core..= id)
          ]
      )
