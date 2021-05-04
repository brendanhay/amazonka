{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaPackage.Types.HlsManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsManifest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.AdMarkers
import Network.AWS.MediaPackage.Types.PlaylistType
import qualified Network.AWS.Prelude as Prelude

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'newHlsManifest' smart constructor.
data HlsManifest = HlsManifest'
  { -- | This setting controls how ad markers are included in the packaged
    -- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
    -- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
    -- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
    -- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
    -- blackout tags based on SCTE-35 messages in the input source.
    -- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
    -- transition events in HLS and CMAF manifests. For this option, you must
    -- set a programDateTimeIntervalSeconds value that is greater than 0.
    adMarkers :: Prelude.Maybe AdMarkers,
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
    -- | Time window (in seconds) contained in each parent manifest.
    playlistWindowSeconds :: Prelude.Maybe Prelude.Int,
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Prelude.Maybe Prelude.Bool,
    -- | An optional short string appended to the end of the OriginEndpoint URL.
    -- If not specified, defaults to the manifestName for the OriginEndpoint.
    manifestName :: Prelude.Maybe Prelude.Text,
    -- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
    -- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
    -- included in the media playlist.
    playlistType :: Prelude.Maybe PlaylistType,
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Prelude.Maybe Prelude.Text,
    -- | The ID of the manifest. The ID must be unique within the OriginEndpoint
    -- and it cannot be changed after it is created.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HlsManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adMarkers', 'hlsManifest_adMarkers' - This setting controls how ad markers are included in the packaged
-- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
-- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
-- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
-- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
-- blackout tags based on SCTE-35 messages in the input source.
-- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
-- transition events in HLS and CMAF manifests. For this option, you must
-- set a programDateTimeIntervalSeconds value that is greater than 0.
--
-- 'programDateTimeIntervalSeconds', 'hlsManifest_programDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content. If the interval is not specified, or set to
-- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
-- and no ID3Timed Metadata messages will be generated. Note that
-- irrespective of this parameter, if any ID3 Timed Metadata is found in
-- HTTP Live Streaming (HLS) input, it will be passed through to HLS
-- output.
--
-- 'playlistWindowSeconds', 'hlsManifest_playlistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
--
-- 'includeIframeOnlyStream', 'hlsManifest_includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- 'manifestName', 'hlsManifest_manifestName' - An optional short string appended to the end of the OriginEndpoint URL.
-- If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- 'playlistType', 'hlsManifest_playlistType' - The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
--
-- 'url', 'hlsManifest_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'id', 'hlsManifest_id' - The ID of the manifest. The ID must be unique within the OriginEndpoint
-- and it cannot be changed after it is created.
newHlsManifest ::
  -- | 'id'
  Prelude.Text ->
  HlsManifest
newHlsManifest pId_ =
  HlsManifest'
    { adMarkers = Prelude.Nothing,
      programDateTimeIntervalSeconds = Prelude.Nothing,
      playlistWindowSeconds = Prelude.Nothing,
      includeIframeOnlyStream = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      playlistType = Prelude.Nothing,
      url = Prelude.Nothing,
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
hlsManifest_adMarkers :: Lens.Lens' HlsManifest (Prelude.Maybe AdMarkers)
hlsManifest_adMarkers = Lens.lens (\HlsManifest' {adMarkers} -> adMarkers) (\s@HlsManifest' {} a -> s {adMarkers = a} :: HlsManifest)

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content. If the interval is not specified, or set to
-- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
-- and no ID3Timed Metadata messages will be generated. Note that
-- irrespective of this parameter, if any ID3 Timed Metadata is found in
-- HTTP Live Streaming (HLS) input, it will be passed through to HLS
-- output.
hlsManifest_programDateTimeIntervalSeconds :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Int)
hlsManifest_programDateTimeIntervalSeconds = Lens.lens (\HlsManifest' {programDateTimeIntervalSeconds} -> programDateTimeIntervalSeconds) (\s@HlsManifest' {} a -> s {programDateTimeIntervalSeconds = a} :: HlsManifest)

-- | Time window (in seconds) contained in each parent manifest.
hlsManifest_playlistWindowSeconds :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Int)
hlsManifest_playlistWindowSeconds = Lens.lens (\HlsManifest' {playlistWindowSeconds} -> playlistWindowSeconds) (\s@HlsManifest' {} a -> s {playlistWindowSeconds = a} :: HlsManifest)

-- | When enabled, an I-Frame only stream will be included in the output.
hlsManifest_includeIframeOnlyStream :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Bool)
hlsManifest_includeIframeOnlyStream = Lens.lens (\HlsManifest' {includeIframeOnlyStream} -> includeIframeOnlyStream) (\s@HlsManifest' {} a -> s {includeIframeOnlyStream = a} :: HlsManifest)

-- | An optional short string appended to the end of the OriginEndpoint URL.
-- If not specified, defaults to the manifestName for the OriginEndpoint.
hlsManifest_manifestName :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Text)
hlsManifest_manifestName = Lens.lens (\HlsManifest' {manifestName} -> manifestName) (\s@HlsManifest' {} a -> s {manifestName = a} :: HlsManifest)

-- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
hlsManifest_playlistType :: Lens.Lens' HlsManifest (Prelude.Maybe PlaylistType)
hlsManifest_playlistType = Lens.lens (\HlsManifest' {playlistType} -> playlistType) (\s@HlsManifest' {} a -> s {playlistType = a} :: HlsManifest)

-- | The URL of the packaged OriginEndpoint for consumption.
hlsManifest_url :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Text)
hlsManifest_url = Lens.lens (\HlsManifest' {url} -> url) (\s@HlsManifest' {} a -> s {url = a} :: HlsManifest)

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint
-- and it cannot be changed after it is created.
hlsManifest_id :: Lens.Lens' HlsManifest Prelude.Text
hlsManifest_id = Lens.lens (\HlsManifest' {id} -> id) (\s@HlsManifest' {} a -> s {id = a} :: HlsManifest)

instance Prelude.FromJSON HlsManifest where
  parseJSON =
    Prelude.withObject
      "HlsManifest"
      ( \x ->
          HlsManifest'
            Prelude.<$> (x Prelude..:? "adMarkers")
            Prelude.<*> (x Prelude..:? "programDateTimeIntervalSeconds")
            Prelude.<*> (x Prelude..:? "playlistWindowSeconds")
            Prelude.<*> (x Prelude..:? "includeIframeOnlyStream")
            Prelude.<*> (x Prelude..:? "manifestName")
            Prelude.<*> (x Prelude..:? "playlistType")
            Prelude.<*> (x Prelude..:? "url")
            Prelude.<*> (x Prelude..: "id")
      )

instance Prelude.Hashable HlsManifest

instance Prelude.NFData HlsManifest
