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
-- Module      : Amazonka.MediaPackage.Types.HlsPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.HlsPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.AdMarkers
import Amazonka.MediaPackage.Types.AdTriggersElement
import Amazonka.MediaPackage.Types.AdsOnDeliveryRestrictions
import Amazonka.MediaPackage.Types.HlsEncryption
import Amazonka.MediaPackage.Types.PlaylistType
import Amazonka.MediaPackage.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | An HTTP Live Streaming (HLS) packaging configuration.
--
-- /See:/ 'newHlsPackage' smart constructor.
data HlsPackage = HlsPackage'
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
    adTriggers :: Prelude.Maybe [AdTriggersElement],
    adsOnDeliveryRestrictions :: Prelude.Maybe AdsOnDeliveryRestrictions,
    encryption :: Prelude.Maybe HlsEncryption,
    -- | When enabled, MediaPackage passes through digital video broadcasting
    -- (DVB) subtitles into the output.
    includeDvbSubtitles :: Prelude.Maybe Prelude.Bool,
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Prelude.Maybe Prelude.Bool,
    -- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
    -- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
    -- included in the media playlist.
    playlistType :: Prelude.Maybe PlaylistType,
    -- | Time window (in seconds) contained in each parent manifest.
    playlistWindowSeconds :: Prelude.Maybe Prelude.Int,
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
    -- | Duration (in seconds) of each fragment. Actual fragments will be rounded
    -- to the nearest multiple of the source fragment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    streamSelection :: Prelude.Maybe StreamSelection,
    -- | When enabled, audio streams will be placed in rendition groups in the
    -- output.
    useAudioRenditionGroup :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adMarkers', 'hlsPackage_adMarkers' - This setting controls how ad markers are included in the packaged
-- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
-- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
-- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
-- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
-- blackout tags based on SCTE-35 messages in the input source.
-- \"DATERANGE\" inserts EXT-X-DATERANGE tags to signal ad and program
-- transition events in HLS and CMAF manifests. For this option, you must
-- set a programDateTimeIntervalSeconds value that is greater than 0.
--
-- 'adTriggers', 'hlsPackage_adTriggers' - Undocumented member.
--
-- 'adsOnDeliveryRestrictions', 'hlsPackage_adsOnDeliveryRestrictions' - Undocumented member.
--
-- 'encryption', 'hlsPackage_encryption' - Undocumented member.
--
-- 'includeDvbSubtitles', 'hlsPackage_includeDvbSubtitles' - When enabled, MediaPackage passes through digital video broadcasting
-- (DVB) subtitles into the output.
--
-- 'includeIframeOnlyStream', 'hlsPackage_includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- 'playlistType', 'hlsPackage_playlistType' - The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
--
-- 'playlistWindowSeconds', 'hlsPackage_playlistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
--
-- 'programDateTimeIntervalSeconds', 'hlsPackage_programDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content. If the interval is not specified, or set to
-- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
-- and no ID3Timed Metadata messages will be generated. Note that
-- irrespective of this parameter, if any ID3 Timed Metadata is found in
-- HTTP Live Streaming (HLS) input, it will be passed through to HLS
-- output.
--
-- 'segmentDurationSeconds', 'hlsPackage_segmentDurationSeconds' - Duration (in seconds) of each fragment. Actual fragments will be rounded
-- to the nearest multiple of the source fragment duration.
--
-- 'streamSelection', 'hlsPackage_streamSelection' - Undocumented member.
--
-- 'useAudioRenditionGroup', 'hlsPackage_useAudioRenditionGroup' - When enabled, audio streams will be placed in rendition groups in the
-- output.
newHlsPackage ::
  HlsPackage
newHlsPackage =
  HlsPackage'
    { adMarkers = Prelude.Nothing,
      adTriggers = Prelude.Nothing,
      adsOnDeliveryRestrictions = Prelude.Nothing,
      encryption = Prelude.Nothing,
      includeDvbSubtitles = Prelude.Nothing,
      includeIframeOnlyStream = Prelude.Nothing,
      playlistType = Prelude.Nothing,
      playlistWindowSeconds = Prelude.Nothing,
      programDateTimeIntervalSeconds = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      streamSelection = Prelude.Nothing,
      useAudioRenditionGroup = Prelude.Nothing
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
hlsPackage_adMarkers :: Lens.Lens' HlsPackage (Prelude.Maybe AdMarkers)
hlsPackage_adMarkers = Lens.lens (\HlsPackage' {adMarkers} -> adMarkers) (\s@HlsPackage' {} a -> s {adMarkers = a} :: HlsPackage)

-- | Undocumented member.
hlsPackage_adTriggers :: Lens.Lens' HlsPackage (Prelude.Maybe [AdTriggersElement])
hlsPackage_adTriggers = Lens.lens (\HlsPackage' {adTriggers} -> adTriggers) (\s@HlsPackage' {} a -> s {adTriggers = a} :: HlsPackage) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
hlsPackage_adsOnDeliveryRestrictions :: Lens.Lens' HlsPackage (Prelude.Maybe AdsOnDeliveryRestrictions)
hlsPackage_adsOnDeliveryRestrictions = Lens.lens (\HlsPackage' {adsOnDeliveryRestrictions} -> adsOnDeliveryRestrictions) (\s@HlsPackage' {} a -> s {adsOnDeliveryRestrictions = a} :: HlsPackage)

-- | Undocumented member.
hlsPackage_encryption :: Lens.Lens' HlsPackage (Prelude.Maybe HlsEncryption)
hlsPackage_encryption = Lens.lens (\HlsPackage' {encryption} -> encryption) (\s@HlsPackage' {} a -> s {encryption = a} :: HlsPackage)

-- | When enabled, MediaPackage passes through digital video broadcasting
-- (DVB) subtitles into the output.
hlsPackage_includeDvbSubtitles :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Bool)
hlsPackage_includeDvbSubtitles = Lens.lens (\HlsPackage' {includeDvbSubtitles} -> includeDvbSubtitles) (\s@HlsPackage' {} a -> s {includeDvbSubtitles = a} :: HlsPackage)

-- | When enabled, an I-Frame only stream will be included in the output.
hlsPackage_includeIframeOnlyStream :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Bool)
hlsPackage_includeIframeOnlyStream = Lens.lens (\HlsPackage' {includeIframeOnlyStream} -> includeIframeOnlyStream) (\s@HlsPackage' {} a -> s {includeIframeOnlyStream = a} :: HlsPackage)

-- | The HTTP Live Streaming (HLS) playlist type. When either \"EVENT\" or
-- \"VOD\" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be
-- included in the media playlist.
hlsPackage_playlistType :: Lens.Lens' HlsPackage (Prelude.Maybe PlaylistType)
hlsPackage_playlistType = Lens.lens (\HlsPackage' {playlistType} -> playlistType) (\s@HlsPackage' {} a -> s {playlistType = a} :: HlsPackage)

-- | Time window (in seconds) contained in each parent manifest.
hlsPackage_playlistWindowSeconds :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Int)
hlsPackage_playlistWindowSeconds = Lens.lens (\HlsPackage' {playlistWindowSeconds} -> playlistWindowSeconds) (\s@HlsPackage' {} a -> s {playlistWindowSeconds = a} :: HlsPackage)

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag
-- inserted into manifests. Additionally, when an interval is specified
-- ID3Timed Metadata messages will be generated every 5 seconds using the
-- ingest time of the content. If the interval is not specified, or set to
-- 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests
-- and no ID3Timed Metadata messages will be generated. Note that
-- irrespective of this parameter, if any ID3 Timed Metadata is found in
-- HTTP Live Streaming (HLS) input, it will be passed through to HLS
-- output.
hlsPackage_programDateTimeIntervalSeconds :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Int)
hlsPackage_programDateTimeIntervalSeconds = Lens.lens (\HlsPackage' {programDateTimeIntervalSeconds} -> programDateTimeIntervalSeconds) (\s@HlsPackage' {} a -> s {programDateTimeIntervalSeconds = a} :: HlsPackage)

-- | Duration (in seconds) of each fragment. Actual fragments will be rounded
-- to the nearest multiple of the source fragment duration.
hlsPackage_segmentDurationSeconds :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Int)
hlsPackage_segmentDurationSeconds = Lens.lens (\HlsPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@HlsPackage' {} a -> s {segmentDurationSeconds = a} :: HlsPackage)

-- | Undocumented member.
hlsPackage_streamSelection :: Lens.Lens' HlsPackage (Prelude.Maybe StreamSelection)
hlsPackage_streamSelection = Lens.lens (\HlsPackage' {streamSelection} -> streamSelection) (\s@HlsPackage' {} a -> s {streamSelection = a} :: HlsPackage)

-- | When enabled, audio streams will be placed in rendition groups in the
-- output.
hlsPackage_useAudioRenditionGroup :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Bool)
hlsPackage_useAudioRenditionGroup = Lens.lens (\HlsPackage' {useAudioRenditionGroup} -> useAudioRenditionGroup) (\s@HlsPackage' {} a -> s {useAudioRenditionGroup = a} :: HlsPackage)

instance Data.FromJSON HlsPackage where
  parseJSON =
    Data.withObject
      "HlsPackage"
      ( \x ->
          HlsPackage'
            Prelude.<$> (x Data..:? "adMarkers")
            Prelude.<*> (x Data..:? "adTriggers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "adsOnDeliveryRestrictions")
            Prelude.<*> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "includeDvbSubtitles")
            Prelude.<*> (x Data..:? "includeIframeOnlyStream")
            Prelude.<*> (x Data..:? "playlistType")
            Prelude.<*> (x Data..:? "playlistWindowSeconds")
            Prelude.<*> (x Data..:? "programDateTimeIntervalSeconds")
            Prelude.<*> (x Data..:? "segmentDurationSeconds")
            Prelude.<*> (x Data..:? "streamSelection")
            Prelude.<*> (x Data..:? "useAudioRenditionGroup")
      )

instance Prelude.Hashable HlsPackage where
  hashWithSalt _salt HlsPackage' {..} =
    _salt
      `Prelude.hashWithSalt` adMarkers
      `Prelude.hashWithSalt` adTriggers
      `Prelude.hashWithSalt` adsOnDeliveryRestrictions
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` includeDvbSubtitles
      `Prelude.hashWithSalt` includeIframeOnlyStream
      `Prelude.hashWithSalt` playlistType
      `Prelude.hashWithSalt` playlistWindowSeconds
      `Prelude.hashWithSalt` programDateTimeIntervalSeconds
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` streamSelection
      `Prelude.hashWithSalt` useAudioRenditionGroup

instance Prelude.NFData HlsPackage where
  rnf HlsPackage' {..} =
    Prelude.rnf adMarkers
      `Prelude.seq` Prelude.rnf adTriggers
      `Prelude.seq` Prelude.rnf adsOnDeliveryRestrictions
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf includeDvbSubtitles
      `Prelude.seq` Prelude.rnf includeIframeOnlyStream
      `Prelude.seq` Prelude.rnf playlistType
      `Prelude.seq` Prelude.rnf playlistWindowSeconds
      `Prelude.seq` Prelude.rnf programDateTimeIntervalSeconds
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf streamSelection
      `Prelude.seq` Prelude.rnf useAudioRenditionGroup

instance Data.ToJSON HlsPackage where
  toJSON HlsPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adMarkers" Data..=) Prelude.<$> adMarkers,
            ("adTriggers" Data..=) Prelude.<$> adTriggers,
            ("adsOnDeliveryRestrictions" Data..=)
              Prelude.<$> adsOnDeliveryRestrictions,
            ("encryption" Data..=) Prelude.<$> encryption,
            ("includeDvbSubtitles" Data..=)
              Prelude.<$> includeDvbSubtitles,
            ("includeIframeOnlyStream" Data..=)
              Prelude.<$> includeIframeOnlyStream,
            ("playlistType" Data..=) Prelude.<$> playlistType,
            ("playlistWindowSeconds" Data..=)
              Prelude.<$> playlistWindowSeconds,
            ("programDateTimeIntervalSeconds" Data..=)
              Prelude.<$> programDateTimeIntervalSeconds,
            ("segmentDurationSeconds" Data..=)
              Prelude.<$> segmentDurationSeconds,
            ("streamSelection" Data..=)
              Prelude.<$> streamSelection,
            ("useAudioRenditionGroup" Data..=)
              Prelude.<$> useAudioRenditionGroup
          ]
      )
