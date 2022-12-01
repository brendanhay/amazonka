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
-- Module      : Amazonka.MediaPackageVOD.Types.HlsManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.HlsManifest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageVOD.Types.AdMarkers
import Amazonka.MediaPackageVOD.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | An HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'newHlsManifest' smart constructor.
data HlsManifest = HlsManifest'
  { streamSelection :: Prelude.Maybe StreamSelection,
    -- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
    repeatExtXKey :: Prelude.Maybe Prelude.Bool,
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
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Prelude.Maybe Prelude.Bool,
    -- | This setting controls how ad markers are included in the packaged
    -- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
    -- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
    -- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
    -- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
    -- blackout tags based on SCTE-35 messages in the input source.
    adMarkers :: Prelude.Maybe AdMarkers,
    -- | An optional string to include in the name of the manifest.
    manifestName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamSelection', 'hlsManifest_streamSelection' - Undocumented member.
--
-- 'repeatExtXKey', 'hlsManifest_repeatExtXKey' - When enabled, the EXT-X-KEY tag will be repeated in output manifests.
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
-- 'includeIframeOnlyStream', 'hlsManifest_includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- 'adMarkers', 'hlsManifest_adMarkers' - This setting controls how ad markers are included in the packaged
-- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
-- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
-- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
-- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
-- blackout tags based on SCTE-35 messages in the input source.
--
-- 'manifestName', 'hlsManifest_manifestName' - An optional string to include in the name of the manifest.
newHlsManifest ::
  HlsManifest
newHlsManifest =
  HlsManifest'
    { streamSelection = Prelude.Nothing,
      repeatExtXKey = Prelude.Nothing,
      programDateTimeIntervalSeconds = Prelude.Nothing,
      includeIframeOnlyStream = Prelude.Nothing,
      adMarkers = Prelude.Nothing,
      manifestName = Prelude.Nothing
    }

-- | Undocumented member.
hlsManifest_streamSelection :: Lens.Lens' HlsManifest (Prelude.Maybe StreamSelection)
hlsManifest_streamSelection = Lens.lens (\HlsManifest' {streamSelection} -> streamSelection) (\s@HlsManifest' {} a -> s {streamSelection = a} :: HlsManifest)

-- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
hlsManifest_repeatExtXKey :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Bool)
hlsManifest_repeatExtXKey = Lens.lens (\HlsManifest' {repeatExtXKey} -> repeatExtXKey) (\s@HlsManifest' {} a -> s {repeatExtXKey = a} :: HlsManifest)

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

-- | When enabled, an I-Frame only stream will be included in the output.
hlsManifest_includeIframeOnlyStream :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Bool)
hlsManifest_includeIframeOnlyStream = Lens.lens (\HlsManifest' {includeIframeOnlyStream} -> includeIframeOnlyStream) (\s@HlsManifest' {} a -> s {includeIframeOnlyStream = a} :: HlsManifest)

-- | This setting controls how ad markers are included in the packaged
-- OriginEndpoint. \"NONE\" will omit all SCTE-35 ad markers from the
-- output. \"PASSTHROUGH\" causes the manifest to contain a copy of the
-- SCTE-35 ad markers (comments) taken directly from the input HTTP Live
-- Streaming (HLS) manifest. \"SCTE35_ENHANCED\" generates ad markers and
-- blackout tags based on SCTE-35 messages in the input source.
hlsManifest_adMarkers :: Lens.Lens' HlsManifest (Prelude.Maybe AdMarkers)
hlsManifest_adMarkers = Lens.lens (\HlsManifest' {adMarkers} -> adMarkers) (\s@HlsManifest' {} a -> s {adMarkers = a} :: HlsManifest)

-- | An optional string to include in the name of the manifest.
hlsManifest_manifestName :: Lens.Lens' HlsManifest (Prelude.Maybe Prelude.Text)
hlsManifest_manifestName = Lens.lens (\HlsManifest' {manifestName} -> manifestName) (\s@HlsManifest' {} a -> s {manifestName = a} :: HlsManifest)

instance Core.FromJSON HlsManifest where
  parseJSON =
    Core.withObject
      "HlsManifest"
      ( \x ->
          HlsManifest'
            Prelude.<$> (x Core..:? "streamSelection")
            Prelude.<*> (x Core..:? "repeatExtXKey")
            Prelude.<*> (x Core..:? "programDateTimeIntervalSeconds")
            Prelude.<*> (x Core..:? "includeIframeOnlyStream")
            Prelude.<*> (x Core..:? "adMarkers")
            Prelude.<*> (x Core..:? "manifestName")
      )

instance Prelude.Hashable HlsManifest where
  hashWithSalt _salt HlsManifest' {..} =
    _salt `Prelude.hashWithSalt` streamSelection
      `Prelude.hashWithSalt` repeatExtXKey
      `Prelude.hashWithSalt` programDateTimeIntervalSeconds
      `Prelude.hashWithSalt` includeIframeOnlyStream
      `Prelude.hashWithSalt` adMarkers
      `Prelude.hashWithSalt` manifestName

instance Prelude.NFData HlsManifest where
  rnf HlsManifest' {..} =
    Prelude.rnf streamSelection
      `Prelude.seq` Prelude.rnf repeatExtXKey
      `Prelude.seq` Prelude.rnf programDateTimeIntervalSeconds
      `Prelude.seq` Prelude.rnf includeIframeOnlyStream
      `Prelude.seq` Prelude.rnf adMarkers
      `Prelude.seq` Prelude.rnf manifestName

instance Core.ToJSON HlsManifest where
  toJSON HlsManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("streamSelection" Core..=)
              Prelude.<$> streamSelection,
            ("repeatExtXKey" Core..=) Prelude.<$> repeatExtXKey,
            ("programDateTimeIntervalSeconds" Core..=)
              Prelude.<$> programDateTimeIntervalSeconds,
            ("includeIframeOnlyStream" Core..=)
              Prelude.<$> includeIframeOnlyStream,
            ("adMarkers" Core..=) Prelude.<$> adMarkers,
            ("manifestName" Core..=) Prelude.<$> manifestName
          ]
      )
