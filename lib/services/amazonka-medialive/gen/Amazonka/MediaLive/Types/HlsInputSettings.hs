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
-- Module      : Amazonka.MediaLive.Types.HlsInputSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsInputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.HlsScte35SourceType
import qualified Amazonka.Prelude as Prelude

-- | Hls Input Settings
--
-- /See:/ 'newHlsInputSettings' smart constructor.
data HlsInputSettings = HlsInputSettings'
  { -- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely
    -- matches this value will be chosen, otherwise the highest bandwidth
    -- stream in the m3u8 will be chosen. The bitrate is specified in bits per
    -- second, as in an HLS manifest.
    bandwidth :: Prelude.Maybe Prelude.Natural,
    -- | When specified, reading of the HLS input will begin this many buffer
    -- segments from the end (most recently written segment). When not
    -- specified, the HLS input will begin with the first segment specified in
    -- the m3u8.
    bufferSegments :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive times that attempts to read a manifest or
    -- segment must fail before the input is considered unavailable.
    retries :: Prelude.Maybe Prelude.Natural,
    -- | The number of seconds between retries when an attempt to read a manifest
    -- or segment fails.
    retryInterval :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the source for the SCTE-35 messages that MediaLive will
    -- ingest. Messages can be ingested from the content segments (in the
    -- stream) or from tags in the playlist (the HLS manifest). MediaLive
    -- ignores SCTE-35 information in the source that is not selected.
    scte35Source :: Prelude.Maybe HlsScte35SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsInputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidth', 'hlsInputSettings_bandwidth' - When specified the HLS stream with the m3u8 BANDWIDTH that most closely
-- matches this value will be chosen, otherwise the highest bandwidth
-- stream in the m3u8 will be chosen. The bitrate is specified in bits per
-- second, as in an HLS manifest.
--
-- 'bufferSegments', 'hlsInputSettings_bufferSegments' - When specified, reading of the HLS input will begin this many buffer
-- segments from the end (most recently written segment). When not
-- specified, the HLS input will begin with the first segment specified in
-- the m3u8.
--
-- 'retries', 'hlsInputSettings_retries' - The number of consecutive times that attempts to read a manifest or
-- segment must fail before the input is considered unavailable.
--
-- 'retryInterval', 'hlsInputSettings_retryInterval' - The number of seconds between retries when an attempt to read a manifest
-- or segment fails.
--
-- 'scte35Source', 'hlsInputSettings_scte35Source' - Identifies the source for the SCTE-35 messages that MediaLive will
-- ingest. Messages can be ingested from the content segments (in the
-- stream) or from tags in the playlist (the HLS manifest). MediaLive
-- ignores SCTE-35 information in the source that is not selected.
newHlsInputSettings ::
  HlsInputSettings
newHlsInputSettings =
  HlsInputSettings'
    { bandwidth = Prelude.Nothing,
      bufferSegments = Prelude.Nothing,
      retries = Prelude.Nothing,
      retryInterval = Prelude.Nothing,
      scte35Source = Prelude.Nothing
    }

-- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely
-- matches this value will be chosen, otherwise the highest bandwidth
-- stream in the m3u8 will be chosen. The bitrate is specified in bits per
-- second, as in an HLS manifest.
hlsInputSettings_bandwidth :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_bandwidth = Lens.lens (\HlsInputSettings' {bandwidth} -> bandwidth) (\s@HlsInputSettings' {} a -> s {bandwidth = a} :: HlsInputSettings)

-- | When specified, reading of the HLS input will begin this many buffer
-- segments from the end (most recently written segment). When not
-- specified, the HLS input will begin with the first segment specified in
-- the m3u8.
hlsInputSettings_bufferSegments :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_bufferSegments = Lens.lens (\HlsInputSettings' {bufferSegments} -> bufferSegments) (\s@HlsInputSettings' {} a -> s {bufferSegments = a} :: HlsInputSettings)

-- | The number of consecutive times that attempts to read a manifest or
-- segment must fail before the input is considered unavailable.
hlsInputSettings_retries :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_retries = Lens.lens (\HlsInputSettings' {retries} -> retries) (\s@HlsInputSettings' {} a -> s {retries = a} :: HlsInputSettings)

-- | The number of seconds between retries when an attempt to read a manifest
-- or segment fails.
hlsInputSettings_retryInterval :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_retryInterval = Lens.lens (\HlsInputSettings' {retryInterval} -> retryInterval) (\s@HlsInputSettings' {} a -> s {retryInterval = a} :: HlsInputSettings)

-- | Identifies the source for the SCTE-35 messages that MediaLive will
-- ingest. Messages can be ingested from the content segments (in the
-- stream) or from tags in the playlist (the HLS manifest). MediaLive
-- ignores SCTE-35 information in the source that is not selected.
hlsInputSettings_scte35Source :: Lens.Lens' HlsInputSettings (Prelude.Maybe HlsScte35SourceType)
hlsInputSettings_scte35Source = Lens.lens (\HlsInputSettings' {scte35Source} -> scte35Source) (\s@HlsInputSettings' {} a -> s {scte35Source = a} :: HlsInputSettings)

instance Data.FromJSON HlsInputSettings where
  parseJSON =
    Data.withObject
      "HlsInputSettings"
      ( \x ->
          HlsInputSettings'
            Prelude.<$> (x Data..:? "bandwidth")
            Prelude.<*> (x Data..:? "bufferSegments")
            Prelude.<*> (x Data..:? "retries")
            Prelude.<*> (x Data..:? "retryInterval")
            Prelude.<*> (x Data..:? "scte35Source")
      )

instance Prelude.Hashable HlsInputSettings where
  hashWithSalt _salt HlsInputSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bandwidth
      `Prelude.hashWithSalt` bufferSegments
      `Prelude.hashWithSalt` retries
      `Prelude.hashWithSalt` retryInterval
      `Prelude.hashWithSalt` scte35Source

instance Prelude.NFData HlsInputSettings where
  rnf HlsInputSettings' {..} =
    Prelude.rnf bandwidth
      `Prelude.seq` Prelude.rnf bufferSegments
      `Prelude.seq` Prelude.rnf retries
      `Prelude.seq` Prelude.rnf retryInterval
      `Prelude.seq` Prelude.rnf scte35Source

instance Data.ToJSON HlsInputSettings where
  toJSON HlsInputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bandwidth" Data..=) Prelude.<$> bandwidth,
            ("bufferSegments" Data..=)
              Prelude.<$> bufferSegments,
            ("retries" Data..=) Prelude.<$> retries,
            ("retryInterval" Data..=) Prelude.<$> retryInterval,
            ("scte35Source" Data..=) Prelude.<$> scte35Source
          ]
      )
