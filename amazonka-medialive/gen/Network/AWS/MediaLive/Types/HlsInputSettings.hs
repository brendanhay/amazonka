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
-- Module      : Network.AWS.MediaLive.Types.HlsInputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsInputSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Hls Input Settings
--
-- /See:/ 'newHlsInputSettings' smart constructor.
data HlsInputSettings = HlsInputSettings'
  { -- | The number of seconds between retries when an attempt to read a manifest
    -- or segment fails.
    retryInterval :: Prelude.Maybe Prelude.Natural,
    -- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely
    -- matches this value will be chosen, otherwise the highest bandwidth
    -- stream in the m3u8 will be chosen. The bitrate is specified in bits per
    -- second, as in an HLS manifest.
    bandwidth :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive times that attempts to read a manifest or
    -- segment must fail before the input is considered unavailable.
    retries :: Prelude.Maybe Prelude.Natural,
    -- | When specified, reading of the HLS input will begin this many buffer
    -- segments from the end (most recently written segment). When not
    -- specified, the HLS input will begin with the first segment specified in
    -- the m3u8.
    bufferSegments :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HlsInputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retryInterval', 'hlsInputSettings_retryInterval' - The number of seconds between retries when an attempt to read a manifest
-- or segment fails.
--
-- 'bandwidth', 'hlsInputSettings_bandwidth' - When specified the HLS stream with the m3u8 BANDWIDTH that most closely
-- matches this value will be chosen, otherwise the highest bandwidth
-- stream in the m3u8 will be chosen. The bitrate is specified in bits per
-- second, as in an HLS manifest.
--
-- 'retries', 'hlsInputSettings_retries' - The number of consecutive times that attempts to read a manifest or
-- segment must fail before the input is considered unavailable.
--
-- 'bufferSegments', 'hlsInputSettings_bufferSegments' - When specified, reading of the HLS input will begin this many buffer
-- segments from the end (most recently written segment). When not
-- specified, the HLS input will begin with the first segment specified in
-- the m3u8.
newHlsInputSettings ::
  HlsInputSettings
newHlsInputSettings =
  HlsInputSettings'
    { retryInterval = Prelude.Nothing,
      bandwidth = Prelude.Nothing,
      retries = Prelude.Nothing,
      bufferSegments = Prelude.Nothing
    }

-- | The number of seconds between retries when an attempt to read a manifest
-- or segment fails.
hlsInputSettings_retryInterval :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_retryInterval = Lens.lens (\HlsInputSettings' {retryInterval} -> retryInterval) (\s@HlsInputSettings' {} a -> s {retryInterval = a} :: HlsInputSettings)

-- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely
-- matches this value will be chosen, otherwise the highest bandwidth
-- stream in the m3u8 will be chosen. The bitrate is specified in bits per
-- second, as in an HLS manifest.
hlsInputSettings_bandwidth :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_bandwidth = Lens.lens (\HlsInputSettings' {bandwidth} -> bandwidth) (\s@HlsInputSettings' {} a -> s {bandwidth = a} :: HlsInputSettings)

-- | The number of consecutive times that attempts to read a manifest or
-- segment must fail before the input is considered unavailable.
hlsInputSettings_retries :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_retries = Lens.lens (\HlsInputSettings' {retries} -> retries) (\s@HlsInputSettings' {} a -> s {retries = a} :: HlsInputSettings)

-- | When specified, reading of the HLS input will begin this many buffer
-- segments from the end (most recently written segment). When not
-- specified, the HLS input will begin with the first segment specified in
-- the m3u8.
hlsInputSettings_bufferSegments :: Lens.Lens' HlsInputSettings (Prelude.Maybe Prelude.Natural)
hlsInputSettings_bufferSegments = Lens.lens (\HlsInputSettings' {bufferSegments} -> bufferSegments) (\s@HlsInputSettings' {} a -> s {bufferSegments = a} :: HlsInputSettings)

instance Prelude.FromJSON HlsInputSettings where
  parseJSON =
    Prelude.withObject
      "HlsInputSettings"
      ( \x ->
          HlsInputSettings'
            Prelude.<$> (x Prelude..:? "retryInterval")
            Prelude.<*> (x Prelude..:? "bandwidth")
            Prelude.<*> (x Prelude..:? "retries")
            Prelude.<*> (x Prelude..:? "bufferSegments")
      )

instance Prelude.Hashable HlsInputSettings

instance Prelude.NFData HlsInputSettings

instance Prelude.ToJSON HlsInputSettings where
  toJSON HlsInputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("retryInterval" Prelude..=)
              Prelude.<$> retryInterval,
            ("bandwidth" Prelude..=) Prelude.<$> bandwidth,
            ("retries" Prelude..=) Prelude.<$> retries,
            ("bufferSegments" Prelude..=)
              Prelude.<$> bufferSegments
          ]
      )
