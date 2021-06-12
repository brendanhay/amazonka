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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Hls Input Settings
--
-- /See:/ 'newHlsInputSettings' smart constructor.
data HlsInputSettings = HlsInputSettings'
  { -- | The number of seconds between retries when an attempt to read a manifest
    -- or segment fails.
    retryInterval :: Core.Maybe Core.Natural,
    -- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely
    -- matches this value will be chosen, otherwise the highest bandwidth
    -- stream in the m3u8 will be chosen. The bitrate is specified in bits per
    -- second, as in an HLS manifest.
    bandwidth :: Core.Maybe Core.Natural,
    -- | The number of consecutive times that attempts to read a manifest or
    -- segment must fail before the input is considered unavailable.
    retries :: Core.Maybe Core.Natural,
    -- | When specified, reading of the HLS input will begin this many buffer
    -- segments from the end (most recently written segment). When not
    -- specified, the HLS input will begin with the first segment specified in
    -- the m3u8.
    bufferSegments :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { retryInterval = Core.Nothing,
      bandwidth = Core.Nothing,
      retries = Core.Nothing,
      bufferSegments = Core.Nothing
    }

-- | The number of seconds between retries when an attempt to read a manifest
-- or segment fails.
hlsInputSettings_retryInterval :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hlsInputSettings_retryInterval = Lens.lens (\HlsInputSettings' {retryInterval} -> retryInterval) (\s@HlsInputSettings' {} a -> s {retryInterval = a} :: HlsInputSettings)

-- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely
-- matches this value will be chosen, otherwise the highest bandwidth
-- stream in the m3u8 will be chosen. The bitrate is specified in bits per
-- second, as in an HLS manifest.
hlsInputSettings_bandwidth :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hlsInputSettings_bandwidth = Lens.lens (\HlsInputSettings' {bandwidth} -> bandwidth) (\s@HlsInputSettings' {} a -> s {bandwidth = a} :: HlsInputSettings)

-- | The number of consecutive times that attempts to read a manifest or
-- segment must fail before the input is considered unavailable.
hlsInputSettings_retries :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hlsInputSettings_retries = Lens.lens (\HlsInputSettings' {retries} -> retries) (\s@HlsInputSettings' {} a -> s {retries = a} :: HlsInputSettings)

-- | When specified, reading of the HLS input will begin this many buffer
-- segments from the end (most recently written segment). When not
-- specified, the HLS input will begin with the first segment specified in
-- the m3u8.
hlsInputSettings_bufferSegments :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hlsInputSettings_bufferSegments = Lens.lens (\HlsInputSettings' {bufferSegments} -> bufferSegments) (\s@HlsInputSettings' {} a -> s {bufferSegments = a} :: HlsInputSettings)

instance Core.FromJSON HlsInputSettings where
  parseJSON =
    Core.withObject
      "HlsInputSettings"
      ( \x ->
          HlsInputSettings'
            Core.<$> (x Core..:? "retryInterval")
            Core.<*> (x Core..:? "bandwidth")
            Core.<*> (x Core..:? "retries")
            Core.<*> (x Core..:? "bufferSegments")
      )

instance Core.Hashable HlsInputSettings

instance Core.NFData HlsInputSettings

instance Core.ToJSON HlsInputSettings where
  toJSON HlsInputSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("retryInterval" Core..=) Core.<$> retryInterval,
            ("bandwidth" Core..=) Core.<$> bandwidth,
            ("retries" Core..=) Core.<$> retries,
            ("bufferSegments" Core..=) Core.<$> bufferSegments
          ]
      )
