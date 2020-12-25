{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsInputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsInputSettings
  ( HlsInputSettings (..),

    -- * Smart constructor
    mkHlsInputSettings,

    -- * Lenses
    hisBandwidth,
    hisBufferSegments,
    hisRetries,
    hisRetryInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Hls Input Settings
--
-- /See:/ 'mkHlsInputSettings' smart constructor.
data HlsInputSettings = HlsInputSettings'
  { -- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
    bandwidth :: Core.Maybe Core.Natural,
    -- | When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
    bufferSegments :: Core.Maybe Core.Natural,
    -- | The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
    retries :: Core.Maybe Core.Natural,
    -- | The number of seconds between retries when an attempt to read a manifest or segment fails.
    retryInterval :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsInputSettings' value with any optional fields omitted.
mkHlsInputSettings ::
  HlsInputSettings
mkHlsInputSettings =
  HlsInputSettings'
    { bandwidth = Core.Nothing,
      bufferSegments = Core.Nothing,
      retries = Core.Nothing,
      retryInterval = Core.Nothing
    }

-- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisBandwidth :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hisBandwidth = Lens.field @"bandwidth"
{-# DEPRECATED hisBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
--
-- /Note:/ Consider using 'bufferSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisBufferSegments :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hisBufferSegments = Lens.field @"bufferSegments"
{-# DEPRECATED hisBufferSegments "Use generic-lens or generic-optics with 'bufferSegments' instead." #-}

-- | The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
--
-- /Note:/ Consider using 'retries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisRetries :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hisRetries = Lens.field @"retries"
{-# DEPRECATED hisRetries "Use generic-lens or generic-optics with 'retries' instead." #-}

-- | The number of seconds between retries when an attempt to read a manifest or segment fails.
--
-- /Note:/ Consider using 'retryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisRetryInterval :: Lens.Lens' HlsInputSettings (Core.Maybe Core.Natural)
hisRetryInterval = Lens.field @"retryInterval"
{-# DEPRECATED hisRetryInterval "Use generic-lens or generic-optics with 'retryInterval' instead." #-}

instance Core.FromJSON HlsInputSettings where
  toJSON HlsInputSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bandwidth" Core..=) Core.<$> bandwidth,
            ("bufferSegments" Core..=) Core.<$> bufferSegments,
            ("retries" Core..=) Core.<$> retries,
            ("retryInterval" Core..=) Core.<$> retryInterval
          ]
      )

instance Core.FromJSON HlsInputSettings where
  parseJSON =
    Core.withObject "HlsInputSettings" Core.$
      \x ->
        HlsInputSettings'
          Core.<$> (x Core..:? "bandwidth")
          Core.<*> (x Core..:? "bufferSegments")
          Core.<*> (x Core..:? "retries")
          Core.<*> (x Core..:? "retryInterval")
