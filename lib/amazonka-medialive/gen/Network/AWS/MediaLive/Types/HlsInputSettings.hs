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
    hisBufferSegments,
    hisRetries,
    hisRetryInterval,
    hisBandwidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Hls Input Settings
--
-- /See:/ 'mkHlsInputSettings' smart constructor.
data HlsInputSettings = HlsInputSettings'
  { -- | When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
    bufferSegments :: Lude.Maybe Lude.Natural,
    -- | The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
    retries :: Lude.Maybe Lude.Natural,
    -- | The number of seconds between retries when an attempt to read a manifest or segment fails.
    retryInterval :: Lude.Maybe Lude.Natural,
    -- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
    bandwidth :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsInputSettings' with the minimum fields required to make a request.
--
-- * 'bufferSegments' - When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
-- * 'retries' - The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
-- * 'retryInterval' - The number of seconds between retries when an attempt to read a manifest or segment fails.
-- * 'bandwidth' - When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
mkHlsInputSettings ::
  HlsInputSettings
mkHlsInputSettings =
  HlsInputSettings'
    { bufferSegments = Lude.Nothing,
      retries = Lude.Nothing,
      retryInterval = Lude.Nothing,
      bandwidth = Lude.Nothing
    }

-- | When specified, reading of the HLS input will begin this many buffer segments from the end (most recently written segment).  When not specified, the HLS input will begin with the first segment specified in the m3u8.
--
-- /Note:/ Consider using 'bufferSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisBufferSegments :: Lens.Lens' HlsInputSettings (Lude.Maybe Lude.Natural)
hisBufferSegments = Lens.lens (bufferSegments :: HlsInputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bufferSegments = a} :: HlsInputSettings)
{-# DEPRECATED hisBufferSegments "Use generic-lens or generic-optics with 'bufferSegments' instead." #-}

-- | The number of consecutive times that attempts to read a manifest or segment must fail before the input is considered unavailable.
--
-- /Note:/ Consider using 'retries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisRetries :: Lens.Lens' HlsInputSettings (Lude.Maybe Lude.Natural)
hisRetries = Lens.lens (retries :: HlsInputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {retries = a} :: HlsInputSettings)
{-# DEPRECATED hisRetries "Use generic-lens or generic-optics with 'retries' instead." #-}

-- | The number of seconds between retries when an attempt to read a manifest or segment fails.
--
-- /Note:/ Consider using 'retryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisRetryInterval :: Lens.Lens' HlsInputSettings (Lude.Maybe Lude.Natural)
hisRetryInterval = Lens.lens (retryInterval :: HlsInputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {retryInterval = a} :: HlsInputSettings)
{-# DEPRECATED hisRetryInterval "Use generic-lens or generic-optics with 'retryInterval' instead." #-}

-- | When specified the HLS stream with the m3u8 BANDWIDTH that most closely matches this value will be chosen, otherwise the highest bandwidth stream in the m3u8 will be chosen.  The bitrate is specified in bits per second, as in an HLS manifest.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisBandwidth :: Lens.Lens' HlsInputSettings (Lude.Maybe Lude.Natural)
hisBandwidth = Lens.lens (bandwidth :: HlsInputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bandwidth = a} :: HlsInputSettings)
{-# DEPRECATED hisBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

instance Lude.FromJSON HlsInputSettings where
  parseJSON =
    Lude.withObject
      "HlsInputSettings"
      ( \x ->
          HlsInputSettings'
            Lude.<$> (x Lude..:? "bufferSegments")
            Lude.<*> (x Lude..:? "retries")
            Lude.<*> (x Lude..:? "retryInterval")
            Lude.<*> (x Lude..:? "bandwidth")
      )

instance Lude.ToJSON HlsInputSettings where
  toJSON HlsInputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bufferSegments" Lude..=) Lude.<$> bufferSegments,
            ("retries" Lude..=) Lude.<$> retries,
            ("retryInterval" Lude..=) Lude.<$> retryInterval,
            ("bandwidth" Lude..=) Lude.<$> bandwidth
          ]
      )
