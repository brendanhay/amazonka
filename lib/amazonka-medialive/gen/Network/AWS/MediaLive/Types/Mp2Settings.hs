{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mp2Settings
  ( Mp2Settings (..),

    -- * Smart constructor
    mkMp2Settings,

    -- * Lenses
    mCodingMode,
    mSampleRate,
    mBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Mp2CodingMode
import qualified Network.AWS.Prelude as Lude

-- | Mp2 Settings
--
-- /See:/ 'mkMp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { -- | The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
    codingMode :: Lude.Maybe Mp2CodingMode,
    -- | Sample rate in Hz.
    sampleRate :: Lude.Maybe Lude.Double,
    -- | Average bitrate in bits/second.
    bitrate :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- * 'codingMode' - The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
-- * 'sampleRate' - Sample rate in Hz.
-- * 'bitrate' - Average bitrate in bits/second.
mkMp2Settings ::
  Mp2Settings
mkMp2Settings =
  Mp2Settings'
    { codingMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      bitrate = Lude.Nothing
    }

-- | The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCodingMode :: Lens.Lens' Mp2Settings (Lude.Maybe Mp2CodingMode)
mCodingMode = Lens.lens (codingMode :: Mp2Settings -> Lude.Maybe Mp2CodingMode) (\s a -> s {codingMode = a} :: Mp2Settings)
{-# DEPRECATED mCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | Sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSampleRate :: Lens.Lens' Mp2Settings (Lude.Maybe Lude.Double)
mSampleRate = Lens.lens (sampleRate :: Mp2Settings -> Lude.Maybe Lude.Double) (\s a -> s {sampleRate = a} :: Mp2Settings)
{-# DEPRECATED mSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Average bitrate in bits/second.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBitrate :: Lens.Lens' Mp2Settings (Lude.Maybe Lude.Double)
mBitrate = Lens.lens (bitrate :: Mp2Settings -> Lude.Maybe Lude.Double) (\s a -> s {bitrate = a} :: Mp2Settings)
{-# DEPRECATED mBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

instance Lude.FromJSON Mp2Settings where
  parseJSON =
    Lude.withObject
      "Mp2Settings"
      ( \x ->
          Mp2Settings'
            Lude.<$> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "bitrate")
      )

instance Lude.ToJSON Mp2Settings where
  toJSON Mp2Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("codingMode" Lude..=) Lude.<$> codingMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("bitrate" Lude..=) Lude.<$> bitrate
          ]
      )
