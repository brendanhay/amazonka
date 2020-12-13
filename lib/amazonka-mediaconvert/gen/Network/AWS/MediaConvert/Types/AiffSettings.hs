{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AiffSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AiffSettings
  ( AiffSettings (..),

    -- * Smart constructor
    mkAiffSettings,

    -- * Lenses
    asBitDepth,
    asChannels,
    asSampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- /See:/ 'mkAiffSettings' smart constructor.
data AiffSettings = AiffSettings'
  { -- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
    bitDepth :: Lude.Maybe Lude.Natural,
    -- | Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
    channels :: Lude.Maybe Lude.Natural,
    -- | Sample rate in hz.
    sampleRate :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AiffSettings' with the minimum fields required to make a request.
--
-- * 'bitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
-- * 'channels' - Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
-- * 'sampleRate' - Sample rate in hz.
mkAiffSettings ::
  AiffSettings
mkAiffSettings =
  AiffSettings'
    { bitDepth = Lude.Nothing,
      channels = Lude.Nothing,
      sampleRate = Lude.Nothing
    }

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- /Note:/ Consider using 'bitDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asBitDepth :: Lens.Lens' AiffSettings (Lude.Maybe Lude.Natural)
asBitDepth = Lens.lens (bitDepth :: AiffSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bitDepth = a} :: AiffSettings)
{-# DEPRECATED asBitDepth "Use generic-lens or generic-optics with 'bitDepth' instead." #-}

-- | Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asChannels :: Lens.Lens' AiffSettings (Lude.Maybe Lude.Natural)
asChannels = Lens.lens (channels :: AiffSettings -> Lude.Maybe Lude.Natural) (\s a -> s {channels = a} :: AiffSettings)
{-# DEPRECATED asChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Sample rate in hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSampleRate :: Lens.Lens' AiffSettings (Lude.Maybe Lude.Natural)
asSampleRate = Lens.lens (sampleRate :: AiffSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: AiffSettings)
{-# DEPRECATED asSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Lude.FromJSON AiffSettings where
  parseJSON =
    Lude.withObject
      "AiffSettings"
      ( \x ->
          AiffSettings'
            Lude.<$> (x Lude..:? "bitDepth")
            Lude.<*> (x Lude..:? "channels")
            Lude.<*> (x Lude..:? "sampleRate")
      )

instance Lude.ToJSON AiffSettings where
  toJSON AiffSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bitDepth" Lude..=) Lude.<$> bitDepth,
            ("channels" Lude..=) Lude.<$> channels,
            ("sampleRate" Lude..=) Lude.<$> sampleRate
          ]
      )
