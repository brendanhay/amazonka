{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
  ( AudioCodecOptions (..),

    -- * Smart constructor
    mkAudioCodecOptions,

    -- * Lenses
    acoSigned,
    acoBitDepth,
    acoProfile,
    acoBitOrder,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options associated with your audio codec.
--
-- /See:/ 'mkAudioCodecOptions' smart constructor.
data AudioCodecOptions = AudioCodecOptions'
  { signed ::
      Lude.Maybe Lude.Text,
    bitDepth :: Lude.Maybe Lude.Text,
    profile :: Lude.Maybe Lude.Text,
    bitOrder :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioCodecOptions' with the minimum fields required to make a request.
--
-- * 'bitDepth' - You can only choose an audio bit depth when you specify @flac@ or @pcm@ for the value of Audio:Codec.
--
-- The bit depth of a sample is how many bits of information are included in the audio samples. The higher the bit depth, the better the audio, but the larger the file.
-- Valid values are @16@ and @24@ .
-- The most common bit depth is @24@ .
-- * 'bitOrder' - You can only choose an audio bit order when you specify @pcm@ for the value of Audio:Codec.
--
-- The order the bits of a PCM sample are stored in.
-- The supported value is @LittleEndian@ .
-- * 'profile' - You can only choose an audio profile when you specify AAC for the value of Audio:Codec.
--
-- Specify the AAC profile for the output file. Elastic Transcoder supports the following profiles:
--
--     * @auto@ : If you specify @auto@ , Elastic Transcoder selects the profile based on the bit rate selected for the output file.
--
--
--     * @AAC-LC@ : The most common AAC profile. Use for bit rates larger than 64 kbps.
--
--
--     * @HE-AAC@ : Not supported on some older players and devices. Use for bit rates between 40 and 80 kbps.
--
--
--     * @HE-AACv2@ : Not supported on some players and devices. Use for bit rates less than 48 kbps.
--
--
-- All outputs in a @Smooth@ playlist must have the same value for @Profile@ .
-- * 'signed' - You can only choose whether an audio sample is signed when you specify @pcm@ for the value of Audio:Codec.
--
-- Whether audio samples are represented with negative and positive numbers (signed) or only positive numbers (unsigned).
-- The supported value is @Signed@ .
mkAudioCodecOptions ::
  AudioCodecOptions
mkAudioCodecOptions =
  AudioCodecOptions'
    { signed = Lude.Nothing,
      bitDepth = Lude.Nothing,
      profile = Lude.Nothing,
      bitOrder = Lude.Nothing
    }

-- | You can only choose whether an audio sample is signed when you specify @pcm@ for the value of Audio:Codec.
--
-- Whether audio samples are represented with negative and positive numbers (signed) or only positive numbers (unsigned).
-- The supported value is @Signed@ .
--
-- /Note:/ Consider using 'signed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoSigned :: Lens.Lens' AudioCodecOptions (Lude.Maybe Lude.Text)
acoSigned = Lens.lens (signed :: AudioCodecOptions -> Lude.Maybe Lude.Text) (\s a -> s {signed = a} :: AudioCodecOptions)
{-# DEPRECATED acoSigned "Use generic-lens or generic-optics with 'signed' instead." #-}

-- | You can only choose an audio bit depth when you specify @flac@ or @pcm@ for the value of Audio:Codec.
--
-- The bit depth of a sample is how many bits of information are included in the audio samples. The higher the bit depth, the better the audio, but the larger the file.
-- Valid values are @16@ and @24@ .
-- The most common bit depth is @24@ .
--
-- /Note:/ Consider using 'bitDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoBitDepth :: Lens.Lens' AudioCodecOptions (Lude.Maybe Lude.Text)
acoBitDepth = Lens.lens (bitDepth :: AudioCodecOptions -> Lude.Maybe Lude.Text) (\s a -> s {bitDepth = a} :: AudioCodecOptions)
{-# DEPRECATED acoBitDepth "Use generic-lens or generic-optics with 'bitDepth' instead." #-}

-- | You can only choose an audio profile when you specify AAC for the value of Audio:Codec.
--
-- Specify the AAC profile for the output file. Elastic Transcoder supports the following profiles:
--
--     * @auto@ : If you specify @auto@ , Elastic Transcoder selects the profile based on the bit rate selected for the output file.
--
--
--     * @AAC-LC@ : The most common AAC profile. Use for bit rates larger than 64 kbps.
--
--
--     * @HE-AAC@ : Not supported on some older players and devices. Use for bit rates between 40 and 80 kbps.
--
--
--     * @HE-AACv2@ : Not supported on some players and devices. Use for bit rates less than 48 kbps.
--
--
-- All outputs in a @Smooth@ playlist must have the same value for @Profile@ .
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoProfile :: Lens.Lens' AudioCodecOptions (Lude.Maybe Lude.Text)
acoProfile = Lens.lens (profile :: AudioCodecOptions -> Lude.Maybe Lude.Text) (\s a -> s {profile = a} :: AudioCodecOptions)
{-# DEPRECATED acoProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | You can only choose an audio bit order when you specify @pcm@ for the value of Audio:Codec.
--
-- The order the bits of a PCM sample are stored in.
-- The supported value is @LittleEndian@ .
--
-- /Note:/ Consider using 'bitOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoBitOrder :: Lens.Lens' AudioCodecOptions (Lude.Maybe Lude.Text)
acoBitOrder = Lens.lens (bitOrder :: AudioCodecOptions -> Lude.Maybe Lude.Text) (\s a -> s {bitOrder = a} :: AudioCodecOptions)
{-# DEPRECATED acoBitOrder "Use generic-lens or generic-optics with 'bitOrder' instead." #-}

instance Lude.FromJSON AudioCodecOptions where
  parseJSON =
    Lude.withObject
      "AudioCodecOptions"
      ( \x ->
          AudioCodecOptions'
            Lude.<$> (x Lude..:? "Signed")
            Lude.<*> (x Lude..:? "BitDepth")
            Lude.<*> (x Lude..:? "Profile")
            Lude.<*> (x Lude..:? "BitOrder")
      )

instance Lude.ToJSON AudioCodecOptions where
  toJSON AudioCodecOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Signed" Lude..=) Lude.<$> signed,
            ("BitDepth" Lude..=) Lude.<$> bitDepth,
            ("Profile" Lude..=) Lude.<$> profile,
            ("BitOrder" Lude..=) Lude.<$> bitOrder
          ]
      )
