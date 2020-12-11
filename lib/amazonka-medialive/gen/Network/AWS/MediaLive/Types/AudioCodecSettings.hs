-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioCodecSettings
  ( AudioCodecSettings (..),

    -- * Smart constructor
    mkAudioCodecSettings,

    -- * Lenses
    acsPassThroughSettings,
    acsAc3Settings,
    acsMp2Settings,
    acsWavSettings,
    acsAacSettings,
    acsEac3Settings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AacSettings
import Network.AWS.MediaLive.Types.Ac3Settings
import Network.AWS.MediaLive.Types.Eac3Settings
import Network.AWS.MediaLive.Types.Mp2Settings
import Network.AWS.MediaLive.Types.PassThroughSettings
import Network.AWS.MediaLive.Types.WavSettings
import qualified Network.AWS.Prelude as Lude

-- | Audio Codec Settings
--
-- /See:/ 'mkAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { passThroughSettings ::
      Lude.Maybe PassThroughSettings,
    ac3Settings :: Lude.Maybe Ac3Settings,
    mp2Settings :: Lude.Maybe Mp2Settings,
    wavSettings :: Lude.Maybe WavSettings,
    aacSettings :: Lude.Maybe AacSettings,
    eac3Settings :: Lude.Maybe Eac3Settings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioCodecSettings' with the minimum fields required to make a request.
--
-- * 'aacSettings' - Undocumented field.
-- * 'ac3Settings' - Undocumented field.
-- * 'eac3Settings' - Undocumented field.
-- * 'mp2Settings' - Undocumented field.
-- * 'passThroughSettings' - Undocumented field.
-- * 'wavSettings' - Undocumented field.
mkAudioCodecSettings ::
  AudioCodecSettings
mkAudioCodecSettings =
  AudioCodecSettings'
    { passThroughSettings = Lude.Nothing,
      ac3Settings = Lude.Nothing,
      mp2Settings = Lude.Nothing,
      wavSettings = Lude.Nothing,
      aacSettings = Lude.Nothing,
      eac3Settings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'passThroughSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsPassThroughSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe PassThroughSettings)
acsPassThroughSettings = Lens.lens (passThroughSettings :: AudioCodecSettings -> Lude.Maybe PassThroughSettings) (\s a -> s {passThroughSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsPassThroughSettings "Use generic-lens or generic-optics with 'passThroughSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAc3Settings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Ac3Settings)
acsAc3Settings = Lens.lens (ac3Settings :: AudioCodecSettings -> Lude.Maybe Ac3Settings) (\s a -> s {ac3Settings = a} :: AudioCodecSettings)
{-# DEPRECATED acsAc3Settings "Use generic-lens or generic-optics with 'ac3Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mp2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsMp2Settings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Mp2Settings)
acsMp2Settings = Lens.lens (mp2Settings :: AudioCodecSettings -> Lude.Maybe Mp2Settings) (\s a -> s {mp2Settings = a} :: AudioCodecSettings)
{-# DEPRECATED acsMp2Settings "Use generic-lens or generic-optics with 'mp2Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'wavSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsWavSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe WavSettings)
acsWavSettings = Lens.lens (wavSettings :: AudioCodecSettings -> Lude.Maybe WavSettings) (\s a -> s {wavSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsWavSettings "Use generic-lens or generic-optics with 'wavSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aacSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAacSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe AacSettings)
acsAacSettings = Lens.lens (aacSettings :: AudioCodecSettings -> Lude.Maybe AacSettings) (\s a -> s {aacSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsAacSettings "Use generic-lens or generic-optics with 'aacSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsEac3Settings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Eac3Settings)
acsEac3Settings = Lens.lens (eac3Settings :: AudioCodecSettings -> Lude.Maybe Eac3Settings) (\s a -> s {eac3Settings = a} :: AudioCodecSettings)
{-# DEPRECATED acsEac3Settings "Use generic-lens or generic-optics with 'eac3Settings' instead." #-}

instance Lude.FromJSON AudioCodecSettings where
  parseJSON =
    Lude.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Lude.<$> (x Lude..:? "passThroughSettings")
            Lude.<*> (x Lude..:? "ac3Settings")
            Lude.<*> (x Lude..:? "mp2Settings")
            Lude.<*> (x Lude..:? "wavSettings")
            Lude.<*> (x Lude..:? "aacSettings")
            Lude.<*> (x Lude..:? "eac3Settings")
      )

instance Lude.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("passThroughSettings" Lude..=) Lude.<$> passThroughSettings,
            ("ac3Settings" Lude..=) Lude.<$> ac3Settings,
            ("mp2Settings" Lude..=) Lude.<$> mp2Settings,
            ("wavSettings" Lude..=) Lude.<$> wavSettings,
            ("aacSettings" Lude..=) Lude.<$> aacSettings,
            ("eac3Settings" Lude..=) Lude.<$> eac3Settings
          ]
      )
