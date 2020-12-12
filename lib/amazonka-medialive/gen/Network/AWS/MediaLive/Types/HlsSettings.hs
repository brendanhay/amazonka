{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsSettings
  ( HlsSettings (..),

    -- * Smart constructor
    mkHlsSettings,

    -- * Lenses
    hsFmp4HlsSettings,
    hsAudioOnlyHlsSettings,
    hsStandardHlsSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
import Network.AWS.MediaLive.Types.Fmp4HlsSettings
import Network.AWS.MediaLive.Types.StandardHlsSettings
import qualified Network.AWS.Prelude as Lude

-- | Hls Settings
--
-- /See:/ 'mkHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { fmp4HlsSettings ::
      Lude.Maybe Fmp4HlsSettings,
    audioOnlyHlsSettings :: Lude.Maybe AudioOnlyHlsSettings,
    standardHlsSettings :: Lude.Maybe StandardHlsSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsSettings' with the minimum fields required to make a request.
--
-- * 'audioOnlyHlsSettings' - Undocumented field.
-- * 'fmp4HlsSettings' - Undocumented field.
-- * 'standardHlsSettings' - Undocumented field.
mkHlsSettings ::
  HlsSettings
mkHlsSettings =
  HlsSettings'
    { fmp4HlsSettings = Lude.Nothing,
      audioOnlyHlsSettings = Lude.Nothing,
      standardHlsSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fmp4HlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFmp4HlsSettings :: Lens.Lens' HlsSettings (Lude.Maybe Fmp4HlsSettings)
hsFmp4HlsSettings = Lens.lens (fmp4HlsSettings :: HlsSettings -> Lude.Maybe Fmp4HlsSettings) (\s a -> s {fmp4HlsSettings = a} :: HlsSettings)
{-# DEPRECATED hsFmp4HlsSettings "Use generic-lens or generic-optics with 'fmp4HlsSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioOnlyHlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioOnlyHlsSettings :: Lens.Lens' HlsSettings (Lude.Maybe AudioOnlyHlsSettings)
hsAudioOnlyHlsSettings = Lens.lens (audioOnlyHlsSettings :: HlsSettings -> Lude.Maybe AudioOnlyHlsSettings) (\s a -> s {audioOnlyHlsSettings = a} :: HlsSettings)
{-# DEPRECATED hsAudioOnlyHlsSettings "Use generic-lens or generic-optics with 'audioOnlyHlsSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'standardHlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsStandardHlsSettings :: Lens.Lens' HlsSettings (Lude.Maybe StandardHlsSettings)
hsStandardHlsSettings = Lens.lens (standardHlsSettings :: HlsSettings -> Lude.Maybe StandardHlsSettings) (\s a -> s {standardHlsSettings = a} :: HlsSettings)
{-# DEPRECATED hsStandardHlsSettings "Use generic-lens or generic-optics with 'standardHlsSettings' instead." #-}

instance Lude.FromJSON HlsSettings where
  parseJSON =
    Lude.withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            Lude.<$> (x Lude..:? "fmp4HlsSettings")
            Lude.<*> (x Lude..:? "audioOnlyHlsSettings")
            Lude.<*> (x Lude..:? "standardHlsSettings")
      )

instance Lude.ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fmp4HlsSettings" Lude..=) Lude.<$> fmp4HlsSettings,
            ("audioOnlyHlsSettings" Lude..=) Lude.<$> audioOnlyHlsSettings,
            ("standardHlsSettings" Lude..=) Lude.<$> standardHlsSettings
          ]
      )
