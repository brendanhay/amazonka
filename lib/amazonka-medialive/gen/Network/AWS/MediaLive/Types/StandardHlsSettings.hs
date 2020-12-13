{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StandardHlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StandardHlsSettings
  ( StandardHlsSettings (..),

    -- * Smart constructor
    mkStandardHlsSettings,

    -- * Lenses
    shsAudioRenditionSets,
    shsM3u8Settings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.M3u8Settings
import qualified Network.AWS.Prelude as Lude

-- | Standard Hls Settings
--
-- /See:/ 'mkStandardHlsSettings' smart constructor.
data StandardHlsSettings = StandardHlsSettings'
  { -- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
    audioRenditionSets :: Lude.Maybe Lude.Text,
    m3u8Settings :: M3u8Settings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StandardHlsSettings' with the minimum fields required to make a request.
--
-- * 'audioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
-- * 'm3u8Settings' -
mkStandardHlsSettings ::
  -- | 'm3u8Settings'
  M3u8Settings ->
  StandardHlsSettings
mkStandardHlsSettings pM3u8Settings_ =
  StandardHlsSettings'
    { audioRenditionSets = Lude.Nothing,
      m3u8Settings = pM3u8Settings_
    }

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- /Note:/ Consider using 'audioRenditionSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shsAudioRenditionSets :: Lens.Lens' StandardHlsSettings (Lude.Maybe Lude.Text)
shsAudioRenditionSets = Lens.lens (audioRenditionSets :: StandardHlsSettings -> Lude.Maybe Lude.Text) (\s a -> s {audioRenditionSets = a} :: StandardHlsSettings)
{-# DEPRECATED shsAudioRenditionSets "Use generic-lens or generic-optics with 'audioRenditionSets' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'm3u8Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shsM3u8Settings :: Lens.Lens' StandardHlsSettings M3u8Settings
shsM3u8Settings = Lens.lens (m3u8Settings :: StandardHlsSettings -> M3u8Settings) (\s a -> s {m3u8Settings = a} :: StandardHlsSettings)
{-# DEPRECATED shsM3u8Settings "Use generic-lens or generic-optics with 'm3u8Settings' instead." #-}

instance Lude.FromJSON StandardHlsSettings where
  parseJSON =
    Lude.withObject
      "StandardHlsSettings"
      ( \x ->
          StandardHlsSettings'
            Lude.<$> (x Lude..:? "audioRenditionSets")
            Lude.<*> (x Lude..: "m3u8Settings")
      )

instance Lude.ToJSON StandardHlsSettings where
  toJSON StandardHlsSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("audioRenditionSets" Lude..=) Lude.<$> audioRenditionSets,
            Lude.Just ("m3u8Settings" Lude..= m3u8Settings)
          ]
      )
