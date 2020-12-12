{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsCdnSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCdnSettings
  ( HlsCdnSettings (..),

    -- * Smart constructor
    mkHlsCdnSettings,

    -- * Lenses
    hcsHlsAkamaiSettings,
    hcsHlsMediaStoreSettings,
    hcsHlsBasicPutSettings,
    hcsHlsWebdavSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsAkamaiSettings
import Network.AWS.MediaLive.Types.HlsBasicPutSettings
import Network.AWS.MediaLive.Types.HlsMediaStoreSettings
import Network.AWS.MediaLive.Types.HlsWebdavSettings
import qualified Network.AWS.Prelude as Lude

-- | Hls Cdn Settings
--
-- /See:/ 'mkHlsCdnSettings' smart constructor.
data HlsCdnSettings = HlsCdnSettings'
  { hlsAkamaiSettings ::
      Lude.Maybe HlsAkamaiSettings,
    hlsMediaStoreSettings :: Lude.Maybe HlsMediaStoreSettings,
    hlsBasicPutSettings :: Lude.Maybe HlsBasicPutSettings,
    hlsWebdavSettings :: Lude.Maybe HlsWebdavSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsCdnSettings' with the minimum fields required to make a request.
--
-- * 'hlsAkamaiSettings' - Undocumented field.
-- * 'hlsBasicPutSettings' - Undocumented field.
-- * 'hlsMediaStoreSettings' - Undocumented field.
-- * 'hlsWebdavSettings' - Undocumented field.
mkHlsCdnSettings ::
  HlsCdnSettings
mkHlsCdnSettings =
  HlsCdnSettings'
    { hlsAkamaiSettings = Lude.Nothing,
      hlsMediaStoreSettings = Lude.Nothing,
      hlsBasicPutSettings = Lude.Nothing,
      hlsWebdavSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsAkamaiSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsAkamaiSettings :: Lens.Lens' HlsCdnSettings (Lude.Maybe HlsAkamaiSettings)
hcsHlsAkamaiSettings = Lens.lens (hlsAkamaiSettings :: HlsCdnSettings -> Lude.Maybe HlsAkamaiSettings) (\s a -> s {hlsAkamaiSettings = a} :: HlsCdnSettings)
{-# DEPRECATED hcsHlsAkamaiSettings "Use generic-lens or generic-optics with 'hlsAkamaiSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsMediaStoreSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsMediaStoreSettings :: Lens.Lens' HlsCdnSettings (Lude.Maybe HlsMediaStoreSettings)
hcsHlsMediaStoreSettings = Lens.lens (hlsMediaStoreSettings :: HlsCdnSettings -> Lude.Maybe HlsMediaStoreSettings) (\s a -> s {hlsMediaStoreSettings = a} :: HlsCdnSettings)
{-# DEPRECATED hcsHlsMediaStoreSettings "Use generic-lens or generic-optics with 'hlsMediaStoreSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsBasicPutSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsBasicPutSettings :: Lens.Lens' HlsCdnSettings (Lude.Maybe HlsBasicPutSettings)
hcsHlsBasicPutSettings = Lens.lens (hlsBasicPutSettings :: HlsCdnSettings -> Lude.Maybe HlsBasicPutSettings) (\s a -> s {hlsBasicPutSettings = a} :: HlsCdnSettings)
{-# DEPRECATED hcsHlsBasicPutSettings "Use generic-lens or generic-optics with 'hlsBasicPutSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsWebdavSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsWebdavSettings :: Lens.Lens' HlsCdnSettings (Lude.Maybe HlsWebdavSettings)
hcsHlsWebdavSettings = Lens.lens (hlsWebdavSettings :: HlsCdnSettings -> Lude.Maybe HlsWebdavSettings) (\s a -> s {hlsWebdavSettings = a} :: HlsCdnSettings)
{-# DEPRECATED hcsHlsWebdavSettings "Use generic-lens or generic-optics with 'hlsWebdavSettings' instead." #-}

instance Lude.FromJSON HlsCdnSettings where
  parseJSON =
    Lude.withObject
      "HlsCdnSettings"
      ( \x ->
          HlsCdnSettings'
            Lude.<$> (x Lude..:? "hlsAkamaiSettings")
            Lude.<*> (x Lude..:? "hlsMediaStoreSettings")
            Lude.<*> (x Lude..:? "hlsBasicPutSettings")
            Lude.<*> (x Lude..:? "hlsWebdavSettings")
      )

instance Lude.ToJSON HlsCdnSettings where
  toJSON HlsCdnSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("hlsAkamaiSettings" Lude..=) Lude.<$> hlsAkamaiSettings,
            ("hlsMediaStoreSettings" Lude..=) Lude.<$> hlsMediaStoreSettings,
            ("hlsBasicPutSettings" Lude..=) Lude.<$> hlsBasicPutSettings,
            ("hlsWebdavSettings" Lude..=) Lude.<$> hlsWebdavSettings
          ]
      )
