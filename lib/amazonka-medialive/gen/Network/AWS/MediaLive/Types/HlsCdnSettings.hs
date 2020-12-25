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
    hcsHlsBasicPutSettings,
    hcsHlsMediaStoreSettings,
    hcsHlsWebdavSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.HlsAkamaiSettings as Types
import qualified Network.AWS.MediaLive.Types.HlsBasicPutSettings as Types
import qualified Network.AWS.MediaLive.Types.HlsMediaStoreSettings as Types
import qualified Network.AWS.MediaLive.Types.HlsWebdavSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Hls Cdn Settings
--
-- /See:/ 'mkHlsCdnSettings' smart constructor.
data HlsCdnSettings = HlsCdnSettings'
  { hlsAkamaiSettings :: Core.Maybe Types.HlsAkamaiSettings,
    hlsBasicPutSettings :: Core.Maybe Types.HlsBasicPutSettings,
    hlsMediaStoreSettings :: Core.Maybe Types.HlsMediaStoreSettings,
    hlsWebdavSettings :: Core.Maybe Types.HlsWebdavSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsCdnSettings' value with any optional fields omitted.
mkHlsCdnSettings ::
  HlsCdnSettings
mkHlsCdnSettings =
  HlsCdnSettings'
    { hlsAkamaiSettings = Core.Nothing,
      hlsBasicPutSettings = Core.Nothing,
      hlsMediaStoreSettings = Core.Nothing,
      hlsWebdavSettings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsAkamaiSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsAkamaiSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe Types.HlsAkamaiSettings)
hcsHlsAkamaiSettings = Lens.field @"hlsAkamaiSettings"
{-# DEPRECATED hcsHlsAkamaiSettings "Use generic-lens or generic-optics with 'hlsAkamaiSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsBasicPutSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsBasicPutSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe Types.HlsBasicPutSettings)
hcsHlsBasicPutSettings = Lens.field @"hlsBasicPutSettings"
{-# DEPRECATED hcsHlsBasicPutSettings "Use generic-lens or generic-optics with 'hlsBasicPutSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsMediaStoreSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsMediaStoreSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe Types.HlsMediaStoreSettings)
hcsHlsMediaStoreSettings = Lens.field @"hlsMediaStoreSettings"
{-# DEPRECATED hcsHlsMediaStoreSettings "Use generic-lens or generic-optics with 'hlsMediaStoreSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsWebdavSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcsHlsWebdavSettings :: Lens.Lens' HlsCdnSettings (Core.Maybe Types.HlsWebdavSettings)
hcsHlsWebdavSettings = Lens.field @"hlsWebdavSettings"
{-# DEPRECATED hcsHlsWebdavSettings "Use generic-lens or generic-optics with 'hlsWebdavSettings' instead." #-}

instance Core.FromJSON HlsCdnSettings where
  toJSON HlsCdnSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("hlsAkamaiSettings" Core..=) Core.<$> hlsAkamaiSettings,
            ("hlsBasicPutSettings" Core..=) Core.<$> hlsBasicPutSettings,
            ("hlsMediaStoreSettings" Core..=) Core.<$> hlsMediaStoreSettings,
            ("hlsWebdavSettings" Core..=) Core.<$> hlsWebdavSettings
          ]
      )

instance Core.FromJSON HlsCdnSettings where
  parseJSON =
    Core.withObject "HlsCdnSettings" Core.$
      \x ->
        HlsCdnSettings'
          Core.<$> (x Core..:? "hlsAkamaiSettings")
          Core.<*> (x Core..:? "hlsBasicPutSettings")
          Core.<*> (x Core..:? "hlsMediaStoreSettings")
          Core.<*> (x Core..:? "hlsWebdavSettings")
