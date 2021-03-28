{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsSettings
  ( HlsSettings (..)
  -- * Smart constructor
  , mkHlsSettings
  -- * Lenses
  , hsAudioOnlyHlsSettings
  , hsFmp4HlsSettings
  , hsStandardHlsSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioOnlyHlsSettings as Types
import qualified Network.AWS.MediaLive.Types.Fmp4HlsSettings as Types
import qualified Network.AWS.MediaLive.Types.StandardHlsSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Hls Settings
--
-- /See:/ 'mkHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { audioOnlyHlsSettings :: Core.Maybe Types.AudioOnlyHlsSettings
  , fmp4HlsSettings :: Core.Maybe Types.Fmp4HlsSettings
  , standardHlsSettings :: Core.Maybe Types.StandardHlsSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsSettings' value with any optional fields omitted.
mkHlsSettings
    :: HlsSettings
mkHlsSettings
  = HlsSettings'{audioOnlyHlsSettings = Core.Nothing,
                 fmp4HlsSettings = Core.Nothing, standardHlsSettings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioOnlyHlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioOnlyHlsSettings :: Lens.Lens' HlsSettings (Core.Maybe Types.AudioOnlyHlsSettings)
hsAudioOnlyHlsSettings = Lens.field @"audioOnlyHlsSettings"
{-# INLINEABLE hsAudioOnlyHlsSettings #-}
{-# DEPRECATED audioOnlyHlsSettings "Use generic-lens or generic-optics with 'audioOnlyHlsSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fmp4HlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsFmp4HlsSettings :: Lens.Lens' HlsSettings (Core.Maybe Types.Fmp4HlsSettings)
hsFmp4HlsSettings = Lens.field @"fmp4HlsSettings"
{-# INLINEABLE hsFmp4HlsSettings #-}
{-# DEPRECATED fmp4HlsSettings "Use generic-lens or generic-optics with 'fmp4HlsSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'standardHlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsStandardHlsSettings :: Lens.Lens' HlsSettings (Core.Maybe Types.StandardHlsSettings)
hsStandardHlsSettings = Lens.field @"standardHlsSettings"
{-# INLINEABLE hsStandardHlsSettings #-}
{-# DEPRECATED standardHlsSettings "Use generic-lens or generic-optics with 'standardHlsSettings' instead"  #-}

instance Core.FromJSON HlsSettings where
        toJSON HlsSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioOnlyHlsSettings" Core..=) Core.<$> audioOnlyHlsSettings,
                  ("fmp4HlsSettings" Core..=) Core.<$> fmp4HlsSettings,
                  ("standardHlsSettings" Core..=) Core.<$> standardHlsSettings])

instance Core.FromJSON HlsSettings where
        parseJSON
          = Core.withObject "HlsSettings" Core.$
              \ x ->
                HlsSettings' Core.<$>
                  (x Core..:? "audioOnlyHlsSettings") Core.<*>
                    x Core..:? "fmp4HlsSettings"
                    Core.<*> x Core..:? "standardHlsSettings"
