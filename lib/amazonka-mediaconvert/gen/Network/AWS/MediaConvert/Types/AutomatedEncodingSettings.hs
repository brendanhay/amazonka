{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
  ( AutomatedEncodingSettings (..)
  -- * Smart constructor
  , mkAutomatedEncodingSettings
  -- * Lenses
  , aesAbrSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AutomatedAbrSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
--
-- /See:/ 'mkAutomatedEncodingSettings' smart constructor.
newtype AutomatedEncodingSettings = AutomatedEncodingSettings'
  { abrSettings :: Core.Maybe Types.AutomatedAbrSettings
    -- ^ Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AutomatedEncodingSettings' value with any optional fields omitted.
mkAutomatedEncodingSettings
    :: AutomatedEncodingSettings
mkAutomatedEncodingSettings
  = AutomatedEncodingSettings'{abrSettings = Core.Nothing}

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
--
-- /Note:/ Consider using 'abrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesAbrSettings :: Lens.Lens' AutomatedEncodingSettings (Core.Maybe Types.AutomatedAbrSettings)
aesAbrSettings = Lens.field @"abrSettings"
{-# INLINEABLE aesAbrSettings #-}
{-# DEPRECATED abrSettings "Use generic-lens or generic-optics with 'abrSettings' instead"  #-}

instance Core.FromJSON AutomatedEncodingSettings where
        toJSON AutomatedEncodingSettings{..}
          = Core.object
              (Core.catMaybes [("abrSettings" Core..=) Core.<$> abrSettings])

instance Core.FromJSON AutomatedEncodingSettings where
        parseJSON
          = Core.withObject "AutomatedEncodingSettings" Core.$
              \ x ->
                AutomatedEncodingSettings' Core.<$> (x Core..:? "abrSettings")
