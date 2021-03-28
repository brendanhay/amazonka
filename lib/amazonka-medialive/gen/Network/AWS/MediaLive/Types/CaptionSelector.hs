{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.CaptionSelector
  ( CaptionSelector (..)
  -- * Smart constructor
  , mkCaptionSelector
  -- * Lenses
  , csName
  , csLanguageCode
  , csSelectorSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.CaptionSelectorSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'mkCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { name :: Core.Text
    -- ^ Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
  , languageCode :: Core.Maybe Core.Text
    -- ^ When specified this field indicates the three letter language code of the caption track to extract from the source.
  , selectorSettings :: Core.Maybe Types.CaptionSelectorSettings
    -- ^ Caption selector settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionSelector' value with any optional fields omitted.
mkCaptionSelector
    :: Core.Text -- ^ 'name'
    -> CaptionSelector
mkCaptionSelector name
  = CaptionSelector'{name, languageCode = Core.Nothing,
                     selectorSettings = Core.Nothing}

-- | Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CaptionSelector Core.Text
csName = Lens.field @"name"
{-# INLINEABLE csName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | When specified this field indicates the three letter language code of the caption track to extract from the source.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLanguageCode :: Lens.Lens' CaptionSelector (Core.Maybe Core.Text)
csLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE csLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | Caption selector settings.
--
-- /Note:/ Consider using 'selectorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSelectorSettings :: Lens.Lens' CaptionSelector (Core.Maybe Types.CaptionSelectorSettings)
csSelectorSettings = Lens.field @"selectorSettings"
{-# INLINEABLE csSelectorSettings #-}
{-# DEPRECATED selectorSettings "Use generic-lens or generic-optics with 'selectorSettings' instead"  #-}

instance Core.FromJSON CaptionSelector where
        toJSON CaptionSelector{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("languageCode" Core..=) Core.<$> languageCode,
                  ("selectorSettings" Core..=) Core.<$> selectorSettings])

instance Core.FromJSON CaptionSelector where
        parseJSON
          = Core.withObject "CaptionSelector" Core.$
              \ x ->
                CaptionSelector' Core.<$>
                  (x Core..: "name") Core.<*> x Core..:? "languageCode" Core.<*>
                    x Core..:? "selectorSettings"
