{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
  ( HlsCaptionLanguageMapping (..),

    -- * Smart constructor
    mkHlsCaptionLanguageMapping,

    -- * Lenses
    hclmCaptionChannel,
    hclmCustomLanguageCode,
    hclmLanguageCode,
    hclmLanguageDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.LanguageCode as Types
import qualified Network.AWS.Prelude as Core

-- | Caption Language Mapping
--
-- /See:/ 'mkHlsCaptionLanguageMapping' smart constructor.
data HlsCaptionLanguageMapping = HlsCaptionLanguageMapping'
  { -- | Caption channel.
    captionChannel :: Core.Maybe Core.Int,
    -- | Specify the language for this captions channel, using the ISO 639-2 or ISO 639-3 three-letter language code
    customLanguageCode :: Core.Maybe Core.Text,
    -- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | Caption language description.
    languageDescription :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsCaptionLanguageMapping' value with any optional fields omitted.
mkHlsCaptionLanguageMapping ::
  HlsCaptionLanguageMapping
mkHlsCaptionLanguageMapping =
  HlsCaptionLanguageMapping'
    { captionChannel = Core.Nothing,
      customLanguageCode = Core.Nothing,
      languageCode = Core.Nothing,
      languageDescription = Core.Nothing
    }

-- | Caption channel.
--
-- /Note:/ Consider using 'captionChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmCaptionChannel :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe Core.Int)
hclmCaptionChannel = Lens.field @"captionChannel"
{-# DEPRECATED hclmCaptionChannel "Use generic-lens or generic-optics with 'captionChannel' instead." #-}

-- | Specify the language for this captions channel, using the ISO 639-2 or ISO 639-3 three-letter language code
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmCustomLanguageCode :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe Core.Text)
hclmCustomLanguageCode = Lens.field @"customLanguageCode"
{-# DEPRECATED hclmCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmLanguageCode :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe Types.LanguageCode)
hclmLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED hclmLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Caption language description.
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmLanguageDescription :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe Core.Text)
hclmLanguageDescription = Lens.field @"languageDescription"
{-# DEPRECATED hclmLanguageDescription "Use generic-lens or generic-optics with 'languageDescription' instead." #-}

instance Core.FromJSON HlsCaptionLanguageMapping where
  toJSON HlsCaptionLanguageMapping {..} =
    Core.object
      ( Core.catMaybes
          [ ("captionChannel" Core..=) Core.<$> captionChannel,
            ("customLanguageCode" Core..=) Core.<$> customLanguageCode,
            ("languageCode" Core..=) Core.<$> languageCode,
            ("languageDescription" Core..=) Core.<$> languageDescription
          ]
      )

instance Core.FromJSON HlsCaptionLanguageMapping where
  parseJSON =
    Core.withObject "HlsCaptionLanguageMapping" Core.$
      \x ->
        HlsCaptionLanguageMapping'
          Core.<$> (x Core..:? "captionChannel")
          Core.<*> (x Core..:? "customLanguageCode")
          Core.<*> (x Core..:? "languageCode")
          Core.<*> (x Core..:? "languageDescription")
