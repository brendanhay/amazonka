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
    hclmCustomLanguageCode,
    hclmLanguageCode,
    hclmLanguageDescription,
    hclmCaptionChannel,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.LanguageCode
import qualified Network.AWS.Prelude as Lude

-- | Caption Language Mapping
--
-- /See:/ 'mkHlsCaptionLanguageMapping' smart constructor.
data HlsCaptionLanguageMapping = HlsCaptionLanguageMapping'
  { -- | Specify the language for this captions channel, using the ISO 639-2 or ISO 639-3 three-letter language code
    customLanguageCode :: Lude.Maybe Lude.Text,
    -- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
    languageCode :: Lude.Maybe LanguageCode,
    -- | Caption language description.
    languageDescription :: Lude.Maybe Lude.Text,
    -- | Caption channel.
    captionChannel :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsCaptionLanguageMapping' with the minimum fields required to make a request.
--
-- * 'customLanguageCode' - Specify the language for this captions channel, using the ISO 639-2 or ISO 639-3 three-letter language code
-- * 'languageCode' - Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
-- * 'languageDescription' - Caption language description.
-- * 'captionChannel' - Caption channel.
mkHlsCaptionLanguageMapping ::
  HlsCaptionLanguageMapping
mkHlsCaptionLanguageMapping =
  HlsCaptionLanguageMapping'
    { customLanguageCode = Lude.Nothing,
      languageCode = Lude.Nothing,
      languageDescription = Lude.Nothing,
      captionChannel = Lude.Nothing
    }

-- | Specify the language for this captions channel, using the ISO 639-2 or ISO 639-3 three-letter language code
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmCustomLanguageCode :: Lens.Lens' HlsCaptionLanguageMapping (Lude.Maybe Lude.Text)
hclmCustomLanguageCode = Lens.lens (customLanguageCode :: HlsCaptionLanguageMapping -> Lude.Maybe Lude.Text) (\s a -> s {customLanguageCode = a} :: HlsCaptionLanguageMapping)
{-# DEPRECATED hclmCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmLanguageCode :: Lens.Lens' HlsCaptionLanguageMapping (Lude.Maybe LanguageCode)
hclmLanguageCode = Lens.lens (languageCode :: HlsCaptionLanguageMapping -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: HlsCaptionLanguageMapping)
{-# DEPRECATED hclmLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Caption language description.
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmLanguageDescription :: Lens.Lens' HlsCaptionLanguageMapping (Lude.Maybe Lude.Text)
hclmLanguageDescription = Lens.lens (languageDescription :: HlsCaptionLanguageMapping -> Lude.Maybe Lude.Text) (\s a -> s {languageDescription = a} :: HlsCaptionLanguageMapping)
{-# DEPRECATED hclmLanguageDescription "Use generic-lens or generic-optics with 'languageDescription' instead." #-}

-- | Caption channel.
--
-- /Note:/ Consider using 'captionChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hclmCaptionChannel :: Lens.Lens' HlsCaptionLanguageMapping (Lude.Maybe Lude.Int)
hclmCaptionChannel = Lens.lens (captionChannel :: HlsCaptionLanguageMapping -> Lude.Maybe Lude.Int) (\s a -> s {captionChannel = a} :: HlsCaptionLanguageMapping)
{-# DEPRECATED hclmCaptionChannel "Use generic-lens or generic-optics with 'captionChannel' instead." #-}

instance Lude.FromJSON HlsCaptionLanguageMapping where
  parseJSON =
    Lude.withObject
      "HlsCaptionLanguageMapping"
      ( \x ->
          HlsCaptionLanguageMapping'
            Lude.<$> (x Lude..:? "customLanguageCode")
            Lude.<*> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "languageDescription")
            Lude.<*> (x Lude..:? "captionChannel")
      )

instance Lude.ToJSON HlsCaptionLanguageMapping where
  toJSON HlsCaptionLanguageMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customLanguageCode" Lude..=) Lude.<$> customLanguageCode,
            ("languageCode" Lude..=) Lude.<$> languageCode,
            ("languageDescription" Lude..=) Lude.<$> languageDescription,
            ("captionChannel" Lude..=) Lude.<$> captionChannel
          ]
      )
