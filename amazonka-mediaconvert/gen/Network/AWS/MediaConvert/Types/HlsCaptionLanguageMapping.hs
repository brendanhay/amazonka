{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.LanguageCode

-- | Caption Language Mapping
--
-- /See:/ 'newHlsCaptionLanguageMapping' smart constructor.
data HlsCaptionLanguageMapping = HlsCaptionLanguageMapping'
  { -- | Specify the language, using the ISO 639-2 three-letter code listed at
    -- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
    languageCode :: Core.Maybe LanguageCode,
    -- | Caption language description.
    languageDescription :: Core.Maybe Core.Text,
    -- | Specify the language for this captions channel, using the ISO 639-2 or
    -- ISO 639-3 three-letter language code
    customLanguageCode :: Core.Maybe Core.Text,
    -- | Caption channel.
    captionChannel :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsCaptionLanguageMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'hlsCaptionLanguageMapping_languageCode' - Specify the language, using the ISO 639-2 three-letter code listed at
-- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
--
-- 'languageDescription', 'hlsCaptionLanguageMapping_languageDescription' - Caption language description.
--
-- 'customLanguageCode', 'hlsCaptionLanguageMapping_customLanguageCode' - Specify the language for this captions channel, using the ISO 639-2 or
-- ISO 639-3 three-letter language code
--
-- 'captionChannel', 'hlsCaptionLanguageMapping_captionChannel' - Caption channel.
newHlsCaptionLanguageMapping ::
  HlsCaptionLanguageMapping
newHlsCaptionLanguageMapping =
  HlsCaptionLanguageMapping'
    { languageCode =
        Core.Nothing,
      languageDescription = Core.Nothing,
      customLanguageCode = Core.Nothing,
      captionChannel = Core.Nothing
    }

-- | Specify the language, using the ISO 639-2 three-letter code listed at
-- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
hlsCaptionLanguageMapping_languageCode :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe LanguageCode)
hlsCaptionLanguageMapping_languageCode = Lens.lens (\HlsCaptionLanguageMapping' {languageCode} -> languageCode) (\s@HlsCaptionLanguageMapping' {} a -> s {languageCode = a} :: HlsCaptionLanguageMapping)

-- | Caption language description.
hlsCaptionLanguageMapping_languageDescription :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe Core.Text)
hlsCaptionLanguageMapping_languageDescription = Lens.lens (\HlsCaptionLanguageMapping' {languageDescription} -> languageDescription) (\s@HlsCaptionLanguageMapping' {} a -> s {languageDescription = a} :: HlsCaptionLanguageMapping)

-- | Specify the language for this captions channel, using the ISO 639-2 or
-- ISO 639-3 three-letter language code
hlsCaptionLanguageMapping_customLanguageCode :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe Core.Text)
hlsCaptionLanguageMapping_customLanguageCode = Lens.lens (\HlsCaptionLanguageMapping' {customLanguageCode} -> customLanguageCode) (\s@HlsCaptionLanguageMapping' {} a -> s {customLanguageCode = a} :: HlsCaptionLanguageMapping)

-- | Caption channel.
hlsCaptionLanguageMapping_captionChannel :: Lens.Lens' HlsCaptionLanguageMapping (Core.Maybe Core.Int)
hlsCaptionLanguageMapping_captionChannel = Lens.lens (\HlsCaptionLanguageMapping' {captionChannel} -> captionChannel) (\s@HlsCaptionLanguageMapping' {} a -> s {captionChannel = a} :: HlsCaptionLanguageMapping)

instance Core.FromJSON HlsCaptionLanguageMapping where
  parseJSON =
    Core.withObject
      "HlsCaptionLanguageMapping"
      ( \x ->
          HlsCaptionLanguageMapping'
            Core.<$> (x Core..:? "languageCode")
            Core.<*> (x Core..:? "languageDescription")
            Core.<*> (x Core..:? "customLanguageCode")
            Core.<*> (x Core..:? "captionChannel")
      )

instance Core.Hashable HlsCaptionLanguageMapping

instance Core.NFData HlsCaptionLanguageMapping

instance Core.ToJSON HlsCaptionLanguageMapping where
  toJSON HlsCaptionLanguageMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ ("languageCode" Core..=) Core.<$> languageCode,
            ("languageDescription" Core..=)
              Core.<$> languageDescription,
            ("customLanguageCode" Core..=)
              Core.<$> customLanguageCode,
            ("captionChannel" Core..=) Core.<$> captionChannel
          ]
      )
