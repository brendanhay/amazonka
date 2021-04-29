{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.LanguageCode
import qualified Network.AWS.Prelude as Prelude

-- | Caption Language Mapping
--
-- /See:/ 'newHlsCaptionLanguageMapping' smart constructor.
data HlsCaptionLanguageMapping = HlsCaptionLanguageMapping'
  { -- | Specify the language, using the ISO 639-2 three-letter code listed at
    -- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Caption language description.
    languageDescription :: Prelude.Maybe Prelude.Text,
    -- | Specify the language for this captions channel, using the ISO 639-2 or
    -- ISO 639-3 three-letter language code
    customLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | Caption channel.
    captionChannel :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      languageDescription = Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      captionChannel = Prelude.Nothing
    }

-- | Specify the language, using the ISO 639-2 three-letter code listed at
-- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
hlsCaptionLanguageMapping_languageCode :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe LanguageCode)
hlsCaptionLanguageMapping_languageCode = Lens.lens (\HlsCaptionLanguageMapping' {languageCode} -> languageCode) (\s@HlsCaptionLanguageMapping' {} a -> s {languageCode = a} :: HlsCaptionLanguageMapping)

-- | Caption language description.
hlsCaptionLanguageMapping_languageDescription :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe Prelude.Text)
hlsCaptionLanguageMapping_languageDescription = Lens.lens (\HlsCaptionLanguageMapping' {languageDescription} -> languageDescription) (\s@HlsCaptionLanguageMapping' {} a -> s {languageDescription = a} :: HlsCaptionLanguageMapping)

-- | Specify the language for this captions channel, using the ISO 639-2 or
-- ISO 639-3 three-letter language code
hlsCaptionLanguageMapping_customLanguageCode :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe Prelude.Text)
hlsCaptionLanguageMapping_customLanguageCode = Lens.lens (\HlsCaptionLanguageMapping' {customLanguageCode} -> customLanguageCode) (\s@HlsCaptionLanguageMapping' {} a -> s {customLanguageCode = a} :: HlsCaptionLanguageMapping)

-- | Caption channel.
hlsCaptionLanguageMapping_captionChannel :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe Prelude.Int)
hlsCaptionLanguageMapping_captionChannel = Lens.lens (\HlsCaptionLanguageMapping' {captionChannel} -> captionChannel) (\s@HlsCaptionLanguageMapping' {} a -> s {captionChannel = a} :: HlsCaptionLanguageMapping)

instance Prelude.FromJSON HlsCaptionLanguageMapping where
  parseJSON =
    Prelude.withObject
      "HlsCaptionLanguageMapping"
      ( \x ->
          HlsCaptionLanguageMapping'
            Prelude.<$> (x Prelude..:? "languageCode")
            Prelude.<*> (x Prelude..:? "languageDescription")
            Prelude.<*> (x Prelude..:? "customLanguageCode")
            Prelude.<*> (x Prelude..:? "captionChannel")
      )

instance Prelude.Hashable HlsCaptionLanguageMapping

instance Prelude.NFData HlsCaptionLanguageMapping

instance Prelude.ToJSON HlsCaptionLanguageMapping where
  toJSON HlsCaptionLanguageMapping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("languageCode" Prelude..=)
              Prelude.<$> languageCode,
            ("languageDescription" Prelude..=)
              Prelude.<$> languageDescription,
            ("customLanguageCode" Prelude..=)
              Prelude.<$> customLanguageCode,
            ("captionChannel" Prelude..=)
              Prelude.<$> captionChannel
          ]
      )
