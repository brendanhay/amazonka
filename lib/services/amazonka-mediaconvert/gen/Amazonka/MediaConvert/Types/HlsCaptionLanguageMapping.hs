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
-- Module      : Amazonka.MediaConvert.Types.HlsCaptionLanguageMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsCaptionLanguageMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.LanguageCode
import qualified Amazonka.Prelude as Prelude

-- | Caption Language Mapping
--
-- /See:/ 'newHlsCaptionLanguageMapping' smart constructor.
data HlsCaptionLanguageMapping = HlsCaptionLanguageMapping'
  { -- | Caption channel.
    captionChannel :: Prelude.Maybe Prelude.Int,
    -- | Specify the language for this captions channel, using the ISO 639-2 or
    -- ISO 639-3 three-letter language code
    customLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | Specify the language, using the ISO 639-2 three-letter code listed at
    -- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Caption language description.
    languageDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsCaptionLanguageMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captionChannel', 'hlsCaptionLanguageMapping_captionChannel' - Caption channel.
--
-- 'customLanguageCode', 'hlsCaptionLanguageMapping_customLanguageCode' - Specify the language for this captions channel, using the ISO 639-2 or
-- ISO 639-3 three-letter language code
--
-- 'languageCode', 'hlsCaptionLanguageMapping_languageCode' - Specify the language, using the ISO 639-2 three-letter code listed at
-- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
--
-- 'languageDescription', 'hlsCaptionLanguageMapping_languageDescription' - Caption language description.
newHlsCaptionLanguageMapping ::
  HlsCaptionLanguageMapping
newHlsCaptionLanguageMapping =
  HlsCaptionLanguageMapping'
    { captionChannel =
        Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      languageDescription = Prelude.Nothing
    }

-- | Caption channel.
hlsCaptionLanguageMapping_captionChannel :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe Prelude.Int)
hlsCaptionLanguageMapping_captionChannel = Lens.lens (\HlsCaptionLanguageMapping' {captionChannel} -> captionChannel) (\s@HlsCaptionLanguageMapping' {} a -> s {captionChannel = a} :: HlsCaptionLanguageMapping)

-- | Specify the language for this captions channel, using the ISO 639-2 or
-- ISO 639-3 three-letter language code
hlsCaptionLanguageMapping_customLanguageCode :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe Prelude.Text)
hlsCaptionLanguageMapping_customLanguageCode = Lens.lens (\HlsCaptionLanguageMapping' {customLanguageCode} -> customLanguageCode) (\s@HlsCaptionLanguageMapping' {} a -> s {customLanguageCode = a} :: HlsCaptionLanguageMapping)

-- | Specify the language, using the ISO 639-2 three-letter code listed at
-- https:\/\/www.loc.gov\/standards\/iso639-2\/php\/code_list.php.
hlsCaptionLanguageMapping_languageCode :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe LanguageCode)
hlsCaptionLanguageMapping_languageCode = Lens.lens (\HlsCaptionLanguageMapping' {languageCode} -> languageCode) (\s@HlsCaptionLanguageMapping' {} a -> s {languageCode = a} :: HlsCaptionLanguageMapping)

-- | Caption language description.
hlsCaptionLanguageMapping_languageDescription :: Lens.Lens' HlsCaptionLanguageMapping (Prelude.Maybe Prelude.Text)
hlsCaptionLanguageMapping_languageDescription = Lens.lens (\HlsCaptionLanguageMapping' {languageDescription} -> languageDescription) (\s@HlsCaptionLanguageMapping' {} a -> s {languageDescription = a} :: HlsCaptionLanguageMapping)

instance Data.FromJSON HlsCaptionLanguageMapping where
  parseJSON =
    Data.withObject
      "HlsCaptionLanguageMapping"
      ( \x ->
          HlsCaptionLanguageMapping'
            Prelude.<$> (x Data..:? "captionChannel")
            Prelude.<*> (x Data..:? "customLanguageCode")
            Prelude.<*> (x Data..:? "languageCode")
            Prelude.<*> (x Data..:? "languageDescription")
      )

instance Prelude.Hashable HlsCaptionLanguageMapping where
  hashWithSalt _salt HlsCaptionLanguageMapping' {..} =
    _salt
      `Prelude.hashWithSalt` captionChannel
      `Prelude.hashWithSalt` customLanguageCode
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageDescription

instance Prelude.NFData HlsCaptionLanguageMapping where
  rnf HlsCaptionLanguageMapping' {..} =
    Prelude.rnf captionChannel
      `Prelude.seq` Prelude.rnf customLanguageCode
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf languageDescription

instance Data.ToJSON HlsCaptionLanguageMapping where
  toJSON HlsCaptionLanguageMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("captionChannel" Data..=)
              Prelude.<$> captionChannel,
            ("customLanguageCode" Data..=)
              Prelude.<$> customLanguageCode,
            ("languageCode" Data..=) Prelude.<$> languageCode,
            ("languageDescription" Data..=)
              Prelude.<$> languageDescription
          ]
      )
