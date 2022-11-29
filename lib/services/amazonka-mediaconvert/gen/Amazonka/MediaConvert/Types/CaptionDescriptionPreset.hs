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
-- Module      : Amazonka.MediaConvert.Types.CaptionDescriptionPreset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionDescriptionPreset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.CaptionDestinationSettings
import Amazonka.MediaConvert.Types.LanguageCode
import qualified Amazonka.Prelude as Prelude

-- | Caption Description for preset
--
-- /See:/ 'newCaptionDescriptionPreset' smart constructor.
data CaptionDescriptionPreset = CaptionDescriptionPreset'
  { -- | Specify the language for this captions output track. For most captions
    -- output formats, the encoder puts this language information in the output
    -- captions metadata. If your output captions format is DVB-Sub or Burn in,
    -- the encoder uses this language information when automatically selecting
    -- the font script for rendering the captions text. For all outputs, you
    -- can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can
    -- also use any other code in the full RFC-5646 specification. Streaming
    -- outputs are those that are in one of the following output groups: CMAF,
    -- DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
    customLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | Specify a label for this set of output captions. For example,
    -- \"English\", \"Director commentary\", or \"track_2\". For streaming
    -- outputs, MediaConvert passes this information into destination manifests
    -- for display on the end-viewer\'s player device. For outputs in other
    -- output groups, the service ignores this setting.
    languageDescription :: Prelude.Maybe Prelude.Text,
    -- | Specify the language of this captions output track. For most captions
    -- output formats, the encoder puts this language information in the output
    -- captions metadata. If your output captions format is DVB-Sub or Burn in,
    -- the encoder uses this language information to choose the font language
    -- for rendering the captions text.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Settings related to one captions tab on the MediaConvert console. In
    -- your job JSON, an instance of captions DestinationSettings is equivalent
    -- to one captions tab in the console. Usually, one captions tab
    -- corresponds to one output captions track. Depending on your output
    -- captions format, one tab might correspond to a set of output captions
    -- tracks. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/including-captions.html.
    destinationSettings :: Prelude.Maybe CaptionDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionDescriptionPreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLanguageCode', 'captionDescriptionPreset_customLanguageCode' - Specify the language for this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information when automatically selecting
-- the font script for rendering the captions text. For all outputs, you
-- can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can
-- also use any other code in the full RFC-5646 specification. Streaming
-- outputs are those that are in one of the following output groups: CMAF,
-- DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- 'languageDescription', 'captionDescriptionPreset_languageDescription' - Specify a label for this set of output captions. For example,
-- \"English\", \"Director commentary\", or \"track_2\". For streaming
-- outputs, MediaConvert passes this information into destination manifests
-- for display on the end-viewer\'s player device. For outputs in other
-- output groups, the service ignores this setting.
--
-- 'languageCode', 'captionDescriptionPreset_languageCode' - Specify the language of this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information to choose the font language
-- for rendering the captions text.
--
-- 'destinationSettings', 'captionDescriptionPreset_destinationSettings' - Settings related to one captions tab on the MediaConvert console. In
-- your job JSON, an instance of captions DestinationSettings is equivalent
-- to one captions tab in the console. Usually, one captions tab
-- corresponds to one output captions track. Depending on your output
-- captions format, one tab might correspond to a set of output captions
-- tracks. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/including-captions.html.
newCaptionDescriptionPreset ::
  CaptionDescriptionPreset
newCaptionDescriptionPreset =
  CaptionDescriptionPreset'
    { customLanguageCode =
        Prelude.Nothing,
      languageDescription = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      destinationSettings = Prelude.Nothing
    }

-- | Specify the language for this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information when automatically selecting
-- the font script for rendering the captions text. For all outputs, you
-- can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can
-- also use any other code in the full RFC-5646 specification. Streaming
-- outputs are those that are in one of the following output groups: CMAF,
-- DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
captionDescriptionPreset_customLanguageCode :: Lens.Lens' CaptionDescriptionPreset (Prelude.Maybe Prelude.Text)
captionDescriptionPreset_customLanguageCode = Lens.lens (\CaptionDescriptionPreset' {customLanguageCode} -> customLanguageCode) (\s@CaptionDescriptionPreset' {} a -> s {customLanguageCode = a} :: CaptionDescriptionPreset)

-- | Specify a label for this set of output captions. For example,
-- \"English\", \"Director commentary\", or \"track_2\". For streaming
-- outputs, MediaConvert passes this information into destination manifests
-- for display on the end-viewer\'s player device. For outputs in other
-- output groups, the service ignores this setting.
captionDescriptionPreset_languageDescription :: Lens.Lens' CaptionDescriptionPreset (Prelude.Maybe Prelude.Text)
captionDescriptionPreset_languageDescription = Lens.lens (\CaptionDescriptionPreset' {languageDescription} -> languageDescription) (\s@CaptionDescriptionPreset' {} a -> s {languageDescription = a} :: CaptionDescriptionPreset)

-- | Specify the language of this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information to choose the font language
-- for rendering the captions text.
captionDescriptionPreset_languageCode :: Lens.Lens' CaptionDescriptionPreset (Prelude.Maybe LanguageCode)
captionDescriptionPreset_languageCode = Lens.lens (\CaptionDescriptionPreset' {languageCode} -> languageCode) (\s@CaptionDescriptionPreset' {} a -> s {languageCode = a} :: CaptionDescriptionPreset)

-- | Settings related to one captions tab on the MediaConvert console. In
-- your job JSON, an instance of captions DestinationSettings is equivalent
-- to one captions tab in the console. Usually, one captions tab
-- corresponds to one output captions track. Depending on your output
-- captions format, one tab might correspond to a set of output captions
-- tracks. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/including-captions.html.
captionDescriptionPreset_destinationSettings :: Lens.Lens' CaptionDescriptionPreset (Prelude.Maybe CaptionDestinationSettings)
captionDescriptionPreset_destinationSettings = Lens.lens (\CaptionDescriptionPreset' {destinationSettings} -> destinationSettings) (\s@CaptionDescriptionPreset' {} a -> s {destinationSettings = a} :: CaptionDescriptionPreset)

instance Core.FromJSON CaptionDescriptionPreset where
  parseJSON =
    Core.withObject
      "CaptionDescriptionPreset"
      ( \x ->
          CaptionDescriptionPreset'
            Prelude.<$> (x Core..:? "customLanguageCode")
            Prelude.<*> (x Core..:? "languageDescription")
            Prelude.<*> (x Core..:? "languageCode")
            Prelude.<*> (x Core..:? "destinationSettings")
      )

instance Prelude.Hashable CaptionDescriptionPreset where
  hashWithSalt _salt CaptionDescriptionPreset' {..} =
    _salt `Prelude.hashWithSalt` customLanguageCode
      `Prelude.hashWithSalt` languageDescription
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` destinationSettings

instance Prelude.NFData CaptionDescriptionPreset where
  rnf CaptionDescriptionPreset' {..} =
    Prelude.rnf customLanguageCode
      `Prelude.seq` Prelude.rnf languageDescription
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf destinationSettings

instance Core.ToJSON CaptionDescriptionPreset where
  toJSON CaptionDescriptionPreset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("customLanguageCode" Core..=)
              Prelude.<$> customLanguageCode,
            ("languageDescription" Core..=)
              Prelude.<$> languageDescription,
            ("languageCode" Core..=) Prelude.<$> languageCode,
            ("destinationSettings" Core..=)
              Prelude.<$> destinationSettings
          ]
      )
