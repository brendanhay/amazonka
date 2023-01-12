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
-- Module      : Amazonka.MediaConvert.Types.CaptionDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.CaptionDestinationSettings
import Amazonka.MediaConvert.Types.LanguageCode
import qualified Amazonka.Prelude as Prelude

-- | This object holds groups of settings related to captions for one output.
-- For each output that has captions, include one instance of
-- CaptionDescriptions.
--
-- /See:/ 'newCaptionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { -- | Specifies which \"Caption Selector\":#inputs-caption_selector to use
    -- from each input when generating captions. The name should be of the
    -- format \"Caption Selector \", which denotes that the Nth Caption
    -- Selector will be used from each input.
    captionSelectorName :: Prelude.Maybe Prelude.Text,
    -- | Specify the language for this captions output track. For most captions
    -- output formats, the encoder puts this language information in the output
    -- captions metadata. If your output captions format is DVB-Sub or Burn in,
    -- the encoder uses this language information when automatically selecting
    -- the font script for rendering the captions text. For all outputs, you
    -- can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can
    -- also use any other code in the full RFC-5646 specification. Streaming
    -- outputs are those that are in one of the following output groups: CMAF,
    -- DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
    customLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | Settings related to one captions tab on the MediaConvert console. In
    -- your job JSON, an instance of captions DestinationSettings is equivalent
    -- to one captions tab in the console. Usually, one captions tab
    -- corresponds to one output captions track. Depending on your output
    -- captions format, one tab might correspond to a set of output captions
    -- tracks. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/including-captions.html.
    destinationSettings :: Prelude.Maybe CaptionDestinationSettings,
    -- | Specify the language of this captions output track. For most captions
    -- output formats, the encoder puts this language information in the output
    -- captions metadata. If your output captions format is DVB-Sub or Burn in,
    -- the encoder uses this language information to choose the font language
    -- for rendering the captions text.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Specify a label for this set of output captions. For example,
    -- \"English\", \"Director commentary\", or \"track_2\". For streaming
    -- outputs, MediaConvert passes this information into destination manifests
    -- for display on the end-viewer\'s player device. For outputs in other
    -- output groups, the service ignores this setting.
    languageDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captionSelectorName', 'captionDescription_captionSelectorName' - Specifies which \"Caption Selector\":#inputs-caption_selector to use
-- from each input when generating captions. The name should be of the
-- format \"Caption Selector \", which denotes that the Nth Caption
-- Selector will be used from each input.
--
-- 'customLanguageCode', 'captionDescription_customLanguageCode' - Specify the language for this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information when automatically selecting
-- the font script for rendering the captions text. For all outputs, you
-- can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can
-- also use any other code in the full RFC-5646 specification. Streaming
-- outputs are those that are in one of the following output groups: CMAF,
-- DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- 'destinationSettings', 'captionDescription_destinationSettings' - Settings related to one captions tab on the MediaConvert console. In
-- your job JSON, an instance of captions DestinationSettings is equivalent
-- to one captions tab in the console. Usually, one captions tab
-- corresponds to one output captions track. Depending on your output
-- captions format, one tab might correspond to a set of output captions
-- tracks. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/including-captions.html.
--
-- 'languageCode', 'captionDescription_languageCode' - Specify the language of this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information to choose the font language
-- for rendering the captions text.
--
-- 'languageDescription', 'captionDescription_languageDescription' - Specify a label for this set of output captions. For example,
-- \"English\", \"Director commentary\", or \"track_2\". For streaming
-- outputs, MediaConvert passes this information into destination manifests
-- for display on the end-viewer\'s player device. For outputs in other
-- output groups, the service ignores this setting.
newCaptionDescription ::
  CaptionDescription
newCaptionDescription =
  CaptionDescription'
    { captionSelectorName =
        Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      languageDescription = Prelude.Nothing
    }

-- | Specifies which \"Caption Selector\":#inputs-caption_selector to use
-- from each input when generating captions. The name should be of the
-- format \"Caption Selector \", which denotes that the Nth Caption
-- Selector will be used from each input.
captionDescription_captionSelectorName :: Lens.Lens' CaptionDescription (Prelude.Maybe Prelude.Text)
captionDescription_captionSelectorName = Lens.lens (\CaptionDescription' {captionSelectorName} -> captionSelectorName) (\s@CaptionDescription' {} a -> s {captionSelectorName = a} :: CaptionDescription)

-- | Specify the language for this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information when automatically selecting
-- the font script for rendering the captions text. For all outputs, you
-- can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can
-- also use any other code in the full RFC-5646 specification. Streaming
-- outputs are those that are in one of the following output groups: CMAF,
-- DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
captionDescription_customLanguageCode :: Lens.Lens' CaptionDescription (Prelude.Maybe Prelude.Text)
captionDescription_customLanguageCode = Lens.lens (\CaptionDescription' {customLanguageCode} -> customLanguageCode) (\s@CaptionDescription' {} a -> s {customLanguageCode = a} :: CaptionDescription)

-- | Settings related to one captions tab on the MediaConvert console. In
-- your job JSON, an instance of captions DestinationSettings is equivalent
-- to one captions tab in the console. Usually, one captions tab
-- corresponds to one output captions track. Depending on your output
-- captions format, one tab might correspond to a set of output captions
-- tracks. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/including-captions.html.
captionDescription_destinationSettings :: Lens.Lens' CaptionDescription (Prelude.Maybe CaptionDestinationSettings)
captionDescription_destinationSettings = Lens.lens (\CaptionDescription' {destinationSettings} -> destinationSettings) (\s@CaptionDescription' {} a -> s {destinationSettings = a} :: CaptionDescription)

-- | Specify the language of this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information to choose the font language
-- for rendering the captions text.
captionDescription_languageCode :: Lens.Lens' CaptionDescription (Prelude.Maybe LanguageCode)
captionDescription_languageCode = Lens.lens (\CaptionDescription' {languageCode} -> languageCode) (\s@CaptionDescription' {} a -> s {languageCode = a} :: CaptionDescription)

-- | Specify a label for this set of output captions. For example,
-- \"English\", \"Director commentary\", or \"track_2\". For streaming
-- outputs, MediaConvert passes this information into destination manifests
-- for display on the end-viewer\'s player device. For outputs in other
-- output groups, the service ignores this setting.
captionDescription_languageDescription :: Lens.Lens' CaptionDescription (Prelude.Maybe Prelude.Text)
captionDescription_languageDescription = Lens.lens (\CaptionDescription' {languageDescription} -> languageDescription) (\s@CaptionDescription' {} a -> s {languageDescription = a} :: CaptionDescription)

instance Data.FromJSON CaptionDescription where
  parseJSON =
    Data.withObject
      "CaptionDescription"
      ( \x ->
          CaptionDescription'
            Prelude.<$> (x Data..:? "captionSelectorName")
            Prelude.<*> (x Data..:? "customLanguageCode")
            Prelude.<*> (x Data..:? "destinationSettings")
            Prelude.<*> (x Data..:? "languageCode")
            Prelude.<*> (x Data..:? "languageDescription")
      )

instance Prelude.Hashable CaptionDescription where
  hashWithSalt _salt CaptionDescription' {..} =
    _salt `Prelude.hashWithSalt` captionSelectorName
      `Prelude.hashWithSalt` customLanguageCode
      `Prelude.hashWithSalt` destinationSettings
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` languageDescription

instance Prelude.NFData CaptionDescription where
  rnf CaptionDescription' {..} =
    Prelude.rnf captionSelectorName
      `Prelude.seq` Prelude.rnf customLanguageCode
      `Prelude.seq` Prelude.rnf destinationSettings
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf languageDescription

instance Data.ToJSON CaptionDescription where
  toJSON CaptionDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("captionSelectorName" Data..=)
              Prelude.<$> captionSelectorName,
            ("customLanguageCode" Data..=)
              Prelude.<$> customLanguageCode,
            ("destinationSettings" Data..=)
              Prelude.<$> destinationSettings,
            ("languageCode" Data..=) Prelude.<$> languageCode,
            ("languageDescription" Data..=)
              Prelude.<$> languageDescription
          ]
      )
