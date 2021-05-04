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
-- Module      : Network.AWS.MediaConvert.Types.CaptionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDescription where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
import Network.AWS.MediaConvert.Types.LanguageCode
import qualified Network.AWS.Prelude as Prelude

-- | Description of Caption output
--
-- /See:/ 'newCaptionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { -- | Specify the language of this captions output track. For most captions
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
    languageDescription :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies which \"Caption Selector\":#inputs-caption_selector to use
    -- from each input when generating captions. The name should be of the
    -- format \"Caption Selector \", which denotes that the Nth Caption
    -- Selector will be used from each input.
    captionSelectorName :: Prelude.Maybe Prelude.Text,
    -- | Specific settings required by destination type. Note that
    -- burnin_destination_settings are not available if the source of the
    -- caption data is Embedded or Teletext.
    destinationSettings :: Prelude.Maybe CaptionDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CaptionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'captionSelectorName', 'captionDescription_captionSelectorName' - Specifies which \"Caption Selector\":#inputs-caption_selector to use
-- from each input when generating captions. The name should be of the
-- format \"Caption Selector \", which denotes that the Nth Caption
-- Selector will be used from each input.
--
-- 'destinationSettings', 'captionDescription_destinationSettings' - Specific settings required by destination type. Note that
-- burnin_destination_settings are not available if the source of the
-- caption data is Embedded or Teletext.
newCaptionDescription ::
  CaptionDescription
newCaptionDescription =
  CaptionDescription'
    { languageCode = Prelude.Nothing,
      languageDescription = Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      captionSelectorName = Prelude.Nothing,
      destinationSettings = Prelude.Nothing
    }

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

-- | Specifies which \"Caption Selector\":#inputs-caption_selector to use
-- from each input when generating captions. The name should be of the
-- format \"Caption Selector \", which denotes that the Nth Caption
-- Selector will be used from each input.
captionDescription_captionSelectorName :: Lens.Lens' CaptionDescription (Prelude.Maybe Prelude.Text)
captionDescription_captionSelectorName = Lens.lens (\CaptionDescription' {captionSelectorName} -> captionSelectorName) (\s@CaptionDescription' {} a -> s {captionSelectorName = a} :: CaptionDescription)

-- | Specific settings required by destination type. Note that
-- burnin_destination_settings are not available if the source of the
-- caption data is Embedded or Teletext.
captionDescription_destinationSettings :: Lens.Lens' CaptionDescription (Prelude.Maybe CaptionDestinationSettings)
captionDescription_destinationSettings = Lens.lens (\CaptionDescription' {destinationSettings} -> destinationSettings) (\s@CaptionDescription' {} a -> s {destinationSettings = a} :: CaptionDescription)

instance Prelude.FromJSON CaptionDescription where
  parseJSON =
    Prelude.withObject
      "CaptionDescription"
      ( \x ->
          CaptionDescription'
            Prelude.<$> (x Prelude..:? "languageCode")
            Prelude.<*> (x Prelude..:? "languageDescription")
            Prelude.<*> (x Prelude..:? "customLanguageCode")
            Prelude.<*> (x Prelude..:? "captionSelectorName")
            Prelude.<*> (x Prelude..:? "destinationSettings")
      )

instance Prelude.Hashable CaptionDescription

instance Prelude.NFData CaptionDescription

instance Prelude.ToJSON CaptionDescription where
  toJSON CaptionDescription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("languageCode" Prelude..=)
              Prelude.<$> languageCode,
            ("languageDescription" Prelude..=)
              Prelude.<$> languageDescription,
            ("customLanguageCode" Prelude..=)
              Prelude.<$> customLanguageCode,
            ("captionSelectorName" Prelude..=)
              Prelude.<$> captionSelectorName,
            ("destinationSettings" Prelude..=)
              Prelude.<$> destinationSettings
          ]
      )
