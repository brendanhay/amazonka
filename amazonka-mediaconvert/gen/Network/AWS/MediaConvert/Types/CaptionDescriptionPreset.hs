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
-- Module      : Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDescriptionPreset where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
import Network.AWS.MediaConvert.Types.LanguageCode
import qualified Network.AWS.Prelude as Prelude

-- | Caption Description for preset
--
-- /See:/ 'newCaptionDescriptionPreset' smart constructor.
data CaptionDescriptionPreset = CaptionDescriptionPreset'
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
    -- | Specific settings required by destination type. Note that
    -- burnin_destination_settings are not available if the source of the
    -- caption data is Embedded or Teletext.
    destinationSettings :: Prelude.Maybe CaptionDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CaptionDescriptionPreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'captionDescriptionPreset_languageCode' - Specify the language of this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information to choose the font language
-- for rendering the captions text.
--
-- 'languageDescription', 'captionDescriptionPreset_languageDescription' - Specify a label for this set of output captions. For example,
-- \"English\", \"Director commentary\", or \"track_2\". For streaming
-- outputs, MediaConvert passes this information into destination manifests
-- for display on the end-viewer\'s player device. For outputs in other
-- output groups, the service ignores this setting.
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
-- 'destinationSettings', 'captionDescriptionPreset_destinationSettings' - Specific settings required by destination type. Note that
-- burnin_destination_settings are not available if the source of the
-- caption data is Embedded or Teletext.
newCaptionDescriptionPreset ::
  CaptionDescriptionPreset
newCaptionDescriptionPreset =
  CaptionDescriptionPreset'
    { languageCode =
        Prelude.Nothing,
      languageDescription = Prelude.Nothing,
      customLanguageCode = Prelude.Nothing,
      destinationSettings = Prelude.Nothing
    }

-- | Specify the language of this captions output track. For most captions
-- output formats, the encoder puts this language information in the output
-- captions metadata. If your output captions format is DVB-Sub or Burn in,
-- the encoder uses this language information to choose the font language
-- for rendering the captions text.
captionDescriptionPreset_languageCode :: Lens.Lens' CaptionDescriptionPreset (Prelude.Maybe LanguageCode)
captionDescriptionPreset_languageCode = Lens.lens (\CaptionDescriptionPreset' {languageCode} -> languageCode) (\s@CaptionDescriptionPreset' {} a -> s {languageCode = a} :: CaptionDescriptionPreset)

-- | Specify a label for this set of output captions. For example,
-- \"English\", \"Director commentary\", or \"track_2\". For streaming
-- outputs, MediaConvert passes this information into destination manifests
-- for display on the end-viewer\'s player device. For outputs in other
-- output groups, the service ignores this setting.
captionDescriptionPreset_languageDescription :: Lens.Lens' CaptionDescriptionPreset (Prelude.Maybe Prelude.Text)
captionDescriptionPreset_languageDescription = Lens.lens (\CaptionDescriptionPreset' {languageDescription} -> languageDescription) (\s@CaptionDescriptionPreset' {} a -> s {languageDescription = a} :: CaptionDescriptionPreset)

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

-- | Specific settings required by destination type. Note that
-- burnin_destination_settings are not available if the source of the
-- caption data is Embedded or Teletext.
captionDescriptionPreset_destinationSettings :: Lens.Lens' CaptionDescriptionPreset (Prelude.Maybe CaptionDestinationSettings)
captionDescriptionPreset_destinationSettings = Lens.lens (\CaptionDescriptionPreset' {destinationSettings} -> destinationSettings) (\s@CaptionDescriptionPreset' {} a -> s {destinationSettings = a} :: CaptionDescriptionPreset)

instance Prelude.FromJSON CaptionDescriptionPreset where
  parseJSON =
    Prelude.withObject
      "CaptionDescriptionPreset"
      ( \x ->
          CaptionDescriptionPreset'
            Prelude.<$> (x Prelude..:? "languageCode")
            Prelude.<*> (x Prelude..:? "languageDescription")
            Prelude.<*> (x Prelude..:? "customLanguageCode")
            Prelude.<*> (x Prelude..:? "destinationSettings")
      )

instance Prelude.Hashable CaptionDescriptionPreset

instance Prelude.NFData CaptionDescriptionPreset

instance Prelude.ToJSON CaptionDescriptionPreset where
  toJSON CaptionDescriptionPreset' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("languageCode" Prelude..=)
              Prelude.<$> languageCode,
            ("languageDescription" Prelude..=)
              Prelude.<$> languageDescription,
            ("customLanguageCode" Prelude..=)
              Prelude.<$> customLanguageCode,
            ("destinationSettings" Prelude..=)
              Prelude.<$> destinationSettings
          ]
      )
