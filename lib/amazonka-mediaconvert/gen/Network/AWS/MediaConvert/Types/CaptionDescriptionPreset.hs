-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
  ( CaptionDescriptionPreset (..),

    -- * Smart constructor
    mkCaptionDescriptionPreset,

    -- * Lenses
    cdpCustomLanguageCode,
    cdpLanguageCode,
    cdpDestinationSettings,
    cdpLanguageDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CaptionDestinationSettings
import Network.AWS.MediaConvert.Types.LanguageCode
import qualified Network.AWS.Prelude as Lude

-- | Caption Description for preset
--
-- /See:/ 'mkCaptionDescriptionPreset' smart constructor.
data CaptionDescriptionPreset = CaptionDescriptionPreset'
  { customLanguageCode ::
      Lude.Maybe Lude.Text,
    languageCode :: Lude.Maybe LanguageCode,
    destinationSettings ::
      Lude.Maybe CaptionDestinationSettings,
    languageDescription ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionDescriptionPreset' with the minimum fields required to make a request.
--
-- * 'customLanguageCode' - Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
-- * 'destinationSettings' - Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
-- * 'languageCode' - Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
-- * 'languageDescription' - Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
mkCaptionDescriptionPreset ::
  CaptionDescriptionPreset
mkCaptionDescriptionPreset =
  CaptionDescriptionPreset'
    { customLanguageCode = Lude.Nothing,
      languageCode = Lude.Nothing,
      destinationSettings = Lude.Nothing,
      languageDescription = Lude.Nothing
    }

-- | Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpCustomLanguageCode :: Lens.Lens' CaptionDescriptionPreset (Lude.Maybe Lude.Text)
cdpCustomLanguageCode = Lens.lens (customLanguageCode :: CaptionDescriptionPreset -> Lude.Maybe Lude.Text) (\s a -> s {customLanguageCode = a} :: CaptionDescriptionPreset)
{-# DEPRECATED cdpCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpLanguageCode :: Lens.Lens' CaptionDescriptionPreset (Lude.Maybe LanguageCode)
cdpLanguageCode = Lens.lens (languageCode :: CaptionDescriptionPreset -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: CaptionDescriptionPreset)
{-# DEPRECATED cdpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpDestinationSettings :: Lens.Lens' CaptionDescriptionPreset (Lude.Maybe CaptionDestinationSettings)
cdpDestinationSettings = Lens.lens (destinationSettings :: CaptionDescriptionPreset -> Lude.Maybe CaptionDestinationSettings) (\s a -> s {destinationSettings = a} :: CaptionDescriptionPreset)
{-# DEPRECATED cdpDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpLanguageDescription :: Lens.Lens' CaptionDescriptionPreset (Lude.Maybe Lude.Text)
cdpLanguageDescription = Lens.lens (languageDescription :: CaptionDescriptionPreset -> Lude.Maybe Lude.Text) (\s a -> s {languageDescription = a} :: CaptionDescriptionPreset)
{-# DEPRECATED cdpLanguageDescription "Use generic-lens or generic-optics with 'languageDescription' instead." #-}

instance Lude.FromJSON CaptionDescriptionPreset where
  parseJSON =
    Lude.withObject
      "CaptionDescriptionPreset"
      ( \x ->
          CaptionDescriptionPreset'
            Lude.<$> (x Lude..:? "customLanguageCode")
            Lude.<*> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "destinationSettings")
            Lude.<*> (x Lude..:? "languageDescription")
      )

instance Lude.ToJSON CaptionDescriptionPreset where
  toJSON CaptionDescriptionPreset' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customLanguageCode" Lude..=) Lude.<$> customLanguageCode,
            ("languageCode" Lude..=) Lude.<$> languageCode,
            ("destinationSettings" Lude..=) Lude.<$> destinationSettings,
            ("languageDescription" Lude..=) Lude.<$> languageDescription
          ]
      )
