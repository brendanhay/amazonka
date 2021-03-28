{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
  ( CaptionDescriptionPreset (..)
  -- * Smart constructor
  , mkCaptionDescriptionPreset
  -- * Lenses
  , cdpCustomLanguageCode
  , cdpDestinationSettings
  , cdpLanguageCode
  , cdpLanguageDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CaptionDestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.LanguageCode as Types
import qualified Network.AWS.Prelude as Core

-- | Caption Description for preset
--
-- /See:/ 'mkCaptionDescriptionPreset' smart constructor.
data CaptionDescriptionPreset = CaptionDescriptionPreset'
  { customLanguageCode :: Core.Maybe Core.Text
    -- ^ Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
  , destinationSettings :: Core.Maybe Types.CaptionDestinationSettings
    -- ^ Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
  , languageDescription :: Core.Maybe Core.Text
    -- ^ Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionDescriptionPreset' value with any optional fields omitted.
mkCaptionDescriptionPreset
    :: CaptionDescriptionPreset
mkCaptionDescriptionPreset
  = CaptionDescriptionPreset'{customLanguageCode = Core.Nothing,
                              destinationSettings = Core.Nothing, languageCode = Core.Nothing,
                              languageDescription = Core.Nothing}

-- | Specify the language for this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information when automatically selecting the font script for rendering the captions text. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpCustomLanguageCode :: Lens.Lens' CaptionDescriptionPreset (Core.Maybe Core.Text)
cdpCustomLanguageCode = Lens.field @"customLanguageCode"
{-# INLINEABLE cdpCustomLanguageCode #-}
{-# DEPRECATED customLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead"  #-}

-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpDestinationSettings :: Lens.Lens' CaptionDescriptionPreset (Core.Maybe Types.CaptionDestinationSettings)
cdpDestinationSettings = Lens.field @"destinationSettings"
{-# INLINEABLE cdpDestinationSettings #-}
{-# DEPRECATED destinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead"  #-}

-- | Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpLanguageCode :: Lens.Lens' CaptionDescriptionPreset (Core.Maybe Types.LanguageCode)
cdpLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE cdpLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | Specify a label for this set of output captions. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpLanguageDescription :: Lens.Lens' CaptionDescriptionPreset (Core.Maybe Core.Text)
cdpLanguageDescription = Lens.field @"languageDescription"
{-# INLINEABLE cdpLanguageDescription #-}
{-# DEPRECATED languageDescription "Use generic-lens or generic-optics with 'languageDescription' instead"  #-}

instance Core.FromJSON CaptionDescriptionPreset where
        toJSON CaptionDescriptionPreset{..}
          = Core.object
              (Core.catMaybes
                 [("customLanguageCode" Core..=) Core.<$> customLanguageCode,
                  ("destinationSettings" Core..=) Core.<$> destinationSettings,
                  ("languageCode" Core..=) Core.<$> languageCode,
                  ("languageDescription" Core..=) Core.<$> languageDescription])

instance Core.FromJSON CaptionDescriptionPreset where
        parseJSON
          = Core.withObject "CaptionDescriptionPreset" Core.$
              \ x ->
                CaptionDescriptionPreset' Core.<$>
                  (x Core..:? "customLanguageCode") Core.<*>
                    x Core..:? "destinationSettings"
                    Core.<*> x Core..:? "languageCode"
                    Core.<*> x Core..:? "languageDescription"
