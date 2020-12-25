{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSelector
  ( CaptionSelector (..),

    -- * Smart constructor
    mkCaptionSelector,

    -- * Lenses
    csCustomLanguageCode,
    csLanguageCode,
    csSourceSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CaptionSourceSettings as Types
import qualified Network.AWS.MediaConvert.Types.LanguageCode as Types
import qualified Network.AWS.Prelude as Core

-- | Set up captions in your outputs by first selecting them from your input here.
--
-- /See:/ 'mkCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { -- | The specific language to extract from source, using the ISO 639-2 or ISO 639-3 three-letter language code. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
    customLanguageCode :: Core.Maybe Core.Text,
    -- | The specific language to extract from source. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml file, specify the URI of the input captions source file. If your input captions are IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
    sourceSettings :: Core.Maybe Types.CaptionSourceSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionSelector' value with any optional fields omitted.
mkCaptionSelector ::
  CaptionSelector
mkCaptionSelector =
  CaptionSelector'
    { customLanguageCode = Core.Nothing,
      languageCode = Core.Nothing,
      sourceSettings = Core.Nothing
    }

-- | The specific language to extract from source, using the ISO 639-2 or ISO 639-3 three-letter language code. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomLanguageCode :: Lens.Lens' CaptionSelector (Core.Maybe Core.Text)
csCustomLanguageCode = Lens.field @"customLanguageCode"
{-# DEPRECATED csCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | The specific language to extract from source. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLanguageCode :: Lens.Lens' CaptionSelector (Core.Maybe Types.LanguageCode)
csLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED csLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml file, specify the URI of the input captions source file. If your input captions are IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /Note:/ Consider using 'sourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceSettings :: Lens.Lens' CaptionSelector (Core.Maybe Types.CaptionSourceSettings)
csSourceSettings = Lens.field @"sourceSettings"
{-# DEPRECATED csSourceSettings "Use generic-lens or generic-optics with 'sourceSettings' instead." #-}

instance Core.FromJSON CaptionSelector where
  toJSON CaptionSelector {..} =
    Core.object
      ( Core.catMaybes
          [ ("customLanguageCode" Core..=) Core.<$> customLanguageCode,
            ("languageCode" Core..=) Core.<$> languageCode,
            ("sourceSettings" Core..=) Core.<$> sourceSettings
          ]
      )

instance Core.FromJSON CaptionSelector where
  parseJSON =
    Core.withObject "CaptionSelector" Core.$
      \x ->
        CaptionSelector'
          Core.<$> (x Core..:? "customLanguageCode")
          Core.<*> (x Core..:? "languageCode")
          Core.<*> (x Core..:? "sourceSettings")
