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
import Network.AWS.MediaConvert.Types.CaptionSourceSettings
import Network.AWS.MediaConvert.Types.LanguageCode
import qualified Network.AWS.Prelude as Lude

-- | Set up captions in your outputs by first selecting them from your input here.
--
-- /See:/ 'mkCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { customLanguageCode ::
      Lude.Maybe Lude.Text,
    languageCode :: Lude.Maybe LanguageCode,
    sourceSettings :: Lude.Maybe CaptionSourceSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionSelector' with the minimum fields required to make a request.
--
-- * 'customLanguageCode' - The specific language to extract from source, using the ISO 639-2 or ISO 639-3 three-letter language code. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
-- * 'languageCode' - The specific language to extract from source. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
-- * 'sourceSettings' - If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml file, specify the URI of the input captions source file. If your input captions are IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
mkCaptionSelector ::
  CaptionSelector
mkCaptionSelector =
  CaptionSelector'
    { customLanguageCode = Lude.Nothing,
      languageCode = Lude.Nothing,
      sourceSettings = Lude.Nothing
    }

-- | The specific language to extract from source, using the ISO 639-2 or ISO 639-3 three-letter language code. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
--
-- /Note:/ Consider using 'customLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomLanguageCode :: Lens.Lens' CaptionSelector (Lude.Maybe Lude.Text)
csCustomLanguageCode = Lens.lens (customLanguageCode :: CaptionSelector -> Lude.Maybe Lude.Text) (\s a -> s {customLanguageCode = a} :: CaptionSelector)
{-# DEPRECATED csCustomLanguageCode "Use generic-lens or generic-optics with 'customLanguageCode' instead." #-}

-- | The specific language to extract from source. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLanguageCode :: Lens.Lens' CaptionSelector (Lude.Maybe LanguageCode)
csLanguageCode = Lens.lens (languageCode :: CaptionSelector -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: CaptionSelector)
{-# DEPRECATED csLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml file, specify the URI of the input captions source file. If your input captions are IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /Note:/ Consider using 'sourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceSettings :: Lens.Lens' CaptionSelector (Lude.Maybe CaptionSourceSettings)
csSourceSettings = Lens.lens (sourceSettings :: CaptionSelector -> Lude.Maybe CaptionSourceSettings) (\s a -> s {sourceSettings = a} :: CaptionSelector)
{-# DEPRECATED csSourceSettings "Use generic-lens or generic-optics with 'sourceSettings' instead." #-}

instance Lude.FromJSON CaptionSelector where
  parseJSON =
    Lude.withObject
      "CaptionSelector"
      ( \x ->
          CaptionSelector'
            Lude.<$> (x Lude..:? "customLanguageCode")
            Lude.<*> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "sourceSettings")
      )

instance Lude.ToJSON CaptionSelector where
  toJSON CaptionSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customLanguageCode" Lude..=) Lude.<$> customLanguageCode,
            ("languageCode" Lude..=) Lude.<$> languageCode,
            ("sourceSettings" Lude..=) Lude.<$> sourceSettings
          ]
      )
