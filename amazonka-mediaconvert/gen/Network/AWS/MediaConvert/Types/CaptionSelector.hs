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
-- Module      : Network.AWS.MediaConvert.Types.CaptionSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSelector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CaptionSourceSettings
import Network.AWS.MediaConvert.Types.LanguageCode

-- | Set up captions in your outputs by first selecting them from your input
-- here.
--
-- /See:/ 'newCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { -- | The specific language to extract from source. If input is SCTE-27,
    -- complete this field and\/or PID to select the caption language to
    -- extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete
    -- this field and\/or PID to select the caption language to extract. If
    -- input is DVB-Sub that is being passed through, omit this field (and PID
    -- field); there is no way to extract a specific language with pass-through
    -- captions.
    languageCode :: Core.Maybe LanguageCode,
    -- | The specific language to extract from source, using the ISO 639-2 or ISO
    -- 639-3 three-letter language code. If input is SCTE-27, complete this
    -- field and\/or PID to select the caption language to extract. If input is
    -- DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and\/or
    -- PID to select the caption language to extract. If input is DVB-Sub that
    -- is being passed through, omit this field (and PID field); there is no
    -- way to extract a specific language with pass-through captions.
    customLanguageCode :: Core.Maybe Core.Text,
    -- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
    -- file, specify the URI of the input captions source file. If your input
    -- captions are IMSC in an IMF package, use TrackSourceSettings instead of
    -- FileSoureSettings.
    sourceSettings :: Core.Maybe CaptionSourceSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CaptionSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'captionSelector_languageCode' - The specific language to extract from source. If input is SCTE-27,
-- complete this field and\/or PID to select the caption language to
-- extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete
-- this field and\/or PID to select the caption language to extract. If
-- input is DVB-Sub that is being passed through, omit this field (and PID
-- field); there is no way to extract a specific language with pass-through
-- captions.
--
-- 'customLanguageCode', 'captionSelector_customLanguageCode' - The specific language to extract from source, using the ISO 639-2 or ISO
-- 639-3 three-letter language code. If input is SCTE-27, complete this
-- field and\/or PID to select the caption language to extract. If input is
-- DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and\/or
-- PID to select the caption language to extract. If input is DVB-Sub that
-- is being passed through, omit this field (and PID field); there is no
-- way to extract a specific language with pass-through captions.
--
-- 'sourceSettings', 'captionSelector_sourceSettings' - If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
-- file, specify the URI of the input captions source file. If your input
-- captions are IMSC in an IMF package, use TrackSourceSettings instead of
-- FileSoureSettings.
newCaptionSelector ::
  CaptionSelector
newCaptionSelector =
  CaptionSelector'
    { languageCode = Core.Nothing,
      customLanguageCode = Core.Nothing,
      sourceSettings = Core.Nothing
    }

-- | The specific language to extract from source. If input is SCTE-27,
-- complete this field and\/or PID to select the caption language to
-- extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete
-- this field and\/or PID to select the caption language to extract. If
-- input is DVB-Sub that is being passed through, omit this field (and PID
-- field); there is no way to extract a specific language with pass-through
-- captions.
captionSelector_languageCode :: Lens.Lens' CaptionSelector (Core.Maybe LanguageCode)
captionSelector_languageCode = Lens.lens (\CaptionSelector' {languageCode} -> languageCode) (\s@CaptionSelector' {} a -> s {languageCode = a} :: CaptionSelector)

-- | The specific language to extract from source, using the ISO 639-2 or ISO
-- 639-3 three-letter language code. If input is SCTE-27, complete this
-- field and\/or PID to select the caption language to extract. If input is
-- DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and\/or
-- PID to select the caption language to extract. If input is DVB-Sub that
-- is being passed through, omit this field (and PID field); there is no
-- way to extract a specific language with pass-through captions.
captionSelector_customLanguageCode :: Lens.Lens' CaptionSelector (Core.Maybe Core.Text)
captionSelector_customLanguageCode = Lens.lens (\CaptionSelector' {customLanguageCode} -> customLanguageCode) (\s@CaptionSelector' {} a -> s {customLanguageCode = a} :: CaptionSelector)

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
-- file, specify the URI of the input captions source file. If your input
-- captions are IMSC in an IMF package, use TrackSourceSettings instead of
-- FileSoureSettings.
captionSelector_sourceSettings :: Lens.Lens' CaptionSelector (Core.Maybe CaptionSourceSettings)
captionSelector_sourceSettings = Lens.lens (\CaptionSelector' {sourceSettings} -> sourceSettings) (\s@CaptionSelector' {} a -> s {sourceSettings = a} :: CaptionSelector)

instance Core.FromJSON CaptionSelector where
  parseJSON =
    Core.withObject
      "CaptionSelector"
      ( \x ->
          CaptionSelector'
            Core.<$> (x Core..:? "languageCode")
            Core.<*> (x Core..:? "customLanguageCode")
            Core.<*> (x Core..:? "sourceSettings")
      )

instance Core.Hashable CaptionSelector

instance Core.NFData CaptionSelector

instance Core.ToJSON CaptionSelector where
  toJSON CaptionSelector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("languageCode" Core..=) Core.<$> languageCode,
            ("customLanguageCode" Core..=)
              Core.<$> customLanguageCode,
            ("sourceSettings" Core..=) Core.<$> sourceSettings
          ]
      )
