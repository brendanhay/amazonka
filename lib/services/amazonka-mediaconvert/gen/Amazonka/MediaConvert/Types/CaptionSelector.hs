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
-- Module      : Amazonka.MediaConvert.Types.CaptionSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.CaptionSourceSettings
import Amazonka.MediaConvert.Types.LanguageCode
import qualified Amazonka.Prelude as Prelude

-- | Use captions selectors to specify the captions data from your input that
-- you use in your outputs. You can use up to 20 captions selectors per
-- input.
--
-- /See:/ 'newCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { -- | The specific language to extract from source, using the ISO 639-2 or ISO
    -- 639-3 three-letter language code. If input is SCTE-27, complete this
    -- field and\/or PID to select the caption language to extract. If input is
    -- DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and\/or
    -- PID to select the caption language to extract. If input is DVB-Sub that
    -- is being passed through, omit this field (and PID field); there is no
    -- way to extract a specific language with pass-through captions.
    customLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | The specific language to extract from source. If input is SCTE-27,
    -- complete this field and\/or PID to select the caption language to
    -- extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete
    -- this field and\/or PID to select the caption language to extract. If
    -- input is DVB-Sub that is being passed through, omit this field (and PID
    -- field); there is no way to extract a specific language with pass-through
    -- captions.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
    -- file, specify the URI of the input captions source file. If your input
    -- captions are IMSC in an IMF package, use TrackSourceSettings instead of
    -- FileSoureSettings.
    sourceSettings :: Prelude.Maybe CaptionSourceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLanguageCode', 'captionSelector_customLanguageCode' - The specific language to extract from source, using the ISO 639-2 or ISO
-- 639-3 three-letter language code. If input is SCTE-27, complete this
-- field and\/or PID to select the caption language to extract. If input is
-- DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and\/or
-- PID to select the caption language to extract. If input is DVB-Sub that
-- is being passed through, omit this field (and PID field); there is no
-- way to extract a specific language with pass-through captions.
--
-- 'languageCode', 'captionSelector_languageCode' - The specific language to extract from source. If input is SCTE-27,
-- complete this field and\/or PID to select the caption language to
-- extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete
-- this field and\/or PID to select the caption language to extract. If
-- input is DVB-Sub that is being passed through, omit this field (and PID
-- field); there is no way to extract a specific language with pass-through
-- captions.
--
-- 'sourceSettings', 'captionSelector_sourceSettings' - If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
-- file, specify the URI of the input captions source file. If your input
-- captions are IMSC in an IMF package, use TrackSourceSettings instead of
-- FileSoureSettings.
newCaptionSelector ::
  CaptionSelector
newCaptionSelector =
  CaptionSelector'
    { customLanguageCode =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      sourceSettings = Prelude.Nothing
    }

-- | The specific language to extract from source, using the ISO 639-2 or ISO
-- 639-3 three-letter language code. If input is SCTE-27, complete this
-- field and\/or PID to select the caption language to extract. If input is
-- DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and\/or
-- PID to select the caption language to extract. If input is DVB-Sub that
-- is being passed through, omit this field (and PID field); there is no
-- way to extract a specific language with pass-through captions.
captionSelector_customLanguageCode :: Lens.Lens' CaptionSelector (Prelude.Maybe Prelude.Text)
captionSelector_customLanguageCode = Lens.lens (\CaptionSelector' {customLanguageCode} -> customLanguageCode) (\s@CaptionSelector' {} a -> s {customLanguageCode = a} :: CaptionSelector)

-- | The specific language to extract from source. If input is SCTE-27,
-- complete this field and\/or PID to select the caption language to
-- extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete
-- this field and\/or PID to select the caption language to extract. If
-- input is DVB-Sub that is being passed through, omit this field (and PID
-- field); there is no way to extract a specific language with pass-through
-- captions.
captionSelector_languageCode :: Lens.Lens' CaptionSelector (Prelude.Maybe LanguageCode)
captionSelector_languageCode = Lens.lens (\CaptionSelector' {languageCode} -> languageCode) (\s@CaptionSelector' {} a -> s {languageCode = a} :: CaptionSelector)

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
-- file, specify the URI of the input captions source file. If your input
-- captions are IMSC in an IMF package, use TrackSourceSettings instead of
-- FileSoureSettings.
captionSelector_sourceSettings :: Lens.Lens' CaptionSelector (Prelude.Maybe CaptionSourceSettings)
captionSelector_sourceSettings = Lens.lens (\CaptionSelector' {sourceSettings} -> sourceSettings) (\s@CaptionSelector' {} a -> s {sourceSettings = a} :: CaptionSelector)

instance Data.FromJSON CaptionSelector where
  parseJSON =
    Data.withObject
      "CaptionSelector"
      ( \x ->
          CaptionSelector'
            Prelude.<$> (x Data..:? "customLanguageCode")
            Prelude.<*> (x Data..:? "languageCode")
            Prelude.<*> (x Data..:? "sourceSettings")
      )

instance Prelude.Hashable CaptionSelector where
  hashWithSalt _salt CaptionSelector' {..} =
    _salt
      `Prelude.hashWithSalt` customLanguageCode
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` sourceSettings

instance Prelude.NFData CaptionSelector where
  rnf CaptionSelector' {..} =
    Prelude.rnf customLanguageCode
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf sourceSettings

instance Data.ToJSON CaptionSelector where
  toJSON CaptionSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("customLanguageCode" Data..=)
              Prelude.<$> customLanguageCode,
            ("languageCode" Data..=) Prelude.<$> languageCode,
            ("sourceSettings" Data..=)
              Prelude.<$> sourceSettings
          ]
      )
