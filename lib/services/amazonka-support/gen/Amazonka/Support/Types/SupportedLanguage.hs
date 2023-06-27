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
-- Module      : Amazonka.Support.Types.SupportedLanguage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.SupportedLanguage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A JSON-formatted object that contains the available ISO 639-1 language
-- @code@, @language@ name and langauge @display@ value. The language code
-- is what should be used in the CreateCase call.
--
-- /See:/ 'newSupportedLanguage' smart constructor.
data SupportedLanguage = SupportedLanguage'
  { -- | 2 digit ISO 639-1 code. e.g. @en@
    code :: Prelude.Maybe Prelude.Text,
    -- | Language display value e.g. @ENGLISH@
    display :: Prelude.Maybe Prelude.Text,
    -- | Full language description e.g. @ENGLISH@
    language :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportedLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'supportedLanguage_code' - 2 digit ISO 639-1 code. e.g. @en@
--
-- 'display', 'supportedLanguage_display' - Language display value e.g. @ENGLISH@
--
-- 'language', 'supportedLanguage_language' - Full language description e.g. @ENGLISH@
newSupportedLanguage ::
  SupportedLanguage
newSupportedLanguage =
  SupportedLanguage'
    { code = Prelude.Nothing,
      display = Prelude.Nothing,
      language = Prelude.Nothing
    }

-- | 2 digit ISO 639-1 code. e.g. @en@
supportedLanguage_code :: Lens.Lens' SupportedLanguage (Prelude.Maybe Prelude.Text)
supportedLanguage_code = Lens.lens (\SupportedLanguage' {code} -> code) (\s@SupportedLanguage' {} a -> s {code = a} :: SupportedLanguage)

-- | Language display value e.g. @ENGLISH@
supportedLanguage_display :: Lens.Lens' SupportedLanguage (Prelude.Maybe Prelude.Text)
supportedLanguage_display = Lens.lens (\SupportedLanguage' {display} -> display) (\s@SupportedLanguage' {} a -> s {display = a} :: SupportedLanguage)

-- | Full language description e.g. @ENGLISH@
supportedLanguage_language :: Lens.Lens' SupportedLanguage (Prelude.Maybe Prelude.Text)
supportedLanguage_language = Lens.lens (\SupportedLanguage' {language} -> language) (\s@SupportedLanguage' {} a -> s {language = a} :: SupportedLanguage)

instance Data.FromJSON SupportedLanguage where
  parseJSON =
    Data.withObject
      "SupportedLanguage"
      ( \x ->
          SupportedLanguage'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "display")
            Prelude.<*> (x Data..:? "language")
      )

instance Prelude.Hashable SupportedLanguage where
  hashWithSalt _salt SupportedLanguage' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` display
      `Prelude.hashWithSalt` language

instance Prelude.NFData SupportedLanguage where
  rnf SupportedLanguage' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf display
      `Prelude.seq` Prelude.rnf language
