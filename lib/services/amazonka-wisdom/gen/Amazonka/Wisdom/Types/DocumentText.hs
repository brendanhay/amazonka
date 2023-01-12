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
-- Module      : Amazonka.Wisdom.Types.DocumentText
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.DocumentText where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.Highlight

-- | The text of the document.
--
-- /See:/ 'newDocumentText' smart constructor.
data DocumentText = DocumentText'
  { -- | Highlights in the document text.
    highlights :: Prelude.Maybe [Highlight],
    -- | Text in the document.
    text :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'highlights', 'documentText_highlights' - Highlights in the document text.
--
-- 'text', 'documentText_text' - Text in the document.
newDocumentText ::
  DocumentText
newDocumentText =
  DocumentText'
    { highlights = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | Highlights in the document text.
documentText_highlights :: Lens.Lens' DocumentText (Prelude.Maybe [Highlight])
documentText_highlights = Lens.lens (\DocumentText' {highlights} -> highlights) (\s@DocumentText' {} a -> s {highlights = a} :: DocumentText) Prelude.. Lens.mapping Lens.coerced

-- | Text in the document.
documentText_text :: Lens.Lens' DocumentText (Prelude.Maybe Prelude.Text)
documentText_text = Lens.lens (\DocumentText' {text} -> text) (\s@DocumentText' {} a -> s {text = a} :: DocumentText) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON DocumentText where
  parseJSON =
    Data.withObject
      "DocumentText"
      ( \x ->
          DocumentText'
            Prelude.<$> (x Data..:? "highlights" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "text")
      )

instance Prelude.Hashable DocumentText where
  hashWithSalt _salt DocumentText' {..} =
    _salt `Prelude.hashWithSalt` highlights
      `Prelude.hashWithSalt` text

instance Prelude.NFData DocumentText where
  rnf DocumentText' {..} =
    Prelude.rnf highlights
      `Prelude.seq` Prelude.rnf text
