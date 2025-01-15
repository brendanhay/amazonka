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
-- Module      : Amazonka.Comprehend.Types.DocumentMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentMetadata where

import Amazonka.Comprehend.Types.ExtractedCharactersListItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the document, discovered during text extraction.
--
-- /See:/ 'newDocumentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { -- | List of pages in the document, with the number of characters extracted
    -- from each page.
    extractedCharacters :: Prelude.Maybe [ExtractedCharactersListItem],
    -- | Number of pages in the document.
    pages :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extractedCharacters', 'documentMetadata_extractedCharacters' - List of pages in the document, with the number of characters extracted
-- from each page.
--
-- 'pages', 'documentMetadata_pages' - Number of pages in the document.
newDocumentMetadata ::
  DocumentMetadata
newDocumentMetadata =
  DocumentMetadata'
    { extractedCharacters =
        Prelude.Nothing,
      pages = Prelude.Nothing
    }

-- | List of pages in the document, with the number of characters extracted
-- from each page.
documentMetadata_extractedCharacters :: Lens.Lens' DocumentMetadata (Prelude.Maybe [ExtractedCharactersListItem])
documentMetadata_extractedCharacters = Lens.lens (\DocumentMetadata' {extractedCharacters} -> extractedCharacters) (\s@DocumentMetadata' {} a -> s {extractedCharacters = a} :: DocumentMetadata) Prelude.. Lens.mapping Lens.coerced

-- | Number of pages in the document.
documentMetadata_pages :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Int)
documentMetadata_pages = Lens.lens (\DocumentMetadata' {pages} -> pages) (\s@DocumentMetadata' {} a -> s {pages = a} :: DocumentMetadata)

instance Data.FromJSON DocumentMetadata where
  parseJSON =
    Data.withObject
      "DocumentMetadata"
      ( \x ->
          DocumentMetadata'
            Prelude.<$> ( x
                            Data..:? "ExtractedCharacters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Pages")
      )

instance Prelude.Hashable DocumentMetadata where
  hashWithSalt _salt DocumentMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` extractedCharacters
      `Prelude.hashWithSalt` pages

instance Prelude.NFData DocumentMetadata where
  rnf DocumentMetadata' {..} =
    Prelude.rnf extractedCharacters `Prelude.seq`
      Prelude.rnf pages
