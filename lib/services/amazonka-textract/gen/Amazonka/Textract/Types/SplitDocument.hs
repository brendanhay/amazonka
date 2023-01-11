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
-- Module      : Amazonka.Textract.Types.SplitDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.SplitDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the pages of a document, defined by logical
-- boundary.
--
-- /See:/ 'newSplitDocument' smart constructor.
data SplitDocument = SplitDocument'
  { -- | The index for a given document in a DocumentGroup of a specific Type.
    index :: Prelude.Maybe Prelude.Natural,
    -- | An array of page numbers for a for a given document, ordered by logical
    -- boundary.
    pages :: Prelude.Maybe [Prelude.Natural]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SplitDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'splitDocument_index' - The index for a given document in a DocumentGroup of a specific Type.
--
-- 'pages', 'splitDocument_pages' - An array of page numbers for a for a given document, ordered by logical
-- boundary.
newSplitDocument ::
  SplitDocument
newSplitDocument =
  SplitDocument'
    { index = Prelude.Nothing,
      pages = Prelude.Nothing
    }

-- | The index for a given document in a DocumentGroup of a specific Type.
splitDocument_index :: Lens.Lens' SplitDocument (Prelude.Maybe Prelude.Natural)
splitDocument_index = Lens.lens (\SplitDocument' {index} -> index) (\s@SplitDocument' {} a -> s {index = a} :: SplitDocument)

-- | An array of page numbers for a for a given document, ordered by logical
-- boundary.
splitDocument_pages :: Lens.Lens' SplitDocument (Prelude.Maybe [Prelude.Natural])
splitDocument_pages = Lens.lens (\SplitDocument' {pages} -> pages) (\s@SplitDocument' {} a -> s {pages = a} :: SplitDocument) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SplitDocument where
  parseJSON =
    Data.withObject
      "SplitDocument"
      ( \x ->
          SplitDocument'
            Prelude.<$> (x Data..:? "Index")
            Prelude.<*> (x Data..:? "Pages" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SplitDocument where
  hashWithSalt _salt SplitDocument' {..} =
    _salt `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` pages

instance Prelude.NFData SplitDocument where
  rnf SplitDocument' {..} =
    Prelude.rnf index `Prelude.seq` Prelude.rnf pages
