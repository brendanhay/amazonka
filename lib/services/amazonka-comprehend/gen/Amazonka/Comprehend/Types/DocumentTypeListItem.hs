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
-- Module      : Amazonka.Comprehend.Types.DocumentTypeListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentTypeListItem where

import Amazonka.Comprehend.Types.DocumentType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Document type for each page in the document.
--
-- /See:/ 'newDocumentTypeListItem' smart constructor.
data DocumentTypeListItem = DocumentTypeListItem'
  { -- | Page number.
    page :: Prelude.Maybe Prelude.Int,
    -- | Document type.
    type' :: Prelude.Maybe DocumentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentTypeListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'page', 'documentTypeListItem_page' - Page number.
--
-- 'type'', 'documentTypeListItem_type' - Document type.
newDocumentTypeListItem ::
  DocumentTypeListItem
newDocumentTypeListItem =
  DocumentTypeListItem'
    { page = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Page number.
documentTypeListItem_page :: Lens.Lens' DocumentTypeListItem (Prelude.Maybe Prelude.Int)
documentTypeListItem_page = Lens.lens (\DocumentTypeListItem' {page} -> page) (\s@DocumentTypeListItem' {} a -> s {page = a} :: DocumentTypeListItem)

-- | Document type.
documentTypeListItem_type :: Lens.Lens' DocumentTypeListItem (Prelude.Maybe DocumentType)
documentTypeListItem_type = Lens.lens (\DocumentTypeListItem' {type'} -> type') (\s@DocumentTypeListItem' {} a -> s {type' = a} :: DocumentTypeListItem)

instance Data.FromJSON DocumentTypeListItem where
  parseJSON =
    Data.withObject
      "DocumentTypeListItem"
      ( \x ->
          DocumentTypeListItem'
            Prelude.<$> (x Data..:? "Page") Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable DocumentTypeListItem where
  hashWithSalt _salt DocumentTypeListItem' {..} =
    _salt `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DocumentTypeListItem where
  rnf DocumentTypeListItem' {..} =
    Prelude.rnf page `Prelude.seq` Prelude.rnf type'
