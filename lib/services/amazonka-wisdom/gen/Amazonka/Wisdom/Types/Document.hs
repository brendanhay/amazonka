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
-- Module      : Amazonka.Wisdom.Types.Document
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.Document where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.ContentReference
import Amazonka.Wisdom.Types.DocumentText

-- | The document.
--
-- /See:/ 'newDocument' smart constructor.
data Document = Document'
  { -- | The excerpt from the document.
    excerpt :: Prelude.Maybe DocumentText,
    -- | The title of the document.
    title :: Prelude.Maybe DocumentText,
    -- | A reference to the content resource.
    contentReference :: ContentReference
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Document' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excerpt', 'document_excerpt' - The excerpt from the document.
--
-- 'title', 'document_title' - The title of the document.
--
-- 'contentReference', 'document_contentReference' - A reference to the content resource.
newDocument ::
  -- | 'contentReference'
  ContentReference ->
  Document
newDocument pContentReference_ =
  Document'
    { excerpt = Prelude.Nothing,
      title = Prelude.Nothing,
      contentReference = pContentReference_
    }

-- | The excerpt from the document.
document_excerpt :: Lens.Lens' Document (Prelude.Maybe DocumentText)
document_excerpt = Lens.lens (\Document' {excerpt} -> excerpt) (\s@Document' {} a -> s {excerpt = a} :: Document)

-- | The title of the document.
document_title :: Lens.Lens' Document (Prelude.Maybe DocumentText)
document_title = Lens.lens (\Document' {title} -> title) (\s@Document' {} a -> s {title = a} :: Document)

-- | A reference to the content resource.
document_contentReference :: Lens.Lens' Document ContentReference
document_contentReference = Lens.lens (\Document' {contentReference} -> contentReference) (\s@Document' {} a -> s {contentReference = a} :: Document)

instance Data.FromJSON Document where
  parseJSON =
    Data.withObject
      "Document"
      ( \x ->
          Document'
            Prelude.<$> (x Data..:? "excerpt")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..: "contentReference")
      )

instance Prelude.Hashable Document where
  hashWithSalt _salt Document' {..} =
    _salt
      `Prelude.hashWithSalt` excerpt
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` contentReference

instance Prelude.NFData Document where
  rnf Document' {..} =
    Prelude.rnf excerpt
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf contentReference
