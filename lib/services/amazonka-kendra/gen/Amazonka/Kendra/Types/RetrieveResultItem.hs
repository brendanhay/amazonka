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
-- Module      : Amazonka.Kendra.Types.RetrieveResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.RetrieveResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DocumentAttribute
import qualified Amazonka.Prelude as Prelude

-- | A single retrieved relevant passage result.
--
-- /See:/ 'newRetrieveResultItem' smart constructor.
data RetrieveResultItem = RetrieveResultItem'
  { -- | The contents of the relevant passage.
    content :: Prelude.Maybe Prelude.Text,
    -- | An array of document fields\/attributes assigned to a document in the
    -- search results. For example, the document author (@_author@) or the
    -- source URI (@_source_uri@) of the document.
    documentAttributes :: Prelude.Maybe [DocumentAttribute],
    -- | The identifier of the document.
    documentId :: Prelude.Maybe Prelude.Text,
    -- | The title of the document.
    documentTitle :: Prelude.Maybe Prelude.Text,
    -- | The URI of the original location of the document.
    documentURI :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the relevant passage result.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetrieveResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'retrieveResultItem_content' - The contents of the relevant passage.
--
-- 'documentAttributes', 'retrieveResultItem_documentAttributes' - An array of document fields\/attributes assigned to a document in the
-- search results. For example, the document author (@_author@) or the
-- source URI (@_source_uri@) of the document.
--
-- 'documentId', 'retrieveResultItem_documentId' - The identifier of the document.
--
-- 'documentTitle', 'retrieveResultItem_documentTitle' - The title of the document.
--
-- 'documentURI', 'retrieveResultItem_documentURI' - The URI of the original location of the document.
--
-- 'id', 'retrieveResultItem_id' - The identifier of the relevant passage result.
newRetrieveResultItem ::
  RetrieveResultItem
newRetrieveResultItem =
  RetrieveResultItem'
    { content = Prelude.Nothing,
      documentAttributes = Prelude.Nothing,
      documentId = Prelude.Nothing,
      documentTitle = Prelude.Nothing,
      documentURI = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The contents of the relevant passage.
retrieveResultItem_content :: Lens.Lens' RetrieveResultItem (Prelude.Maybe Prelude.Text)
retrieveResultItem_content = Lens.lens (\RetrieveResultItem' {content} -> content) (\s@RetrieveResultItem' {} a -> s {content = a} :: RetrieveResultItem)

-- | An array of document fields\/attributes assigned to a document in the
-- search results. For example, the document author (@_author@) or the
-- source URI (@_source_uri@) of the document.
retrieveResultItem_documentAttributes :: Lens.Lens' RetrieveResultItem (Prelude.Maybe [DocumentAttribute])
retrieveResultItem_documentAttributes = Lens.lens (\RetrieveResultItem' {documentAttributes} -> documentAttributes) (\s@RetrieveResultItem' {} a -> s {documentAttributes = a} :: RetrieveResultItem) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the document.
retrieveResultItem_documentId :: Lens.Lens' RetrieveResultItem (Prelude.Maybe Prelude.Text)
retrieveResultItem_documentId = Lens.lens (\RetrieveResultItem' {documentId} -> documentId) (\s@RetrieveResultItem' {} a -> s {documentId = a} :: RetrieveResultItem)

-- | The title of the document.
retrieveResultItem_documentTitle :: Lens.Lens' RetrieveResultItem (Prelude.Maybe Prelude.Text)
retrieveResultItem_documentTitle = Lens.lens (\RetrieveResultItem' {documentTitle} -> documentTitle) (\s@RetrieveResultItem' {} a -> s {documentTitle = a} :: RetrieveResultItem)

-- | The URI of the original location of the document.
retrieveResultItem_documentURI :: Lens.Lens' RetrieveResultItem (Prelude.Maybe Prelude.Text)
retrieveResultItem_documentURI = Lens.lens (\RetrieveResultItem' {documentURI} -> documentURI) (\s@RetrieveResultItem' {} a -> s {documentURI = a} :: RetrieveResultItem)

-- | The identifier of the relevant passage result.
retrieveResultItem_id :: Lens.Lens' RetrieveResultItem (Prelude.Maybe Prelude.Text)
retrieveResultItem_id = Lens.lens (\RetrieveResultItem' {id} -> id) (\s@RetrieveResultItem' {} a -> s {id = a} :: RetrieveResultItem)

instance Data.FromJSON RetrieveResultItem where
  parseJSON =
    Data.withObject
      "RetrieveResultItem"
      ( \x ->
          RetrieveResultItem'
            Prelude.<$> (x Data..:? "Content")
            Prelude.<*> ( x
                            Data..:? "DocumentAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DocumentId")
            Prelude.<*> (x Data..:? "DocumentTitle")
            Prelude.<*> (x Data..:? "DocumentURI")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable RetrieveResultItem where
  hashWithSalt _salt RetrieveResultItem' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` documentAttributes
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` documentTitle
      `Prelude.hashWithSalt` documentURI
      `Prelude.hashWithSalt` id

instance Prelude.NFData RetrieveResultItem where
  rnf RetrieveResultItem' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf documentAttributes
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf documentTitle
      `Prelude.seq` Prelude.rnf documentURI
      `Prelude.seq` Prelude.rnf id
