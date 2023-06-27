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
-- Module      : Amazonka.WorkDocs.Types.ResponseItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ResponseItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.CommentMetadata
import Amazonka.WorkDocs.Types.DocumentMetadata
import Amazonka.WorkDocs.Types.DocumentVersionMetadata
import Amazonka.WorkDocs.Types.FolderMetadata
import Amazonka.WorkDocs.Types.ResponseItemType

-- | List of Documents, Folders, Comments, and Document Versions matching the
-- query.
--
-- /See:/ 'newResponseItem' smart constructor.
data ResponseItem = ResponseItem'
  { -- | The comment that matches the query.
    commentMetadata :: Prelude.Maybe CommentMetadata,
    -- | The document that matches the query.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The document version that matches the metadata.
    documentVersionMetadata :: Prelude.Maybe DocumentVersionMetadata,
    -- | The folder that matches the query.
    folderMetadata :: Prelude.Maybe FolderMetadata,
    -- | The type of item being returned.
    resourceType :: Prelude.Maybe ResponseItemType,
    -- | The webUrl of the item being returned.
    webUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentMetadata', 'responseItem_commentMetadata' - The comment that matches the query.
--
-- 'documentMetadata', 'responseItem_documentMetadata' - The document that matches the query.
--
-- 'documentVersionMetadata', 'responseItem_documentVersionMetadata' - The document version that matches the metadata.
--
-- 'folderMetadata', 'responseItem_folderMetadata' - The folder that matches the query.
--
-- 'resourceType', 'responseItem_resourceType' - The type of item being returned.
--
-- 'webUrl', 'responseItem_webUrl' - The webUrl of the item being returned.
newResponseItem ::
  ResponseItem
newResponseItem =
  ResponseItem'
    { commentMetadata = Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      documentVersionMetadata = Prelude.Nothing,
      folderMetadata = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      webUrl = Prelude.Nothing
    }

-- | The comment that matches the query.
responseItem_commentMetadata :: Lens.Lens' ResponseItem (Prelude.Maybe CommentMetadata)
responseItem_commentMetadata = Lens.lens (\ResponseItem' {commentMetadata} -> commentMetadata) (\s@ResponseItem' {} a -> s {commentMetadata = a} :: ResponseItem)

-- | The document that matches the query.
responseItem_documentMetadata :: Lens.Lens' ResponseItem (Prelude.Maybe DocumentMetadata)
responseItem_documentMetadata = Lens.lens (\ResponseItem' {documentMetadata} -> documentMetadata) (\s@ResponseItem' {} a -> s {documentMetadata = a} :: ResponseItem)

-- | The document version that matches the metadata.
responseItem_documentVersionMetadata :: Lens.Lens' ResponseItem (Prelude.Maybe DocumentVersionMetadata)
responseItem_documentVersionMetadata = Lens.lens (\ResponseItem' {documentVersionMetadata} -> documentVersionMetadata) (\s@ResponseItem' {} a -> s {documentVersionMetadata = a} :: ResponseItem)

-- | The folder that matches the query.
responseItem_folderMetadata :: Lens.Lens' ResponseItem (Prelude.Maybe FolderMetadata)
responseItem_folderMetadata = Lens.lens (\ResponseItem' {folderMetadata} -> folderMetadata) (\s@ResponseItem' {} a -> s {folderMetadata = a} :: ResponseItem)

-- | The type of item being returned.
responseItem_resourceType :: Lens.Lens' ResponseItem (Prelude.Maybe ResponseItemType)
responseItem_resourceType = Lens.lens (\ResponseItem' {resourceType} -> resourceType) (\s@ResponseItem' {} a -> s {resourceType = a} :: ResponseItem)

-- | The webUrl of the item being returned.
responseItem_webUrl :: Lens.Lens' ResponseItem (Prelude.Maybe Prelude.Text)
responseItem_webUrl = Lens.lens (\ResponseItem' {webUrl} -> webUrl) (\s@ResponseItem' {} a -> s {webUrl = a} :: ResponseItem) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ResponseItem where
  parseJSON =
    Data.withObject
      "ResponseItem"
      ( \x ->
          ResponseItem'
            Prelude.<$> (x Data..:? "CommentMetadata")
            Prelude.<*> (x Data..:? "DocumentMetadata")
            Prelude.<*> (x Data..:? "DocumentVersionMetadata")
            Prelude.<*> (x Data..:? "FolderMetadata")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "WebUrl")
      )

instance Prelude.Hashable ResponseItem where
  hashWithSalt _salt ResponseItem' {..} =
    _salt
      `Prelude.hashWithSalt` commentMetadata
      `Prelude.hashWithSalt` documentMetadata
      `Prelude.hashWithSalt` documentVersionMetadata
      `Prelude.hashWithSalt` folderMetadata
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` webUrl

instance Prelude.NFData ResponseItem where
  rnf ResponseItem' {..} =
    Prelude.rnf commentMetadata
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf documentVersionMetadata
      `Prelude.seq` Prelude.rnf folderMetadata
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf webUrl
