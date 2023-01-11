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
-- Module      : Amazonka.WorkDocs.Types.DocumentMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.DocumentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.DocumentVersionMetadata
import Amazonka.WorkDocs.Types.ResourceStateType

-- | Describes the document.
--
-- /See:/ 'newDocumentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { -- | The time when the document was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ID of the creator.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the document.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of labels on the document.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The latest version of the document.
    latestVersionMetadata :: Prelude.Maybe DocumentVersionMetadata,
    -- | The time when the document was updated.
    modifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The resource state.
    resourceState :: Prelude.Maybe ResourceStateType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'documentMetadata_createdTimestamp' - The time when the document was created.
--
-- 'creatorId', 'documentMetadata_creatorId' - The ID of the creator.
--
-- 'id', 'documentMetadata_id' - The ID of the document.
--
-- 'labels', 'documentMetadata_labels' - List of labels on the document.
--
-- 'latestVersionMetadata', 'documentMetadata_latestVersionMetadata' - The latest version of the document.
--
-- 'modifiedTimestamp', 'documentMetadata_modifiedTimestamp' - The time when the document was updated.
--
-- 'parentFolderId', 'documentMetadata_parentFolderId' - The ID of the parent folder.
--
-- 'resourceState', 'documentMetadata_resourceState' - The resource state.
newDocumentMetadata ::
  DocumentMetadata
newDocumentMetadata =
  DocumentMetadata'
    { createdTimestamp =
        Prelude.Nothing,
      creatorId = Prelude.Nothing,
      id = Prelude.Nothing,
      labels = Prelude.Nothing,
      latestVersionMetadata = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing,
      parentFolderId = Prelude.Nothing,
      resourceState = Prelude.Nothing
    }

-- | The time when the document was created.
documentMetadata_createdTimestamp :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.UTCTime)
documentMetadata_createdTimestamp = Lens.lens (\DocumentMetadata' {createdTimestamp} -> createdTimestamp) (\s@DocumentMetadata' {} a -> s {createdTimestamp = a} :: DocumentMetadata) Prelude.. Lens.mapping Data._Time

-- | The ID of the creator.
documentMetadata_creatorId :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_creatorId = Lens.lens (\DocumentMetadata' {creatorId} -> creatorId) (\s@DocumentMetadata' {} a -> s {creatorId = a} :: DocumentMetadata)

-- | The ID of the document.
documentMetadata_id :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_id = Lens.lens (\DocumentMetadata' {id} -> id) (\s@DocumentMetadata' {} a -> s {id = a} :: DocumentMetadata)

-- | List of labels on the document.
documentMetadata_labels :: Lens.Lens' DocumentMetadata (Prelude.Maybe [Prelude.Text])
documentMetadata_labels = Lens.lens (\DocumentMetadata' {labels} -> labels) (\s@DocumentMetadata' {} a -> s {labels = a} :: DocumentMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The latest version of the document.
documentMetadata_latestVersionMetadata :: Lens.Lens' DocumentMetadata (Prelude.Maybe DocumentVersionMetadata)
documentMetadata_latestVersionMetadata = Lens.lens (\DocumentMetadata' {latestVersionMetadata} -> latestVersionMetadata) (\s@DocumentMetadata' {} a -> s {latestVersionMetadata = a} :: DocumentMetadata)

-- | The time when the document was updated.
documentMetadata_modifiedTimestamp :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.UTCTime)
documentMetadata_modifiedTimestamp = Lens.lens (\DocumentMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@DocumentMetadata' {} a -> s {modifiedTimestamp = a} :: DocumentMetadata) Prelude.. Lens.mapping Data._Time

-- | The ID of the parent folder.
documentMetadata_parentFolderId :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_parentFolderId = Lens.lens (\DocumentMetadata' {parentFolderId} -> parentFolderId) (\s@DocumentMetadata' {} a -> s {parentFolderId = a} :: DocumentMetadata)

-- | The resource state.
documentMetadata_resourceState :: Lens.Lens' DocumentMetadata (Prelude.Maybe ResourceStateType)
documentMetadata_resourceState = Lens.lens (\DocumentMetadata' {resourceState} -> resourceState) (\s@DocumentMetadata' {} a -> s {resourceState = a} :: DocumentMetadata)

instance Data.FromJSON DocumentMetadata where
  parseJSON =
    Data.withObject
      "DocumentMetadata"
      ( \x ->
          DocumentMetadata'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "CreatorId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LatestVersionMetadata")
            Prelude.<*> (x Data..:? "ModifiedTimestamp")
            Prelude.<*> (x Data..:? "ParentFolderId")
            Prelude.<*> (x Data..:? "ResourceState")
      )

instance Prelude.Hashable DocumentMetadata where
  hashWithSalt _salt DocumentMetadata' {..} =
    _salt `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` creatorId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` latestVersionMetadata
      `Prelude.hashWithSalt` modifiedTimestamp
      `Prelude.hashWithSalt` parentFolderId
      `Prelude.hashWithSalt` resourceState

instance Prelude.NFData DocumentMetadata where
  rnf DocumentMetadata' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf creatorId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf latestVersionMetadata
      `Prelude.seq` Prelude.rnf modifiedTimestamp
      `Prelude.seq` Prelude.rnf parentFolderId
      `Prelude.seq` Prelude.rnf resourceState
