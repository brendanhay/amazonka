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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.DocumentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.DocumentVersionMetadata
import Amazonka.WorkDocs.Types.ResourceStateType

-- | Describes the document.
--
-- /See:/ 'newDocumentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { -- | The ID of the creator.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the document.
    latestVersionMetadata :: Prelude.Maybe DocumentVersionMetadata,
    -- | The time when the document was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ID of the document.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of labels on the document.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The resource state.
    resourceState :: Prelude.Maybe ResourceStateType,
    -- | The time when the document was updated.
    modifiedTimestamp :: Prelude.Maybe Core.POSIX
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
-- 'creatorId', 'documentMetadata_creatorId' - The ID of the creator.
--
-- 'parentFolderId', 'documentMetadata_parentFolderId' - The ID of the parent folder.
--
-- 'latestVersionMetadata', 'documentMetadata_latestVersionMetadata' - The latest version of the document.
--
-- 'createdTimestamp', 'documentMetadata_createdTimestamp' - The time when the document was created.
--
-- 'id', 'documentMetadata_id' - The ID of the document.
--
-- 'labels', 'documentMetadata_labels' - List of labels on the document.
--
-- 'resourceState', 'documentMetadata_resourceState' - The resource state.
--
-- 'modifiedTimestamp', 'documentMetadata_modifiedTimestamp' - The time when the document was updated.
newDocumentMetadata ::
  DocumentMetadata
newDocumentMetadata =
  DocumentMetadata'
    { creatorId = Prelude.Nothing,
      parentFolderId = Prelude.Nothing,
      latestVersionMetadata = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      labels = Prelude.Nothing,
      resourceState = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing
    }

-- | The ID of the creator.
documentMetadata_creatorId :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_creatorId = Lens.lens (\DocumentMetadata' {creatorId} -> creatorId) (\s@DocumentMetadata' {} a -> s {creatorId = a} :: DocumentMetadata)

-- | The ID of the parent folder.
documentMetadata_parentFolderId :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_parentFolderId = Lens.lens (\DocumentMetadata' {parentFolderId} -> parentFolderId) (\s@DocumentMetadata' {} a -> s {parentFolderId = a} :: DocumentMetadata)

-- | The latest version of the document.
documentMetadata_latestVersionMetadata :: Lens.Lens' DocumentMetadata (Prelude.Maybe DocumentVersionMetadata)
documentMetadata_latestVersionMetadata = Lens.lens (\DocumentMetadata' {latestVersionMetadata} -> latestVersionMetadata) (\s@DocumentMetadata' {} a -> s {latestVersionMetadata = a} :: DocumentMetadata)

-- | The time when the document was created.
documentMetadata_createdTimestamp :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.UTCTime)
documentMetadata_createdTimestamp = Lens.lens (\DocumentMetadata' {createdTimestamp} -> createdTimestamp) (\s@DocumentMetadata' {} a -> s {createdTimestamp = a} :: DocumentMetadata) Prelude.. Lens.mapping Core._Time

-- | The ID of the document.
documentMetadata_id :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_id = Lens.lens (\DocumentMetadata' {id} -> id) (\s@DocumentMetadata' {} a -> s {id = a} :: DocumentMetadata)

-- | List of labels on the document.
documentMetadata_labels :: Lens.Lens' DocumentMetadata (Prelude.Maybe [Prelude.Text])
documentMetadata_labels = Lens.lens (\DocumentMetadata' {labels} -> labels) (\s@DocumentMetadata' {} a -> s {labels = a} :: DocumentMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The resource state.
documentMetadata_resourceState :: Lens.Lens' DocumentMetadata (Prelude.Maybe ResourceStateType)
documentMetadata_resourceState = Lens.lens (\DocumentMetadata' {resourceState} -> resourceState) (\s@DocumentMetadata' {} a -> s {resourceState = a} :: DocumentMetadata)

-- | The time when the document was updated.
documentMetadata_modifiedTimestamp :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.UTCTime)
documentMetadata_modifiedTimestamp = Lens.lens (\DocumentMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@DocumentMetadata' {} a -> s {modifiedTimestamp = a} :: DocumentMetadata) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DocumentMetadata where
  parseJSON =
    Core.withObject
      "DocumentMetadata"
      ( \x ->
          DocumentMetadata'
            Prelude.<$> (x Core..:? "CreatorId")
            Prelude.<*> (x Core..:? "ParentFolderId")
            Prelude.<*> (x Core..:? "LatestVersionMetadata")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Labels" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResourceState")
            Prelude.<*> (x Core..:? "ModifiedTimestamp")
      )

instance Prelude.Hashable DocumentMetadata where
  hashWithSalt _salt DocumentMetadata' {..} =
    _salt `Prelude.hashWithSalt` creatorId
      `Prelude.hashWithSalt` parentFolderId
      `Prelude.hashWithSalt` latestVersionMetadata
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` resourceState
      `Prelude.hashWithSalt` modifiedTimestamp

instance Prelude.NFData DocumentMetadata where
  rnf DocumentMetadata' {..} =
    Prelude.rnf creatorId
      `Prelude.seq` Prelude.rnf parentFolderId
      `Prelude.seq` Prelude.rnf latestVersionMetadata
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf resourceState
      `Prelude.seq` Prelude.rnf modifiedTimestamp
