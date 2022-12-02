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
-- Module      : Amazonka.WorkDocs.Types.FolderMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.FolderMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.ResourceStateType

-- | Describes a folder.
--
-- /See:/ 'newFolderMetadata' smart constructor.
data FolderMetadata = FolderMetadata'
  { -- | The ID of the creator.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | The name of the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The time when the folder was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The size of the folder metadata.
    size :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the folder.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of labels on the folder.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The resource state of the folder.
    resourceState :: Prelude.Maybe ResourceStateType,
    -- | The unique identifier created from the subfolders and documents of the
    -- folder.
    signature :: Prelude.Maybe Prelude.Text,
    -- | The size of the latest version of the folder metadata.
    latestVersionSize :: Prelude.Maybe Prelude.Integer,
    -- | The time when the folder was updated.
    modifiedTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FolderMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorId', 'folderMetadata_creatorId' - The ID of the creator.
--
-- 'name', 'folderMetadata_name' - The name of the folder.
--
-- 'parentFolderId', 'folderMetadata_parentFolderId' - The ID of the parent folder.
--
-- 'createdTimestamp', 'folderMetadata_createdTimestamp' - The time when the folder was created.
--
-- 'size', 'folderMetadata_size' - The size of the folder metadata.
--
-- 'id', 'folderMetadata_id' - The ID of the folder.
--
-- 'labels', 'folderMetadata_labels' - List of labels on the folder.
--
-- 'resourceState', 'folderMetadata_resourceState' - The resource state of the folder.
--
-- 'signature', 'folderMetadata_signature' - The unique identifier created from the subfolders and documents of the
-- folder.
--
-- 'latestVersionSize', 'folderMetadata_latestVersionSize' - The size of the latest version of the folder metadata.
--
-- 'modifiedTimestamp', 'folderMetadata_modifiedTimestamp' - The time when the folder was updated.
newFolderMetadata ::
  FolderMetadata
newFolderMetadata =
  FolderMetadata'
    { creatorId = Prelude.Nothing,
      name = Prelude.Nothing,
      parentFolderId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      size = Prelude.Nothing,
      id = Prelude.Nothing,
      labels = Prelude.Nothing,
      resourceState = Prelude.Nothing,
      signature = Prelude.Nothing,
      latestVersionSize = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing
    }

-- | The ID of the creator.
folderMetadata_creatorId :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_creatorId = Lens.lens (\FolderMetadata' {creatorId} -> creatorId) (\s@FolderMetadata' {} a -> s {creatorId = a} :: FolderMetadata)

-- | The name of the folder.
folderMetadata_name :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_name = Lens.lens (\FolderMetadata' {name} -> name) (\s@FolderMetadata' {} a -> s {name = a} :: FolderMetadata)

-- | The ID of the parent folder.
folderMetadata_parentFolderId :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_parentFolderId = Lens.lens (\FolderMetadata' {parentFolderId} -> parentFolderId) (\s@FolderMetadata' {} a -> s {parentFolderId = a} :: FolderMetadata)

-- | The time when the folder was created.
folderMetadata_createdTimestamp :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.UTCTime)
folderMetadata_createdTimestamp = Lens.lens (\FolderMetadata' {createdTimestamp} -> createdTimestamp) (\s@FolderMetadata' {} a -> s {createdTimestamp = a} :: FolderMetadata) Prelude.. Lens.mapping Data._Time

-- | The size of the folder metadata.
folderMetadata_size :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Integer)
folderMetadata_size = Lens.lens (\FolderMetadata' {size} -> size) (\s@FolderMetadata' {} a -> s {size = a} :: FolderMetadata)

-- | The ID of the folder.
folderMetadata_id :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_id = Lens.lens (\FolderMetadata' {id} -> id) (\s@FolderMetadata' {} a -> s {id = a} :: FolderMetadata)

-- | List of labels on the folder.
folderMetadata_labels :: Lens.Lens' FolderMetadata (Prelude.Maybe [Prelude.Text])
folderMetadata_labels = Lens.lens (\FolderMetadata' {labels} -> labels) (\s@FolderMetadata' {} a -> s {labels = a} :: FolderMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The resource state of the folder.
folderMetadata_resourceState :: Lens.Lens' FolderMetadata (Prelude.Maybe ResourceStateType)
folderMetadata_resourceState = Lens.lens (\FolderMetadata' {resourceState} -> resourceState) (\s@FolderMetadata' {} a -> s {resourceState = a} :: FolderMetadata)

-- | The unique identifier created from the subfolders and documents of the
-- folder.
folderMetadata_signature :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_signature = Lens.lens (\FolderMetadata' {signature} -> signature) (\s@FolderMetadata' {} a -> s {signature = a} :: FolderMetadata)

-- | The size of the latest version of the folder metadata.
folderMetadata_latestVersionSize :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Integer)
folderMetadata_latestVersionSize = Lens.lens (\FolderMetadata' {latestVersionSize} -> latestVersionSize) (\s@FolderMetadata' {} a -> s {latestVersionSize = a} :: FolderMetadata)

-- | The time when the folder was updated.
folderMetadata_modifiedTimestamp :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.UTCTime)
folderMetadata_modifiedTimestamp = Lens.lens (\FolderMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@FolderMetadata' {} a -> s {modifiedTimestamp = a} :: FolderMetadata) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON FolderMetadata where
  parseJSON =
    Data.withObject
      "FolderMetadata"
      ( \x ->
          FolderMetadata'
            Prelude.<$> (x Data..:? "CreatorId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ParentFolderId")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Size")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceState")
            Prelude.<*> (x Data..:? "Signature")
            Prelude.<*> (x Data..:? "LatestVersionSize")
            Prelude.<*> (x Data..:? "ModifiedTimestamp")
      )

instance Prelude.Hashable FolderMetadata where
  hashWithSalt _salt FolderMetadata' {..} =
    _salt `Prelude.hashWithSalt` creatorId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parentFolderId
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` resourceState
      `Prelude.hashWithSalt` signature
      `Prelude.hashWithSalt` latestVersionSize
      `Prelude.hashWithSalt` modifiedTimestamp

instance Prelude.NFData FolderMetadata where
  rnf FolderMetadata' {..} =
    Prelude.rnf creatorId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parentFolderId
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf resourceState
      `Prelude.seq` Prelude.rnf signature
      `Prelude.seq` Prelude.rnf latestVersionSize
      `Prelude.seq` Prelude.rnf modifiedTimestamp
