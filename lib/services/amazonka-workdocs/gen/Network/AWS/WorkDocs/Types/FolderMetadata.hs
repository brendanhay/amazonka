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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.FolderMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.ResourceStateType

-- | Describes a folder.
--
-- /See:/ 'newFolderMetadata' smart constructor.
data FolderMetadata = FolderMetadata'
  { -- | The unique identifier created from the subfolders and documents of the
    -- folder.
    signature :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The size of the folder metadata.
    size :: Prelude.Maybe Prelude.Integer,
    -- | The size of the latest version of the folder metadata.
    latestVersionSize :: Prelude.Maybe Prelude.Integer,
    -- | The name of the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time when the folder was updated.
    modifiedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ID of the folder.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of labels on the folder.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The resource state of the folder.
    resourceState :: Prelude.Maybe ResourceStateType,
    -- | The time when the folder was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ID of the creator.
    creatorId :: Prelude.Maybe Prelude.Text
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
-- 'signature', 'folderMetadata_signature' - The unique identifier created from the subfolders and documents of the
-- folder.
--
-- 'parentFolderId', 'folderMetadata_parentFolderId' - The ID of the parent folder.
--
-- 'size', 'folderMetadata_size' - The size of the folder metadata.
--
-- 'latestVersionSize', 'folderMetadata_latestVersionSize' - The size of the latest version of the folder metadata.
--
-- 'name', 'folderMetadata_name' - The name of the folder.
--
-- 'modifiedTimestamp', 'folderMetadata_modifiedTimestamp' - The time when the folder was updated.
--
-- 'id', 'folderMetadata_id' - The ID of the folder.
--
-- 'labels', 'folderMetadata_labels' - List of labels on the folder.
--
-- 'resourceState', 'folderMetadata_resourceState' - The resource state of the folder.
--
-- 'createdTimestamp', 'folderMetadata_createdTimestamp' - The time when the folder was created.
--
-- 'creatorId', 'folderMetadata_creatorId' - The ID of the creator.
newFolderMetadata ::
  FolderMetadata
newFolderMetadata =
  FolderMetadata'
    { signature = Prelude.Nothing,
      parentFolderId = Prelude.Nothing,
      size = Prelude.Nothing,
      latestVersionSize = Prelude.Nothing,
      name = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      labels = Prelude.Nothing,
      resourceState = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      creatorId = Prelude.Nothing
    }

-- | The unique identifier created from the subfolders and documents of the
-- folder.
folderMetadata_signature :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_signature = Lens.lens (\FolderMetadata' {signature} -> signature) (\s@FolderMetadata' {} a -> s {signature = a} :: FolderMetadata)

-- | The ID of the parent folder.
folderMetadata_parentFolderId :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_parentFolderId = Lens.lens (\FolderMetadata' {parentFolderId} -> parentFolderId) (\s@FolderMetadata' {} a -> s {parentFolderId = a} :: FolderMetadata)

-- | The size of the folder metadata.
folderMetadata_size :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Integer)
folderMetadata_size = Lens.lens (\FolderMetadata' {size} -> size) (\s@FolderMetadata' {} a -> s {size = a} :: FolderMetadata)

-- | The size of the latest version of the folder metadata.
folderMetadata_latestVersionSize :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Integer)
folderMetadata_latestVersionSize = Lens.lens (\FolderMetadata' {latestVersionSize} -> latestVersionSize) (\s@FolderMetadata' {} a -> s {latestVersionSize = a} :: FolderMetadata)

-- | The name of the folder.
folderMetadata_name :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_name = Lens.lens (\FolderMetadata' {name} -> name) (\s@FolderMetadata' {} a -> s {name = a} :: FolderMetadata)

-- | The time when the folder was updated.
folderMetadata_modifiedTimestamp :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.UTCTime)
folderMetadata_modifiedTimestamp = Lens.lens (\FolderMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@FolderMetadata' {} a -> s {modifiedTimestamp = a} :: FolderMetadata) Prelude.. Lens.mapping Core._Time

-- | The ID of the folder.
folderMetadata_id :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_id = Lens.lens (\FolderMetadata' {id} -> id) (\s@FolderMetadata' {} a -> s {id = a} :: FolderMetadata)

-- | List of labels on the folder.
folderMetadata_labels :: Lens.Lens' FolderMetadata (Prelude.Maybe [Prelude.Text])
folderMetadata_labels = Lens.lens (\FolderMetadata' {labels} -> labels) (\s@FolderMetadata' {} a -> s {labels = a} :: FolderMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The resource state of the folder.
folderMetadata_resourceState :: Lens.Lens' FolderMetadata (Prelude.Maybe ResourceStateType)
folderMetadata_resourceState = Lens.lens (\FolderMetadata' {resourceState} -> resourceState) (\s@FolderMetadata' {} a -> s {resourceState = a} :: FolderMetadata)

-- | The time when the folder was created.
folderMetadata_createdTimestamp :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.UTCTime)
folderMetadata_createdTimestamp = Lens.lens (\FolderMetadata' {createdTimestamp} -> createdTimestamp) (\s@FolderMetadata' {} a -> s {createdTimestamp = a} :: FolderMetadata) Prelude.. Lens.mapping Core._Time

-- | The ID of the creator.
folderMetadata_creatorId :: Lens.Lens' FolderMetadata (Prelude.Maybe Prelude.Text)
folderMetadata_creatorId = Lens.lens (\FolderMetadata' {creatorId} -> creatorId) (\s@FolderMetadata' {} a -> s {creatorId = a} :: FolderMetadata)

instance Core.FromJSON FolderMetadata where
  parseJSON =
    Core.withObject
      "FolderMetadata"
      ( \x ->
          FolderMetadata'
            Prelude.<$> (x Core..:? "Signature")
            Prelude.<*> (x Core..:? "ParentFolderId")
            Prelude.<*> (x Core..:? "Size")
            Prelude.<*> (x Core..:? "LatestVersionSize")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ModifiedTimestamp")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Labels" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResourceState")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "CreatorId")
      )

instance Prelude.Hashable FolderMetadata

instance Prelude.NFData FolderMetadata
