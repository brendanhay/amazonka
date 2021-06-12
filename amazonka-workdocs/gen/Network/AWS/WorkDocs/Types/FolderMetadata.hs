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
-- Module      : Network.AWS.WorkDocs.Types.FolderMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.FolderMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.ResourceStateType

-- | Describes a folder.
--
-- /See:/ 'newFolderMetadata' smart constructor.
data FolderMetadata = FolderMetadata'
  { -- | The time when the folder was updated.
    modifiedTimestamp :: Core.Maybe Core.POSIX,
    -- | The ID of the parent folder.
    parentFolderId :: Core.Maybe Core.Text,
    -- | The size of the latest version of the folder metadata.
    latestVersionSize :: Core.Maybe Core.Integer,
    -- | The ID of the creator.
    creatorId :: Core.Maybe Core.Text,
    -- | The time when the folder was created.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The ID of the folder.
    id :: Core.Maybe Core.Text,
    -- | List of labels on the folder.
    labels :: Core.Maybe [Core.Text],
    -- | The name of the folder.
    name :: Core.Maybe Core.Text,
    -- | The unique identifier created from the subfolders and documents of the
    -- folder.
    signature :: Core.Maybe Core.Text,
    -- | The resource state of the folder.
    resourceState :: Core.Maybe ResourceStateType,
    -- | The size of the folder metadata.
    size :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FolderMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modifiedTimestamp', 'folderMetadata_modifiedTimestamp' - The time when the folder was updated.
--
-- 'parentFolderId', 'folderMetadata_parentFolderId' - The ID of the parent folder.
--
-- 'latestVersionSize', 'folderMetadata_latestVersionSize' - The size of the latest version of the folder metadata.
--
-- 'creatorId', 'folderMetadata_creatorId' - The ID of the creator.
--
-- 'createdTimestamp', 'folderMetadata_createdTimestamp' - The time when the folder was created.
--
-- 'id', 'folderMetadata_id' - The ID of the folder.
--
-- 'labels', 'folderMetadata_labels' - List of labels on the folder.
--
-- 'name', 'folderMetadata_name' - The name of the folder.
--
-- 'signature', 'folderMetadata_signature' - The unique identifier created from the subfolders and documents of the
-- folder.
--
-- 'resourceState', 'folderMetadata_resourceState' - The resource state of the folder.
--
-- 'size', 'folderMetadata_size' - The size of the folder metadata.
newFolderMetadata ::
  FolderMetadata
newFolderMetadata =
  FolderMetadata'
    { modifiedTimestamp = Core.Nothing,
      parentFolderId = Core.Nothing,
      latestVersionSize = Core.Nothing,
      creatorId = Core.Nothing,
      createdTimestamp = Core.Nothing,
      id = Core.Nothing,
      labels = Core.Nothing,
      name = Core.Nothing,
      signature = Core.Nothing,
      resourceState = Core.Nothing,
      size = Core.Nothing
    }

-- | The time when the folder was updated.
folderMetadata_modifiedTimestamp :: Lens.Lens' FolderMetadata (Core.Maybe Core.UTCTime)
folderMetadata_modifiedTimestamp = Lens.lens (\FolderMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@FolderMetadata' {} a -> s {modifiedTimestamp = a} :: FolderMetadata) Core.. Lens.mapping Core._Time

-- | The ID of the parent folder.
folderMetadata_parentFolderId :: Lens.Lens' FolderMetadata (Core.Maybe Core.Text)
folderMetadata_parentFolderId = Lens.lens (\FolderMetadata' {parentFolderId} -> parentFolderId) (\s@FolderMetadata' {} a -> s {parentFolderId = a} :: FolderMetadata)

-- | The size of the latest version of the folder metadata.
folderMetadata_latestVersionSize :: Lens.Lens' FolderMetadata (Core.Maybe Core.Integer)
folderMetadata_latestVersionSize = Lens.lens (\FolderMetadata' {latestVersionSize} -> latestVersionSize) (\s@FolderMetadata' {} a -> s {latestVersionSize = a} :: FolderMetadata)

-- | The ID of the creator.
folderMetadata_creatorId :: Lens.Lens' FolderMetadata (Core.Maybe Core.Text)
folderMetadata_creatorId = Lens.lens (\FolderMetadata' {creatorId} -> creatorId) (\s@FolderMetadata' {} a -> s {creatorId = a} :: FolderMetadata)

-- | The time when the folder was created.
folderMetadata_createdTimestamp :: Lens.Lens' FolderMetadata (Core.Maybe Core.UTCTime)
folderMetadata_createdTimestamp = Lens.lens (\FolderMetadata' {createdTimestamp} -> createdTimestamp) (\s@FolderMetadata' {} a -> s {createdTimestamp = a} :: FolderMetadata) Core.. Lens.mapping Core._Time

-- | The ID of the folder.
folderMetadata_id :: Lens.Lens' FolderMetadata (Core.Maybe Core.Text)
folderMetadata_id = Lens.lens (\FolderMetadata' {id} -> id) (\s@FolderMetadata' {} a -> s {id = a} :: FolderMetadata)

-- | List of labels on the folder.
folderMetadata_labels :: Lens.Lens' FolderMetadata (Core.Maybe [Core.Text])
folderMetadata_labels = Lens.lens (\FolderMetadata' {labels} -> labels) (\s@FolderMetadata' {} a -> s {labels = a} :: FolderMetadata) Core.. Lens.mapping Lens._Coerce

-- | The name of the folder.
folderMetadata_name :: Lens.Lens' FolderMetadata (Core.Maybe Core.Text)
folderMetadata_name = Lens.lens (\FolderMetadata' {name} -> name) (\s@FolderMetadata' {} a -> s {name = a} :: FolderMetadata)

-- | The unique identifier created from the subfolders and documents of the
-- folder.
folderMetadata_signature :: Lens.Lens' FolderMetadata (Core.Maybe Core.Text)
folderMetadata_signature = Lens.lens (\FolderMetadata' {signature} -> signature) (\s@FolderMetadata' {} a -> s {signature = a} :: FolderMetadata)

-- | The resource state of the folder.
folderMetadata_resourceState :: Lens.Lens' FolderMetadata (Core.Maybe ResourceStateType)
folderMetadata_resourceState = Lens.lens (\FolderMetadata' {resourceState} -> resourceState) (\s@FolderMetadata' {} a -> s {resourceState = a} :: FolderMetadata)

-- | The size of the folder metadata.
folderMetadata_size :: Lens.Lens' FolderMetadata (Core.Maybe Core.Integer)
folderMetadata_size = Lens.lens (\FolderMetadata' {size} -> size) (\s@FolderMetadata' {} a -> s {size = a} :: FolderMetadata)

instance Core.FromJSON FolderMetadata where
  parseJSON =
    Core.withObject
      "FolderMetadata"
      ( \x ->
          FolderMetadata'
            Core.<$> (x Core..:? "ModifiedTimestamp")
            Core.<*> (x Core..:? "ParentFolderId")
            Core.<*> (x Core..:? "LatestVersionSize")
            Core.<*> (x Core..:? "CreatorId")
            Core.<*> (x Core..:? "CreatedTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Labels" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Signature")
            Core.<*> (x Core..:? "ResourceState")
            Core.<*> (x Core..:? "Size")
      )

instance Core.Hashable FolderMetadata

instance Core.NFData FolderMetadata
