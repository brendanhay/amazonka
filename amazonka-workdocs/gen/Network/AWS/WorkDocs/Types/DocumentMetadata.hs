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
-- Module      : Network.AWS.WorkDocs.Types.DocumentMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.DocumentVersionMetadata
import Network.AWS.WorkDocs.Types.ResourceStateType

-- | Describes the document.
--
-- /See:/ 'newDocumentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { -- | The time when the document was updated.
    modifiedTimestamp :: Core.Maybe Core.POSIX,
    -- | The ID of the parent folder.
    parentFolderId :: Core.Maybe Core.Text,
    -- | The ID of the creator.
    creatorId :: Core.Maybe Core.Text,
    -- | The time when the document was created.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The ID of the document.
    id :: Core.Maybe Core.Text,
    -- | List of labels on the document.
    labels :: Core.Maybe [Core.Text],
    -- | The latest version of the document.
    latestVersionMetadata :: Core.Maybe DocumentVersionMetadata,
    -- | The resource state.
    resourceState :: Core.Maybe ResourceStateType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modifiedTimestamp', 'documentMetadata_modifiedTimestamp' - The time when the document was updated.
--
-- 'parentFolderId', 'documentMetadata_parentFolderId' - The ID of the parent folder.
--
-- 'creatorId', 'documentMetadata_creatorId' - The ID of the creator.
--
-- 'createdTimestamp', 'documentMetadata_createdTimestamp' - The time when the document was created.
--
-- 'id', 'documentMetadata_id' - The ID of the document.
--
-- 'labels', 'documentMetadata_labels' - List of labels on the document.
--
-- 'latestVersionMetadata', 'documentMetadata_latestVersionMetadata' - The latest version of the document.
--
-- 'resourceState', 'documentMetadata_resourceState' - The resource state.
newDocumentMetadata ::
  DocumentMetadata
newDocumentMetadata =
  DocumentMetadata'
    { modifiedTimestamp = Core.Nothing,
      parentFolderId = Core.Nothing,
      creatorId = Core.Nothing,
      createdTimestamp = Core.Nothing,
      id = Core.Nothing,
      labels = Core.Nothing,
      latestVersionMetadata = Core.Nothing,
      resourceState = Core.Nothing
    }

-- | The time when the document was updated.
documentMetadata_modifiedTimestamp :: Lens.Lens' DocumentMetadata (Core.Maybe Core.UTCTime)
documentMetadata_modifiedTimestamp = Lens.lens (\DocumentMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@DocumentMetadata' {} a -> s {modifiedTimestamp = a} :: DocumentMetadata) Core.. Lens.mapping Core._Time

-- | The ID of the parent folder.
documentMetadata_parentFolderId :: Lens.Lens' DocumentMetadata (Core.Maybe Core.Text)
documentMetadata_parentFolderId = Lens.lens (\DocumentMetadata' {parentFolderId} -> parentFolderId) (\s@DocumentMetadata' {} a -> s {parentFolderId = a} :: DocumentMetadata)

-- | The ID of the creator.
documentMetadata_creatorId :: Lens.Lens' DocumentMetadata (Core.Maybe Core.Text)
documentMetadata_creatorId = Lens.lens (\DocumentMetadata' {creatorId} -> creatorId) (\s@DocumentMetadata' {} a -> s {creatorId = a} :: DocumentMetadata)

-- | The time when the document was created.
documentMetadata_createdTimestamp :: Lens.Lens' DocumentMetadata (Core.Maybe Core.UTCTime)
documentMetadata_createdTimestamp = Lens.lens (\DocumentMetadata' {createdTimestamp} -> createdTimestamp) (\s@DocumentMetadata' {} a -> s {createdTimestamp = a} :: DocumentMetadata) Core.. Lens.mapping Core._Time

-- | The ID of the document.
documentMetadata_id :: Lens.Lens' DocumentMetadata (Core.Maybe Core.Text)
documentMetadata_id = Lens.lens (\DocumentMetadata' {id} -> id) (\s@DocumentMetadata' {} a -> s {id = a} :: DocumentMetadata)

-- | List of labels on the document.
documentMetadata_labels :: Lens.Lens' DocumentMetadata (Core.Maybe [Core.Text])
documentMetadata_labels = Lens.lens (\DocumentMetadata' {labels} -> labels) (\s@DocumentMetadata' {} a -> s {labels = a} :: DocumentMetadata) Core.. Lens.mapping Lens._Coerce

-- | The latest version of the document.
documentMetadata_latestVersionMetadata :: Lens.Lens' DocumentMetadata (Core.Maybe DocumentVersionMetadata)
documentMetadata_latestVersionMetadata = Lens.lens (\DocumentMetadata' {latestVersionMetadata} -> latestVersionMetadata) (\s@DocumentMetadata' {} a -> s {latestVersionMetadata = a} :: DocumentMetadata)

-- | The resource state.
documentMetadata_resourceState :: Lens.Lens' DocumentMetadata (Core.Maybe ResourceStateType)
documentMetadata_resourceState = Lens.lens (\DocumentMetadata' {resourceState} -> resourceState) (\s@DocumentMetadata' {} a -> s {resourceState = a} :: DocumentMetadata)

instance Core.FromJSON DocumentMetadata where
  parseJSON =
    Core.withObject
      "DocumentMetadata"
      ( \x ->
          DocumentMetadata'
            Core.<$> (x Core..:? "ModifiedTimestamp")
            Core.<*> (x Core..:? "ParentFolderId")
            Core.<*> (x Core..:? "CreatorId")
            Core.<*> (x Core..:? "CreatedTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Labels" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LatestVersionMetadata")
            Core.<*> (x Core..:? "ResourceState")
      )

instance Core.Hashable DocumentMetadata

instance Core.NFData DocumentMetadata
