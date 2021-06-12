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
-- Module      : Network.AWS.WorkDocs.Types.DocumentVersionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentVersionMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.DocumentSourceType
import Network.AWS.WorkDocs.Types.DocumentStatusType
import Network.AWS.WorkDocs.Types.DocumentThumbnailType

-- | Describes a version of a document.
--
-- /See:/ 'newDocumentVersionMetadata' smart constructor.
data DocumentVersionMetadata = DocumentVersionMetadata'
  { -- | The timestamp when the document was last uploaded.
    modifiedTimestamp :: Core.Maybe Core.POSIX,
    -- | The status of the document.
    status :: Core.Maybe DocumentStatusType,
    -- | The ID of the creator.
    creatorId :: Core.Maybe Core.Text,
    -- | The content type of the document.
    contentType :: Core.Maybe Core.Text,
    -- | The timestamp when the document was first uploaded.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The timestamp when the content of the document was modified.
    contentModifiedTimestamp :: Core.Maybe Core.POSIX,
    -- | The ID of the version.
    id :: Core.Maybe Core.Text,
    -- | The source of the document.
    source :: Core.Maybe (Core.HashMap DocumentSourceType (Core.Sensitive Core.Text)),
    -- | The timestamp when the content of the document was originally created.
    contentCreatedTimestamp :: Core.Maybe Core.POSIX,
    -- | The name of the version.
    name :: Core.Maybe Core.Text,
    -- | The signature of the document.
    signature :: Core.Maybe Core.Text,
    -- | The thumbnail of the document.
    thumbnail :: Core.Maybe (Core.HashMap DocumentThumbnailType (Core.Sensitive Core.Text)),
    -- | The size of the document, in bytes.
    size :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentVersionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modifiedTimestamp', 'documentVersionMetadata_modifiedTimestamp' - The timestamp when the document was last uploaded.
--
-- 'status', 'documentVersionMetadata_status' - The status of the document.
--
-- 'creatorId', 'documentVersionMetadata_creatorId' - The ID of the creator.
--
-- 'contentType', 'documentVersionMetadata_contentType' - The content type of the document.
--
-- 'createdTimestamp', 'documentVersionMetadata_createdTimestamp' - The timestamp when the document was first uploaded.
--
-- 'contentModifiedTimestamp', 'documentVersionMetadata_contentModifiedTimestamp' - The timestamp when the content of the document was modified.
--
-- 'id', 'documentVersionMetadata_id' - The ID of the version.
--
-- 'source', 'documentVersionMetadata_source' - The source of the document.
--
-- 'contentCreatedTimestamp', 'documentVersionMetadata_contentCreatedTimestamp' - The timestamp when the content of the document was originally created.
--
-- 'name', 'documentVersionMetadata_name' - The name of the version.
--
-- 'signature', 'documentVersionMetadata_signature' - The signature of the document.
--
-- 'thumbnail', 'documentVersionMetadata_thumbnail' - The thumbnail of the document.
--
-- 'size', 'documentVersionMetadata_size' - The size of the document, in bytes.
newDocumentVersionMetadata ::
  DocumentVersionMetadata
newDocumentVersionMetadata =
  DocumentVersionMetadata'
    { modifiedTimestamp =
        Core.Nothing,
      status = Core.Nothing,
      creatorId = Core.Nothing,
      contentType = Core.Nothing,
      createdTimestamp = Core.Nothing,
      contentModifiedTimestamp = Core.Nothing,
      id = Core.Nothing,
      source = Core.Nothing,
      contentCreatedTimestamp = Core.Nothing,
      name = Core.Nothing,
      signature = Core.Nothing,
      thumbnail = Core.Nothing,
      size = Core.Nothing
    }

-- | The timestamp when the document was last uploaded.
documentVersionMetadata_modifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.UTCTime)
documentVersionMetadata_modifiedTimestamp = Lens.lens (\DocumentVersionMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@DocumentVersionMetadata' {} a -> s {modifiedTimestamp = a} :: DocumentVersionMetadata) Core.. Lens.mapping Core._Time

-- | The status of the document.
documentVersionMetadata_status :: Lens.Lens' DocumentVersionMetadata (Core.Maybe DocumentStatusType)
documentVersionMetadata_status = Lens.lens (\DocumentVersionMetadata' {status} -> status) (\s@DocumentVersionMetadata' {} a -> s {status = a} :: DocumentVersionMetadata)

-- | The ID of the creator.
documentVersionMetadata_creatorId :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.Text)
documentVersionMetadata_creatorId = Lens.lens (\DocumentVersionMetadata' {creatorId} -> creatorId) (\s@DocumentVersionMetadata' {} a -> s {creatorId = a} :: DocumentVersionMetadata)

-- | The content type of the document.
documentVersionMetadata_contentType :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.Text)
documentVersionMetadata_contentType = Lens.lens (\DocumentVersionMetadata' {contentType} -> contentType) (\s@DocumentVersionMetadata' {} a -> s {contentType = a} :: DocumentVersionMetadata)

-- | The timestamp when the document was first uploaded.
documentVersionMetadata_createdTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.UTCTime)
documentVersionMetadata_createdTimestamp = Lens.lens (\DocumentVersionMetadata' {createdTimestamp} -> createdTimestamp) (\s@DocumentVersionMetadata' {} a -> s {createdTimestamp = a} :: DocumentVersionMetadata) Core.. Lens.mapping Core._Time

-- | The timestamp when the content of the document was modified.
documentVersionMetadata_contentModifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.UTCTime)
documentVersionMetadata_contentModifiedTimestamp = Lens.lens (\DocumentVersionMetadata' {contentModifiedTimestamp} -> contentModifiedTimestamp) (\s@DocumentVersionMetadata' {} a -> s {contentModifiedTimestamp = a} :: DocumentVersionMetadata) Core.. Lens.mapping Core._Time

-- | The ID of the version.
documentVersionMetadata_id :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.Text)
documentVersionMetadata_id = Lens.lens (\DocumentVersionMetadata' {id} -> id) (\s@DocumentVersionMetadata' {} a -> s {id = a} :: DocumentVersionMetadata)

-- | The source of the document.
documentVersionMetadata_source :: Lens.Lens' DocumentVersionMetadata (Core.Maybe (Core.HashMap DocumentSourceType Core.Text))
documentVersionMetadata_source = Lens.lens (\DocumentVersionMetadata' {source} -> source) (\s@DocumentVersionMetadata' {} a -> s {source = a} :: DocumentVersionMetadata) Core.. Lens.mapping Lens._Coerce

-- | The timestamp when the content of the document was originally created.
documentVersionMetadata_contentCreatedTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.UTCTime)
documentVersionMetadata_contentCreatedTimestamp = Lens.lens (\DocumentVersionMetadata' {contentCreatedTimestamp} -> contentCreatedTimestamp) (\s@DocumentVersionMetadata' {} a -> s {contentCreatedTimestamp = a} :: DocumentVersionMetadata) Core.. Lens.mapping Core._Time

-- | The name of the version.
documentVersionMetadata_name :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.Text)
documentVersionMetadata_name = Lens.lens (\DocumentVersionMetadata' {name} -> name) (\s@DocumentVersionMetadata' {} a -> s {name = a} :: DocumentVersionMetadata)

-- | The signature of the document.
documentVersionMetadata_signature :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.Text)
documentVersionMetadata_signature = Lens.lens (\DocumentVersionMetadata' {signature} -> signature) (\s@DocumentVersionMetadata' {} a -> s {signature = a} :: DocumentVersionMetadata)

-- | The thumbnail of the document.
documentVersionMetadata_thumbnail :: Lens.Lens' DocumentVersionMetadata (Core.Maybe (Core.HashMap DocumentThumbnailType Core.Text))
documentVersionMetadata_thumbnail = Lens.lens (\DocumentVersionMetadata' {thumbnail} -> thumbnail) (\s@DocumentVersionMetadata' {} a -> s {thumbnail = a} :: DocumentVersionMetadata) Core.. Lens.mapping Lens._Coerce

-- | The size of the document, in bytes.
documentVersionMetadata_size :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.Integer)
documentVersionMetadata_size = Lens.lens (\DocumentVersionMetadata' {size} -> size) (\s@DocumentVersionMetadata' {} a -> s {size = a} :: DocumentVersionMetadata)

instance Core.FromJSON DocumentVersionMetadata where
  parseJSON =
    Core.withObject
      "DocumentVersionMetadata"
      ( \x ->
          DocumentVersionMetadata'
            Core.<$> (x Core..:? "ModifiedTimestamp")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreatorId")
            Core.<*> (x Core..:? "ContentType")
            Core.<*> (x Core..:? "CreatedTimestamp")
            Core.<*> (x Core..:? "ContentModifiedTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Source" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ContentCreatedTimestamp")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Signature")
            Core.<*> (x Core..:? "Thumbnail" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Size")
      )

instance Core.Hashable DocumentVersionMetadata

instance Core.NFData DocumentVersionMetadata
