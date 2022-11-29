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
-- Module      : Amazonka.WorkDocs.Types.DocumentVersionMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.DocumentVersionMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.DocumentSourceType
import Amazonka.WorkDocs.Types.DocumentStatusType
import Amazonka.WorkDocs.Types.DocumentThumbnailType

-- | Describes a version of a document.
--
-- /See:/ 'newDocumentVersionMetadata' smart constructor.
data DocumentVersionMetadata = DocumentVersionMetadata'
  { -- | The ID of the creator.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | The name of the version.
    name :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the document was first uploaded.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The thumbnail of the document.
    thumbnail :: Prelude.Maybe (Prelude.HashMap DocumentThumbnailType (Core.Sensitive Prelude.Text)),
    -- | The size of the document, in bytes.
    size :: Prelude.Maybe Prelude.Integer,
    -- | The timestamp when the content of the document was originally created.
    contentCreatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status of the document.
    status :: Prelude.Maybe DocumentStatusType,
    -- | The ID of the version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the content of the document was modified.
    contentModifiedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The source of the document.
    source :: Prelude.Maybe (Prelude.HashMap DocumentSourceType (Core.Sensitive Prelude.Text)),
    -- | The signature of the document.
    signature :: Prelude.Maybe Prelude.Text,
    -- | The content type of the document.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the document was last uploaded.
    modifiedTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentVersionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorId', 'documentVersionMetadata_creatorId' - The ID of the creator.
--
-- 'name', 'documentVersionMetadata_name' - The name of the version.
--
-- 'createdTimestamp', 'documentVersionMetadata_createdTimestamp' - The timestamp when the document was first uploaded.
--
-- 'thumbnail', 'documentVersionMetadata_thumbnail' - The thumbnail of the document.
--
-- 'size', 'documentVersionMetadata_size' - The size of the document, in bytes.
--
-- 'contentCreatedTimestamp', 'documentVersionMetadata_contentCreatedTimestamp' - The timestamp when the content of the document was originally created.
--
-- 'status', 'documentVersionMetadata_status' - The status of the document.
--
-- 'id', 'documentVersionMetadata_id' - The ID of the version.
--
-- 'contentModifiedTimestamp', 'documentVersionMetadata_contentModifiedTimestamp' - The timestamp when the content of the document was modified.
--
-- 'source', 'documentVersionMetadata_source' - The source of the document.
--
-- 'signature', 'documentVersionMetadata_signature' - The signature of the document.
--
-- 'contentType', 'documentVersionMetadata_contentType' - The content type of the document.
--
-- 'modifiedTimestamp', 'documentVersionMetadata_modifiedTimestamp' - The timestamp when the document was last uploaded.
newDocumentVersionMetadata ::
  DocumentVersionMetadata
newDocumentVersionMetadata =
  DocumentVersionMetadata'
    { creatorId =
        Prelude.Nothing,
      name = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      thumbnail = Prelude.Nothing,
      size = Prelude.Nothing,
      contentCreatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      contentModifiedTimestamp = Prelude.Nothing,
      source = Prelude.Nothing,
      signature = Prelude.Nothing,
      contentType = Prelude.Nothing,
      modifiedTimestamp = Prelude.Nothing
    }

-- | The ID of the creator.
documentVersionMetadata_creatorId :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.Text)
documentVersionMetadata_creatorId = Lens.lens (\DocumentVersionMetadata' {creatorId} -> creatorId) (\s@DocumentVersionMetadata' {} a -> s {creatorId = a} :: DocumentVersionMetadata)

-- | The name of the version.
documentVersionMetadata_name :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.Text)
documentVersionMetadata_name = Lens.lens (\DocumentVersionMetadata' {name} -> name) (\s@DocumentVersionMetadata' {} a -> s {name = a} :: DocumentVersionMetadata)

-- | The timestamp when the document was first uploaded.
documentVersionMetadata_createdTimestamp :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.UTCTime)
documentVersionMetadata_createdTimestamp = Lens.lens (\DocumentVersionMetadata' {createdTimestamp} -> createdTimestamp) (\s@DocumentVersionMetadata' {} a -> s {createdTimestamp = a} :: DocumentVersionMetadata) Prelude.. Lens.mapping Core._Time

-- | The thumbnail of the document.
documentVersionMetadata_thumbnail :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe (Prelude.HashMap DocumentThumbnailType Prelude.Text))
documentVersionMetadata_thumbnail = Lens.lens (\DocumentVersionMetadata' {thumbnail} -> thumbnail) (\s@DocumentVersionMetadata' {} a -> s {thumbnail = a} :: DocumentVersionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The size of the document, in bytes.
documentVersionMetadata_size :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.Integer)
documentVersionMetadata_size = Lens.lens (\DocumentVersionMetadata' {size} -> size) (\s@DocumentVersionMetadata' {} a -> s {size = a} :: DocumentVersionMetadata)

-- | The timestamp when the content of the document was originally created.
documentVersionMetadata_contentCreatedTimestamp :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.UTCTime)
documentVersionMetadata_contentCreatedTimestamp = Lens.lens (\DocumentVersionMetadata' {contentCreatedTimestamp} -> contentCreatedTimestamp) (\s@DocumentVersionMetadata' {} a -> s {contentCreatedTimestamp = a} :: DocumentVersionMetadata) Prelude.. Lens.mapping Core._Time

-- | The status of the document.
documentVersionMetadata_status :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe DocumentStatusType)
documentVersionMetadata_status = Lens.lens (\DocumentVersionMetadata' {status} -> status) (\s@DocumentVersionMetadata' {} a -> s {status = a} :: DocumentVersionMetadata)

-- | The ID of the version.
documentVersionMetadata_id :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.Text)
documentVersionMetadata_id = Lens.lens (\DocumentVersionMetadata' {id} -> id) (\s@DocumentVersionMetadata' {} a -> s {id = a} :: DocumentVersionMetadata)

-- | The timestamp when the content of the document was modified.
documentVersionMetadata_contentModifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.UTCTime)
documentVersionMetadata_contentModifiedTimestamp = Lens.lens (\DocumentVersionMetadata' {contentModifiedTimestamp} -> contentModifiedTimestamp) (\s@DocumentVersionMetadata' {} a -> s {contentModifiedTimestamp = a} :: DocumentVersionMetadata) Prelude.. Lens.mapping Core._Time

-- | The source of the document.
documentVersionMetadata_source :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe (Prelude.HashMap DocumentSourceType Prelude.Text))
documentVersionMetadata_source = Lens.lens (\DocumentVersionMetadata' {source} -> source) (\s@DocumentVersionMetadata' {} a -> s {source = a} :: DocumentVersionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The signature of the document.
documentVersionMetadata_signature :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.Text)
documentVersionMetadata_signature = Lens.lens (\DocumentVersionMetadata' {signature} -> signature) (\s@DocumentVersionMetadata' {} a -> s {signature = a} :: DocumentVersionMetadata)

-- | The content type of the document.
documentVersionMetadata_contentType :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.Text)
documentVersionMetadata_contentType = Lens.lens (\DocumentVersionMetadata' {contentType} -> contentType) (\s@DocumentVersionMetadata' {} a -> s {contentType = a} :: DocumentVersionMetadata)

-- | The timestamp when the document was last uploaded.
documentVersionMetadata_modifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Prelude.Maybe Prelude.UTCTime)
documentVersionMetadata_modifiedTimestamp = Lens.lens (\DocumentVersionMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@DocumentVersionMetadata' {} a -> s {modifiedTimestamp = a} :: DocumentVersionMetadata) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DocumentVersionMetadata where
  parseJSON =
    Core.withObject
      "DocumentVersionMetadata"
      ( \x ->
          DocumentVersionMetadata'
            Prelude.<$> (x Core..:? "CreatorId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "Thumbnail" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Size")
            Prelude.<*> (x Core..:? "ContentCreatedTimestamp")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "ContentModifiedTimestamp")
            Prelude.<*> (x Core..:? "Source" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Signature")
            Prelude.<*> (x Core..:? "ContentType")
            Prelude.<*> (x Core..:? "ModifiedTimestamp")
      )

instance Prelude.Hashable DocumentVersionMetadata where
  hashWithSalt _salt DocumentVersionMetadata' {..} =
    _salt `Prelude.hashWithSalt` creatorId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` thumbnail
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` contentCreatedTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` contentModifiedTimestamp
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` signature
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` modifiedTimestamp

instance Prelude.NFData DocumentVersionMetadata where
  rnf DocumentVersionMetadata' {..} =
    Prelude.rnf creatorId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf thumbnail
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf contentCreatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf contentModifiedTimestamp
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf signature
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf modifiedTimestamp
