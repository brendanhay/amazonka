{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.DocumentVersionMetadata
import Network.AWS.WorkDocs.Types.ResourceStateType

-- | Describes the document.
--
-- /See:/ 'newDocumentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { -- | The time when the document was updated.
    modifiedTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the creator.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | The time when the document was created.
    createdTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the document.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of labels on the document.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The latest version of the document.
    latestVersionMetadata :: Prelude.Maybe DocumentVersionMetadata,
    -- | The resource state.
    resourceState :: Prelude.Maybe ResourceStateType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { modifiedTimestamp =
        Prelude.Nothing,
      parentFolderId = Prelude.Nothing,
      creatorId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      labels = Prelude.Nothing,
      latestVersionMetadata = Prelude.Nothing,
      resourceState = Prelude.Nothing
    }

-- | The time when the document was updated.
documentMetadata_modifiedTimestamp :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.UTCTime)
documentMetadata_modifiedTimestamp = Lens.lens (\DocumentMetadata' {modifiedTimestamp} -> modifiedTimestamp) (\s@DocumentMetadata' {} a -> s {modifiedTimestamp = a} :: DocumentMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the parent folder.
documentMetadata_parentFolderId :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_parentFolderId = Lens.lens (\DocumentMetadata' {parentFolderId} -> parentFolderId) (\s@DocumentMetadata' {} a -> s {parentFolderId = a} :: DocumentMetadata)

-- | The ID of the creator.
documentMetadata_creatorId :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_creatorId = Lens.lens (\DocumentMetadata' {creatorId} -> creatorId) (\s@DocumentMetadata' {} a -> s {creatorId = a} :: DocumentMetadata)

-- | The time when the document was created.
documentMetadata_createdTimestamp :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.UTCTime)
documentMetadata_createdTimestamp = Lens.lens (\DocumentMetadata' {createdTimestamp} -> createdTimestamp) (\s@DocumentMetadata' {} a -> s {createdTimestamp = a} :: DocumentMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the document.
documentMetadata_id :: Lens.Lens' DocumentMetadata (Prelude.Maybe Prelude.Text)
documentMetadata_id = Lens.lens (\DocumentMetadata' {id} -> id) (\s@DocumentMetadata' {} a -> s {id = a} :: DocumentMetadata)

-- | List of labels on the document.
documentMetadata_labels :: Lens.Lens' DocumentMetadata (Prelude.Maybe [Prelude.Text])
documentMetadata_labels = Lens.lens (\DocumentMetadata' {labels} -> labels) (\s@DocumentMetadata' {} a -> s {labels = a} :: DocumentMetadata) Prelude.. Lens.mapping Prelude._Coerce

-- | The latest version of the document.
documentMetadata_latestVersionMetadata :: Lens.Lens' DocumentMetadata (Prelude.Maybe DocumentVersionMetadata)
documentMetadata_latestVersionMetadata = Lens.lens (\DocumentMetadata' {latestVersionMetadata} -> latestVersionMetadata) (\s@DocumentMetadata' {} a -> s {latestVersionMetadata = a} :: DocumentMetadata)

-- | The resource state.
documentMetadata_resourceState :: Lens.Lens' DocumentMetadata (Prelude.Maybe ResourceStateType)
documentMetadata_resourceState = Lens.lens (\DocumentMetadata' {resourceState} -> resourceState) (\s@DocumentMetadata' {} a -> s {resourceState = a} :: DocumentMetadata)

instance Prelude.FromJSON DocumentMetadata where
  parseJSON =
    Prelude.withObject
      "DocumentMetadata"
      ( \x ->
          DocumentMetadata'
            Prelude.<$> (x Prelude..:? "ModifiedTimestamp")
            Prelude.<*> (x Prelude..:? "ParentFolderId")
            Prelude.<*> (x Prelude..:? "CreatorId")
            Prelude.<*> (x Prelude..:? "CreatedTimestamp")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Labels" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LatestVersionMetadata")
            Prelude.<*> (x Prelude..:? "ResourceState")
      )

instance Prelude.Hashable DocumentMetadata

instance Prelude.NFData DocumentMetadata
