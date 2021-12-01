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
-- Module      : Amazonka.WorkDocs.Types.ResourceMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ResourceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.ResourceType
import Amazonka.WorkDocs.Types.UserMetadata

-- | Describes the metadata of a resource.
--
-- /See:/ 'newResourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { -- | The version ID of the resource. This is an optional field and is filled
    -- for action on document version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the resource.
    owner :: Prelude.Maybe UserMetadata,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of resource.
    type' :: Prelude.Maybe ResourceType,
    -- | The original name of the resource before a rename operation.
    originalName :: Prelude.Maybe Prelude.Text,
    -- | The parent ID of the resource before a rename operation.
    parentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'resourceMetadata_versionId' - The version ID of the resource. This is an optional field and is filled
-- for action on document version.
--
-- 'owner', 'resourceMetadata_owner' - The owner of the resource.
--
-- 'name', 'resourceMetadata_name' - The name of the resource.
--
-- 'id', 'resourceMetadata_id' - The ID of the resource.
--
-- 'type'', 'resourceMetadata_type' - The type of resource.
--
-- 'originalName', 'resourceMetadata_originalName' - The original name of the resource before a rename operation.
--
-- 'parentId', 'resourceMetadata_parentId' - The parent ID of the resource before a rename operation.
newResourceMetadata ::
  ResourceMetadata
newResourceMetadata =
  ResourceMetadata'
    { versionId = Prelude.Nothing,
      owner = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      originalName = Prelude.Nothing,
      parentId = Prelude.Nothing
    }

-- | The version ID of the resource. This is an optional field and is filled
-- for action on document version.
resourceMetadata_versionId :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_versionId = Lens.lens (\ResourceMetadata' {versionId} -> versionId) (\s@ResourceMetadata' {} a -> s {versionId = a} :: ResourceMetadata)

-- | The owner of the resource.
resourceMetadata_owner :: Lens.Lens' ResourceMetadata (Prelude.Maybe UserMetadata)
resourceMetadata_owner = Lens.lens (\ResourceMetadata' {owner} -> owner) (\s@ResourceMetadata' {} a -> s {owner = a} :: ResourceMetadata)

-- | The name of the resource.
resourceMetadata_name :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_name = Lens.lens (\ResourceMetadata' {name} -> name) (\s@ResourceMetadata' {} a -> s {name = a} :: ResourceMetadata)

-- | The ID of the resource.
resourceMetadata_id :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_id = Lens.lens (\ResourceMetadata' {id} -> id) (\s@ResourceMetadata' {} a -> s {id = a} :: ResourceMetadata)

-- | The type of resource.
resourceMetadata_type :: Lens.Lens' ResourceMetadata (Prelude.Maybe ResourceType)
resourceMetadata_type = Lens.lens (\ResourceMetadata' {type'} -> type') (\s@ResourceMetadata' {} a -> s {type' = a} :: ResourceMetadata)

-- | The original name of the resource before a rename operation.
resourceMetadata_originalName :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_originalName = Lens.lens (\ResourceMetadata' {originalName} -> originalName) (\s@ResourceMetadata' {} a -> s {originalName = a} :: ResourceMetadata)

-- | The parent ID of the resource before a rename operation.
resourceMetadata_parentId :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_parentId = Lens.lens (\ResourceMetadata' {parentId} -> parentId) (\s@ResourceMetadata' {} a -> s {parentId = a} :: ResourceMetadata)

instance Core.FromJSON ResourceMetadata where
  parseJSON =
    Core.withObject
      "ResourceMetadata"
      ( \x ->
          ResourceMetadata'
            Prelude.<$> (x Core..:? "VersionId")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "OriginalName")
            Prelude.<*> (x Core..:? "ParentId")
      )

instance Prelude.Hashable ResourceMetadata where
  hashWithSalt salt' ResourceMetadata' {..} =
    salt' `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` originalName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData ResourceMetadata where
  rnf ResourceMetadata' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf originalName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
