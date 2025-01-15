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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ResourceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.ResourceType
import Amazonka.WorkDocs.Types.UserMetadata

-- | Describes the metadata of a resource.
--
-- /See:/ 'newResourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { -- | The ID of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The original name of the resource before a rename operation.
    originalName :: Prelude.Maybe Prelude.Text,
    -- | The owner of the resource.
    owner :: Prelude.Maybe UserMetadata,
    -- | The parent ID of the resource before a rename operation.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource.
    type' :: Prelude.Maybe ResourceType,
    -- | The version ID of the resource. This is an optional field and is filled
    -- for action on document version.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'resourceMetadata_id' - The ID of the resource.
--
-- 'name', 'resourceMetadata_name' - The name of the resource.
--
-- 'originalName', 'resourceMetadata_originalName' - The original name of the resource before a rename operation.
--
-- 'owner', 'resourceMetadata_owner' - The owner of the resource.
--
-- 'parentId', 'resourceMetadata_parentId' - The parent ID of the resource before a rename operation.
--
-- 'type'', 'resourceMetadata_type' - The type of resource.
--
-- 'versionId', 'resourceMetadata_versionId' - The version ID of the resource. This is an optional field and is filled
-- for action on document version.
newResourceMetadata ::
  ResourceMetadata
newResourceMetadata =
  ResourceMetadata'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      originalName = Prelude.Nothing,
      owner = Prelude.Nothing,
      parentId = Prelude.Nothing,
      type' = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The ID of the resource.
resourceMetadata_id :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_id = Lens.lens (\ResourceMetadata' {id} -> id) (\s@ResourceMetadata' {} a -> s {id = a} :: ResourceMetadata)

-- | The name of the resource.
resourceMetadata_name :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_name = Lens.lens (\ResourceMetadata' {name} -> name) (\s@ResourceMetadata' {} a -> s {name = a} :: ResourceMetadata)

-- | The original name of the resource before a rename operation.
resourceMetadata_originalName :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_originalName = Lens.lens (\ResourceMetadata' {originalName} -> originalName) (\s@ResourceMetadata' {} a -> s {originalName = a} :: ResourceMetadata)

-- | The owner of the resource.
resourceMetadata_owner :: Lens.Lens' ResourceMetadata (Prelude.Maybe UserMetadata)
resourceMetadata_owner = Lens.lens (\ResourceMetadata' {owner} -> owner) (\s@ResourceMetadata' {} a -> s {owner = a} :: ResourceMetadata)

-- | The parent ID of the resource before a rename operation.
resourceMetadata_parentId :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_parentId = Lens.lens (\ResourceMetadata' {parentId} -> parentId) (\s@ResourceMetadata' {} a -> s {parentId = a} :: ResourceMetadata)

-- | The type of resource.
resourceMetadata_type :: Lens.Lens' ResourceMetadata (Prelude.Maybe ResourceType)
resourceMetadata_type = Lens.lens (\ResourceMetadata' {type'} -> type') (\s@ResourceMetadata' {} a -> s {type' = a} :: ResourceMetadata)

-- | The version ID of the resource. This is an optional field and is filled
-- for action on document version.
resourceMetadata_versionId :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_versionId = Lens.lens (\ResourceMetadata' {versionId} -> versionId) (\s@ResourceMetadata' {} a -> s {versionId = a} :: ResourceMetadata)

instance Data.FromJSON ResourceMetadata where
  parseJSON =
    Data.withObject
      "ResourceMetadata"
      ( \x ->
          ResourceMetadata'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OriginalName")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "ParentId")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "VersionId")
      )

instance Prelude.Hashable ResourceMetadata where
  hashWithSalt _salt ResourceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` originalName
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData ResourceMetadata where
  rnf ResourceMetadata' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf originalName `Prelude.seq`
          Prelude.rnf owner `Prelude.seq`
            Prelude.rnf parentId `Prelude.seq`
              Prelude.rnf type' `Prelude.seq`
                Prelude.rnf versionId
