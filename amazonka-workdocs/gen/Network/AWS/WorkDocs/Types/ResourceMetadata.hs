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
-- Module      : Network.AWS.WorkDocs.Types.ResourceMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourceMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.ResourceType
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the metadata of a resource.
--
-- /See:/ 'newResourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { -- | The original name of the resource before a rename operation.
    originalName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The version ID of the resource. This is an optional field and is filled
    -- for action on document version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The parent ID of the resource before a rename operation.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the resource.
    owner :: Prelude.Maybe UserMetadata,
    -- | The type of resource.
    type' :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originalName', 'resourceMetadata_originalName' - The original name of the resource before a rename operation.
--
-- 'id', 'resourceMetadata_id' - The ID of the resource.
--
-- 'versionId', 'resourceMetadata_versionId' - The version ID of the resource. This is an optional field and is filled
-- for action on document version.
--
-- 'name', 'resourceMetadata_name' - The name of the resource.
--
-- 'parentId', 'resourceMetadata_parentId' - The parent ID of the resource before a rename operation.
--
-- 'owner', 'resourceMetadata_owner' - The owner of the resource.
--
-- 'type'', 'resourceMetadata_type' - The type of resource.
newResourceMetadata ::
  ResourceMetadata
newResourceMetadata =
  ResourceMetadata'
    { originalName = Prelude.Nothing,
      id = Prelude.Nothing,
      versionId = Prelude.Nothing,
      name = Prelude.Nothing,
      parentId = Prelude.Nothing,
      owner = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The original name of the resource before a rename operation.
resourceMetadata_originalName :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_originalName = Lens.lens (\ResourceMetadata' {originalName} -> originalName) (\s@ResourceMetadata' {} a -> s {originalName = a} :: ResourceMetadata)

-- | The ID of the resource.
resourceMetadata_id :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_id = Lens.lens (\ResourceMetadata' {id} -> id) (\s@ResourceMetadata' {} a -> s {id = a} :: ResourceMetadata)

-- | The version ID of the resource. This is an optional field and is filled
-- for action on document version.
resourceMetadata_versionId :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_versionId = Lens.lens (\ResourceMetadata' {versionId} -> versionId) (\s@ResourceMetadata' {} a -> s {versionId = a} :: ResourceMetadata)

-- | The name of the resource.
resourceMetadata_name :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_name = Lens.lens (\ResourceMetadata' {name} -> name) (\s@ResourceMetadata' {} a -> s {name = a} :: ResourceMetadata)

-- | The parent ID of the resource before a rename operation.
resourceMetadata_parentId :: Lens.Lens' ResourceMetadata (Prelude.Maybe Prelude.Text)
resourceMetadata_parentId = Lens.lens (\ResourceMetadata' {parentId} -> parentId) (\s@ResourceMetadata' {} a -> s {parentId = a} :: ResourceMetadata)

-- | The owner of the resource.
resourceMetadata_owner :: Lens.Lens' ResourceMetadata (Prelude.Maybe UserMetadata)
resourceMetadata_owner = Lens.lens (\ResourceMetadata' {owner} -> owner) (\s@ResourceMetadata' {} a -> s {owner = a} :: ResourceMetadata)

-- | The type of resource.
resourceMetadata_type :: Lens.Lens' ResourceMetadata (Prelude.Maybe ResourceType)
resourceMetadata_type = Lens.lens (\ResourceMetadata' {type'} -> type') (\s@ResourceMetadata' {} a -> s {type' = a} :: ResourceMetadata)

instance Prelude.FromJSON ResourceMetadata where
  parseJSON =
    Prelude.withObject
      "ResourceMetadata"
      ( \x ->
          ResourceMetadata'
            Prelude.<$> (x Prelude..:? "OriginalName")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "VersionId")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "ParentId")
            Prelude.<*> (x Prelude..:? "Owner")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable ResourceMetadata

instance Prelude.NFData ResourceMetadata
