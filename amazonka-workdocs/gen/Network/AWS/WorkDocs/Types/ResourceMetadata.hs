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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.ResourceType
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the metadata of a resource.
--
-- /See:/ 'newResourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { -- | The original name of the resource before a rename operation.
    originalName :: Core.Maybe Core.Text,
    -- | The ID of the resource.
    id :: Core.Maybe Core.Text,
    -- | The version ID of the resource. This is an optional field and is filled
    -- for action on document version.
    versionId :: Core.Maybe Core.Text,
    -- | The name of the resource.
    name :: Core.Maybe Core.Text,
    -- | The parent ID of the resource before a rename operation.
    parentId :: Core.Maybe Core.Text,
    -- | The owner of the resource.
    owner :: Core.Maybe UserMetadata,
    -- | The type of resource.
    type' :: Core.Maybe ResourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { originalName = Core.Nothing,
      id = Core.Nothing,
      versionId = Core.Nothing,
      name = Core.Nothing,
      parentId = Core.Nothing,
      owner = Core.Nothing,
      type' = Core.Nothing
    }

-- | The original name of the resource before a rename operation.
resourceMetadata_originalName :: Lens.Lens' ResourceMetadata (Core.Maybe Core.Text)
resourceMetadata_originalName = Lens.lens (\ResourceMetadata' {originalName} -> originalName) (\s@ResourceMetadata' {} a -> s {originalName = a} :: ResourceMetadata)

-- | The ID of the resource.
resourceMetadata_id :: Lens.Lens' ResourceMetadata (Core.Maybe Core.Text)
resourceMetadata_id = Lens.lens (\ResourceMetadata' {id} -> id) (\s@ResourceMetadata' {} a -> s {id = a} :: ResourceMetadata)

-- | The version ID of the resource. This is an optional field and is filled
-- for action on document version.
resourceMetadata_versionId :: Lens.Lens' ResourceMetadata (Core.Maybe Core.Text)
resourceMetadata_versionId = Lens.lens (\ResourceMetadata' {versionId} -> versionId) (\s@ResourceMetadata' {} a -> s {versionId = a} :: ResourceMetadata)

-- | The name of the resource.
resourceMetadata_name :: Lens.Lens' ResourceMetadata (Core.Maybe Core.Text)
resourceMetadata_name = Lens.lens (\ResourceMetadata' {name} -> name) (\s@ResourceMetadata' {} a -> s {name = a} :: ResourceMetadata)

-- | The parent ID of the resource before a rename operation.
resourceMetadata_parentId :: Lens.Lens' ResourceMetadata (Core.Maybe Core.Text)
resourceMetadata_parentId = Lens.lens (\ResourceMetadata' {parentId} -> parentId) (\s@ResourceMetadata' {} a -> s {parentId = a} :: ResourceMetadata)

-- | The owner of the resource.
resourceMetadata_owner :: Lens.Lens' ResourceMetadata (Core.Maybe UserMetadata)
resourceMetadata_owner = Lens.lens (\ResourceMetadata' {owner} -> owner) (\s@ResourceMetadata' {} a -> s {owner = a} :: ResourceMetadata)

-- | The type of resource.
resourceMetadata_type :: Lens.Lens' ResourceMetadata (Core.Maybe ResourceType)
resourceMetadata_type = Lens.lens (\ResourceMetadata' {type'} -> type') (\s@ResourceMetadata' {} a -> s {type' = a} :: ResourceMetadata)

instance Core.FromJSON ResourceMetadata where
  parseJSON =
    Core.withObject
      "ResourceMetadata"
      ( \x ->
          ResourceMetadata'
            Core.<$> (x Core..:? "OriginalName")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "VersionId")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "ParentId")
            Core.<*> (x Core..:? "Owner")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ResourceMetadata

instance Core.NFData ResourceMetadata
