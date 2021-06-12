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
-- Module      : Network.AWS.Config.Types.ResourceKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceKey where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details that identify a resource within AWS Config, including the
-- resource type and resource ID.
--
-- /See:/ 'newResourceKey' smart constructor.
data ResourceKey = ResourceKey'
  { -- | The resource type.
    resourceType :: ResourceType,
    -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resourceKey_resourceType' - The resource type.
--
-- 'resourceId', 'resourceKey_resourceId' - The ID of the resource (for example., sg-xxxxxx).
newResourceKey ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'resourceId'
  Core.Text ->
  ResourceKey
newResourceKey pResourceType_ pResourceId_ =
  ResourceKey'
    { resourceType = pResourceType_,
      resourceId = pResourceId_
    }

-- | The resource type.
resourceKey_resourceType :: Lens.Lens' ResourceKey ResourceType
resourceKey_resourceType = Lens.lens (\ResourceKey' {resourceType} -> resourceType) (\s@ResourceKey' {} a -> s {resourceType = a} :: ResourceKey)

-- | The ID of the resource (for example., sg-xxxxxx).
resourceKey_resourceId :: Lens.Lens' ResourceKey Core.Text
resourceKey_resourceId = Lens.lens (\ResourceKey' {resourceId} -> resourceId) (\s@ResourceKey' {} a -> s {resourceId = a} :: ResourceKey)

instance Core.FromJSON ResourceKey where
  parseJSON =
    Core.withObject
      "ResourceKey"
      ( \x ->
          ResourceKey'
            Core.<$> (x Core..: "resourceType")
            Core.<*> (x Core..: "resourceId")
      )

instance Core.Hashable ResourceKey

instance Core.NFData ResourceKey

instance Core.ToJSON ResourceKey where
  toJSON ResourceKey' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceType" Core..= resourceType),
            Core.Just ("resourceId" Core..= resourceId)
          ]
      )
