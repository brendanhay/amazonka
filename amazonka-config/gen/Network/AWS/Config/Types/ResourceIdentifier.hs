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
-- Module      : Network.AWS.Config.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceIdentifier where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details that identify a resource that is discovered by AWS Config,
-- including the resource type, ID, and (if available) the custom resource
-- name.
--
-- /See:/ 'newResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The ID of the resource (for example, @sg-xxxxxx@).
    resourceId :: Core.Maybe Core.Text,
    -- | The type of resource.
    resourceType :: Core.Maybe ResourceType,
    -- | The time that the resource was deleted.
    resourceDeletionTime :: Core.Maybe Core.POSIX,
    -- | The custom name of the resource (if available).
    resourceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'resourceIdentifier_resourceId' - The ID of the resource (for example, @sg-xxxxxx@).
--
-- 'resourceType', 'resourceIdentifier_resourceType' - The type of resource.
--
-- 'resourceDeletionTime', 'resourceIdentifier_resourceDeletionTime' - The time that the resource was deleted.
--
-- 'resourceName', 'resourceIdentifier_resourceName' - The custom name of the resource (if available).
newResourceIdentifier ::
  ResourceIdentifier
newResourceIdentifier =
  ResourceIdentifier'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      resourceDeletionTime = Core.Nothing,
      resourceName = Core.Nothing
    }

-- | The ID of the resource (for example, @sg-xxxxxx@).
resourceIdentifier_resourceId :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_resourceId = Lens.lens (\ResourceIdentifier' {resourceId} -> resourceId) (\s@ResourceIdentifier' {} a -> s {resourceId = a} :: ResourceIdentifier)

-- | The type of resource.
resourceIdentifier_resourceType :: Lens.Lens' ResourceIdentifier (Core.Maybe ResourceType)
resourceIdentifier_resourceType = Lens.lens (\ResourceIdentifier' {resourceType} -> resourceType) (\s@ResourceIdentifier' {} a -> s {resourceType = a} :: ResourceIdentifier)

-- | The time that the resource was deleted.
resourceIdentifier_resourceDeletionTime :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.UTCTime)
resourceIdentifier_resourceDeletionTime = Lens.lens (\ResourceIdentifier' {resourceDeletionTime} -> resourceDeletionTime) (\s@ResourceIdentifier' {} a -> s {resourceDeletionTime = a} :: ResourceIdentifier) Core.. Lens.mapping Core._Time

-- | The custom name of the resource (if available).
resourceIdentifier_resourceName :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_resourceName = Lens.lens (\ResourceIdentifier' {resourceName} -> resourceName) (\s@ResourceIdentifier' {} a -> s {resourceName = a} :: ResourceIdentifier)

instance Core.FromJSON ResourceIdentifier where
  parseJSON =
    Core.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Core.<$> (x Core..:? "resourceId")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "resourceDeletionTime")
            Core.<*> (x Core..:? "resourceName")
      )

instance Core.Hashable ResourceIdentifier

instance Core.NFData ResourceIdentifier
