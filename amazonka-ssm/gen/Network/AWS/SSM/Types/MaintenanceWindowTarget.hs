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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.MaintenanceWindowResourceType
import Network.AWS.SSM.Types.Target

-- | The target registered with the maintenance window.
--
-- /See:/ 'newMaintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
  { -- | The ID of the target.
    windowTargetId :: Core.Maybe Core.Text,
    -- | The type of target that is being registered with the maintenance window.
    resourceType :: Core.Maybe MaintenanceWindowResourceType,
    -- | The targets, either instances or tags.
    --
    -- Specify instances using the following format:
    --
    -- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
    --
    -- Tags are specified using the following format:
    --
    -- @Key=\<tag name>,Values=\<tag value>@.
    targets :: Core.Maybe [Target],
    -- | The name for the maintenance window target.
    name :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window to register the target with.
    windowId :: Core.Maybe Core.Text,
    -- | A description for the target.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A user-provided value that will be included in any CloudWatch events
    -- that are raised while running tasks for these targets in this
    -- maintenance window.
    ownerInformation :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaintenanceWindowTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowTargetId', 'maintenanceWindowTarget_windowTargetId' - The ID of the target.
--
-- 'resourceType', 'maintenanceWindowTarget_resourceType' - The type of target that is being registered with the maintenance window.
--
-- 'targets', 'maintenanceWindowTarget_targets' - The targets, either instances or tags.
--
-- Specify instances using the following format:
--
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
--
-- Tags are specified using the following format:
--
-- @Key=\<tag name>,Values=\<tag value>@.
--
-- 'name', 'maintenanceWindowTarget_name' - The name for the maintenance window target.
--
-- 'windowId', 'maintenanceWindowTarget_windowId' - The ID of the maintenance window to register the target with.
--
-- 'description', 'maintenanceWindowTarget_description' - A description for the target.
--
-- 'ownerInformation', 'maintenanceWindowTarget_ownerInformation' - A user-provided value that will be included in any CloudWatch events
-- that are raised while running tasks for these targets in this
-- maintenance window.
newMaintenanceWindowTarget ::
  MaintenanceWindowTarget
newMaintenanceWindowTarget =
  MaintenanceWindowTarget'
    { windowTargetId =
        Core.Nothing,
      resourceType = Core.Nothing,
      targets = Core.Nothing,
      name = Core.Nothing,
      windowId = Core.Nothing,
      description = Core.Nothing,
      ownerInformation = Core.Nothing
    }

-- | The ID of the target.
maintenanceWindowTarget_windowTargetId :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Core.Text)
maintenanceWindowTarget_windowTargetId = Lens.lens (\MaintenanceWindowTarget' {windowTargetId} -> windowTargetId) (\s@MaintenanceWindowTarget' {} a -> s {windowTargetId = a} :: MaintenanceWindowTarget)

-- | The type of target that is being registered with the maintenance window.
maintenanceWindowTarget_resourceType :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe MaintenanceWindowResourceType)
maintenanceWindowTarget_resourceType = Lens.lens (\MaintenanceWindowTarget' {resourceType} -> resourceType) (\s@MaintenanceWindowTarget' {} a -> s {resourceType = a} :: MaintenanceWindowTarget)

-- | The targets, either instances or tags.
--
-- Specify instances using the following format:
--
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
--
-- Tags are specified using the following format:
--
-- @Key=\<tag name>,Values=\<tag value>@.
maintenanceWindowTarget_targets :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe [Target])
maintenanceWindowTarget_targets = Lens.lens (\MaintenanceWindowTarget' {targets} -> targets) (\s@MaintenanceWindowTarget' {} a -> s {targets = a} :: MaintenanceWindowTarget) Core.. Lens.mapping Lens._Coerce

-- | The name for the maintenance window target.
maintenanceWindowTarget_name :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Core.Text)
maintenanceWindowTarget_name = Lens.lens (\MaintenanceWindowTarget' {name} -> name) (\s@MaintenanceWindowTarget' {} a -> s {name = a} :: MaintenanceWindowTarget)

-- | The ID of the maintenance window to register the target with.
maintenanceWindowTarget_windowId :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Core.Text)
maintenanceWindowTarget_windowId = Lens.lens (\MaintenanceWindowTarget' {windowId} -> windowId) (\s@MaintenanceWindowTarget' {} a -> s {windowId = a} :: MaintenanceWindowTarget)

-- | A description for the target.
maintenanceWindowTarget_description :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Core.Text)
maintenanceWindowTarget_description = Lens.lens (\MaintenanceWindowTarget' {description} -> description) (\s@MaintenanceWindowTarget' {} a -> s {description = a} :: MaintenanceWindowTarget) Core.. Lens.mapping Core._Sensitive

-- | A user-provided value that will be included in any CloudWatch events
-- that are raised while running tasks for these targets in this
-- maintenance window.
maintenanceWindowTarget_ownerInformation :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Core.Text)
maintenanceWindowTarget_ownerInformation = Lens.lens (\MaintenanceWindowTarget' {ownerInformation} -> ownerInformation) (\s@MaintenanceWindowTarget' {} a -> s {ownerInformation = a} :: MaintenanceWindowTarget) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON MaintenanceWindowTarget where
  parseJSON =
    Core.withObject
      "MaintenanceWindowTarget"
      ( \x ->
          MaintenanceWindowTarget'
            Core.<$> (x Core..:? "WindowTargetId")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (x Core..:? "Targets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "WindowId")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "OwnerInformation")
      )

instance Core.Hashable MaintenanceWindowTarget

instance Core.NFData MaintenanceWindowTarget
