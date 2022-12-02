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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.MaintenanceWindowResourceType
import Amazonka.SSM.Types.Target

-- | The target registered with the maintenance window.
--
-- /See:/ 'newMaintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
  { -- | The ID of the target.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The type of target that is being registered with the maintenance window.
    resourceType :: Prelude.Maybe MaintenanceWindowResourceType,
    -- | The name for the maintenance window target.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window to register the target with.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The targets, either managed nodes or tags.
    --
    -- Specify managed nodes using the following format:
    --
    -- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
    --
    -- Tags are specified using the following format:
    --
    -- @Key=\<tag name>,Values=\<tag value>@.
    targets :: Prelude.Maybe [Target],
    -- | A description for the target.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A user-provided value that will be included in any Amazon CloudWatch
    -- Events events that are raised while running tasks for these targets in
    -- this maintenance window.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'name', 'maintenanceWindowTarget_name' - The name for the maintenance window target.
--
-- 'windowId', 'maintenanceWindowTarget_windowId' - The ID of the maintenance window to register the target with.
--
-- 'targets', 'maintenanceWindowTarget_targets' - The targets, either managed nodes or tags.
--
-- Specify managed nodes using the following format:
--
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
--
-- Tags are specified using the following format:
--
-- @Key=\<tag name>,Values=\<tag value>@.
--
-- 'description', 'maintenanceWindowTarget_description' - A description for the target.
--
-- 'ownerInformation', 'maintenanceWindowTarget_ownerInformation' - A user-provided value that will be included in any Amazon CloudWatch
-- Events events that are raised while running tasks for these targets in
-- this maintenance window.
newMaintenanceWindowTarget ::
  MaintenanceWindowTarget
newMaintenanceWindowTarget =
  MaintenanceWindowTarget'
    { windowTargetId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      windowId = Prelude.Nothing,
      targets = Prelude.Nothing,
      description = Prelude.Nothing,
      ownerInformation = Prelude.Nothing
    }

-- | The ID of the target.
maintenanceWindowTarget_windowTargetId :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_windowTargetId = Lens.lens (\MaintenanceWindowTarget' {windowTargetId} -> windowTargetId) (\s@MaintenanceWindowTarget' {} a -> s {windowTargetId = a} :: MaintenanceWindowTarget)

-- | The type of target that is being registered with the maintenance window.
maintenanceWindowTarget_resourceType :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe MaintenanceWindowResourceType)
maintenanceWindowTarget_resourceType = Lens.lens (\MaintenanceWindowTarget' {resourceType} -> resourceType) (\s@MaintenanceWindowTarget' {} a -> s {resourceType = a} :: MaintenanceWindowTarget)

-- | The name for the maintenance window target.
maintenanceWindowTarget_name :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_name = Lens.lens (\MaintenanceWindowTarget' {name} -> name) (\s@MaintenanceWindowTarget' {} a -> s {name = a} :: MaintenanceWindowTarget)

-- | The ID of the maintenance window to register the target with.
maintenanceWindowTarget_windowId :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_windowId = Lens.lens (\MaintenanceWindowTarget' {windowId} -> windowId) (\s@MaintenanceWindowTarget' {} a -> s {windowId = a} :: MaintenanceWindowTarget)

-- | The targets, either managed nodes or tags.
--
-- Specify managed nodes using the following format:
--
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
--
-- Tags are specified using the following format:
--
-- @Key=\<tag name>,Values=\<tag value>@.
maintenanceWindowTarget_targets :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe [Target])
maintenanceWindowTarget_targets = Lens.lens (\MaintenanceWindowTarget' {targets} -> targets) (\s@MaintenanceWindowTarget' {} a -> s {targets = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Lens.coerced

-- | A description for the target.
maintenanceWindowTarget_description :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_description = Lens.lens (\MaintenanceWindowTarget' {description} -> description) (\s@MaintenanceWindowTarget' {} a -> s {description = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Data._Sensitive

-- | A user-provided value that will be included in any Amazon CloudWatch
-- Events events that are raised while running tasks for these targets in
-- this maintenance window.
maintenanceWindowTarget_ownerInformation :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_ownerInformation = Lens.lens (\MaintenanceWindowTarget' {ownerInformation} -> ownerInformation) (\s@MaintenanceWindowTarget' {} a -> s {ownerInformation = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON MaintenanceWindowTarget where
  parseJSON =
    Data.withObject
      "MaintenanceWindowTarget"
      ( \x ->
          MaintenanceWindowTarget'
            Prelude.<$> (x Data..:? "WindowTargetId")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "WindowId")
            Prelude.<*> (x Data..:? "Targets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "OwnerInformation")
      )

instance Prelude.Hashable MaintenanceWindowTarget where
  hashWithSalt _salt MaintenanceWindowTarget' {..} =
    _salt `Prelude.hashWithSalt` windowTargetId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` windowId
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ownerInformation

instance Prelude.NFData MaintenanceWindowTarget where
  rnf MaintenanceWindowTarget' {..} =
    Prelude.rnf windowTargetId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ownerInformation
