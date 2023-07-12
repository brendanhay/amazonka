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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | A description for the target.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name for the maintenance window target.
    name :: Prelude.Maybe Prelude.Text,
    -- | A user-provided value that will be included in any Amazon CloudWatch
    -- Events events that are raised while running tasks for these targets in
    -- this maintenance window.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The type of target that is being registered with the maintenance window.
    resourceType :: Prelude.Maybe MaintenanceWindowResourceType,
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
    -- | The ID of the maintenance window to register the target with.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the target.
    windowTargetId :: Prelude.Maybe Prelude.Text
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
-- 'description', 'maintenanceWindowTarget_description' - A description for the target.
--
-- 'name', 'maintenanceWindowTarget_name' - The name for the maintenance window target.
--
-- 'ownerInformation', 'maintenanceWindowTarget_ownerInformation' - A user-provided value that will be included in any Amazon CloudWatch
-- Events events that are raised while running tasks for these targets in
-- this maintenance window.
--
-- 'resourceType', 'maintenanceWindowTarget_resourceType' - The type of target that is being registered with the maintenance window.
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
-- 'windowId', 'maintenanceWindowTarget_windowId' - The ID of the maintenance window to register the target with.
--
-- 'windowTargetId', 'maintenanceWindowTarget_windowTargetId' - The ID of the target.
newMaintenanceWindowTarget ::
  MaintenanceWindowTarget
newMaintenanceWindowTarget =
  MaintenanceWindowTarget'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      ownerInformation = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      targets = Prelude.Nothing,
      windowId = Prelude.Nothing,
      windowTargetId = Prelude.Nothing
    }

-- | A description for the target.
maintenanceWindowTarget_description :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_description = Lens.lens (\MaintenanceWindowTarget' {description} -> description) (\s@MaintenanceWindowTarget' {} a -> s {description = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Data._Sensitive

-- | The name for the maintenance window target.
maintenanceWindowTarget_name :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_name = Lens.lens (\MaintenanceWindowTarget' {name} -> name) (\s@MaintenanceWindowTarget' {} a -> s {name = a} :: MaintenanceWindowTarget)

-- | A user-provided value that will be included in any Amazon CloudWatch
-- Events events that are raised while running tasks for these targets in
-- this maintenance window.
maintenanceWindowTarget_ownerInformation :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_ownerInformation = Lens.lens (\MaintenanceWindowTarget' {ownerInformation} -> ownerInformation) (\s@MaintenanceWindowTarget' {} a -> s {ownerInformation = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Data._Sensitive

-- | The type of target that is being registered with the maintenance window.
maintenanceWindowTarget_resourceType :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe MaintenanceWindowResourceType)
maintenanceWindowTarget_resourceType = Lens.lens (\MaintenanceWindowTarget' {resourceType} -> resourceType) (\s@MaintenanceWindowTarget' {} a -> s {resourceType = a} :: MaintenanceWindowTarget)

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

-- | The ID of the maintenance window to register the target with.
maintenanceWindowTarget_windowId :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_windowId = Lens.lens (\MaintenanceWindowTarget' {windowId} -> windowId) (\s@MaintenanceWindowTarget' {} a -> s {windowId = a} :: MaintenanceWindowTarget)

-- | The ID of the target.
maintenanceWindowTarget_windowTargetId :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_windowTargetId = Lens.lens (\MaintenanceWindowTarget' {windowTargetId} -> windowTargetId) (\s@MaintenanceWindowTarget' {} a -> s {windowTargetId = a} :: MaintenanceWindowTarget)

instance Data.FromJSON MaintenanceWindowTarget where
  parseJSON =
    Data.withObject
      "MaintenanceWindowTarget"
      ( \x ->
          MaintenanceWindowTarget'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OwnerInformation")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "Targets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "WindowId")
            Prelude.<*> (x Data..:? "WindowTargetId")
      )

instance Prelude.Hashable MaintenanceWindowTarget where
  hashWithSalt _salt MaintenanceWindowTarget' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerInformation
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` windowId
      `Prelude.hashWithSalt` windowTargetId

instance Prelude.NFData MaintenanceWindowTarget where
  rnf MaintenanceWindowTarget' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerInformation
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf windowTargetId
