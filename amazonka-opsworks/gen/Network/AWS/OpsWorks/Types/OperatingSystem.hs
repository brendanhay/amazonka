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
-- Module      : Network.AWS.OpsWorks.Types.OperatingSystem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.OperatingSystem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager

-- | Describes supported operating systems in AWS OpsWorks Stacks.
--
-- /See:/ 'newOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { -- | Indicates that an operating system is not supported for new instances.
    supported :: Core.Maybe Core.Bool,
    -- | Supported configuration manager name and versions for an AWS OpsWorks
    -- Stacks operating system.
    configurationManagers :: Core.Maybe [OperatingSystemConfigurationManager],
    -- | The ID of a supported operating system, such as @Amazon Linux 2018.03@.
    id :: Core.Maybe Core.Text,
    -- | The version of the operating system, including the release and edition,
    -- if applicable.
    reportedVersion :: Core.Maybe Core.Text,
    -- | The name of the operating system, such as @Amazon Linux 2018.03@.
    name :: Core.Maybe Core.Text,
    -- | The type of a supported operating system, either @Linux@ or @Windows@.
    type' :: Core.Maybe Core.Text,
    -- | A short name for the operating system manufacturer.
    reportedName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OperatingSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supported', 'operatingSystem_supported' - Indicates that an operating system is not supported for new instances.
--
-- 'configurationManagers', 'operatingSystem_configurationManagers' - Supported configuration manager name and versions for an AWS OpsWorks
-- Stacks operating system.
--
-- 'id', 'operatingSystem_id' - The ID of a supported operating system, such as @Amazon Linux 2018.03@.
--
-- 'reportedVersion', 'operatingSystem_reportedVersion' - The version of the operating system, including the release and edition,
-- if applicable.
--
-- 'name', 'operatingSystem_name' - The name of the operating system, such as @Amazon Linux 2018.03@.
--
-- 'type'', 'operatingSystem_type' - The type of a supported operating system, either @Linux@ or @Windows@.
--
-- 'reportedName', 'operatingSystem_reportedName' - A short name for the operating system manufacturer.
newOperatingSystem ::
  OperatingSystem
newOperatingSystem =
  OperatingSystem'
    { supported = Core.Nothing,
      configurationManagers = Core.Nothing,
      id = Core.Nothing,
      reportedVersion = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing,
      reportedName = Core.Nothing
    }

-- | Indicates that an operating system is not supported for new instances.
operatingSystem_supported :: Lens.Lens' OperatingSystem (Core.Maybe Core.Bool)
operatingSystem_supported = Lens.lens (\OperatingSystem' {supported} -> supported) (\s@OperatingSystem' {} a -> s {supported = a} :: OperatingSystem)

-- | Supported configuration manager name and versions for an AWS OpsWorks
-- Stacks operating system.
operatingSystem_configurationManagers :: Lens.Lens' OperatingSystem (Core.Maybe [OperatingSystemConfigurationManager])
operatingSystem_configurationManagers = Lens.lens (\OperatingSystem' {configurationManagers} -> configurationManagers) (\s@OperatingSystem' {} a -> s {configurationManagers = a} :: OperatingSystem) Core.. Lens.mapping Lens._Coerce

-- | The ID of a supported operating system, such as @Amazon Linux 2018.03@.
operatingSystem_id :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
operatingSystem_id = Lens.lens (\OperatingSystem' {id} -> id) (\s@OperatingSystem' {} a -> s {id = a} :: OperatingSystem)

-- | The version of the operating system, including the release and edition,
-- if applicable.
operatingSystem_reportedVersion :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
operatingSystem_reportedVersion = Lens.lens (\OperatingSystem' {reportedVersion} -> reportedVersion) (\s@OperatingSystem' {} a -> s {reportedVersion = a} :: OperatingSystem)

-- | The name of the operating system, such as @Amazon Linux 2018.03@.
operatingSystem_name :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
operatingSystem_name = Lens.lens (\OperatingSystem' {name} -> name) (\s@OperatingSystem' {} a -> s {name = a} :: OperatingSystem)

-- | The type of a supported operating system, either @Linux@ or @Windows@.
operatingSystem_type :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
operatingSystem_type = Lens.lens (\OperatingSystem' {type'} -> type') (\s@OperatingSystem' {} a -> s {type' = a} :: OperatingSystem)

-- | A short name for the operating system manufacturer.
operatingSystem_reportedName :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
operatingSystem_reportedName = Lens.lens (\OperatingSystem' {reportedName} -> reportedName) (\s@OperatingSystem' {} a -> s {reportedName = a} :: OperatingSystem)

instance Core.FromJSON OperatingSystem where
  parseJSON =
    Core.withObject
      "OperatingSystem"
      ( \x ->
          OperatingSystem'
            Core.<$> (x Core..:? "Supported")
            Core.<*> ( x Core..:? "ConfigurationManagers"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "ReportedVersion")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "ReportedName")
      )

instance Core.Hashable OperatingSystem

instance Core.NFData OperatingSystem
