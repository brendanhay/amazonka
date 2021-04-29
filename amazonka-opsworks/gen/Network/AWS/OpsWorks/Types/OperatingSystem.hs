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
-- Module      : Network.AWS.OpsWorks.Types.OperatingSystem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.OperatingSystem where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
import qualified Network.AWS.Prelude as Prelude

-- | Describes supported operating systems in AWS OpsWorks Stacks.
--
-- /See:/ 'newOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { -- | Indicates that an operating system is not supported for new instances.
    supported :: Prelude.Maybe Prelude.Bool,
    -- | Supported configuration manager name and versions for an AWS OpsWorks
    -- Stacks operating system.
    configurationManagers :: Prelude.Maybe [OperatingSystemConfigurationManager],
    -- | The ID of a supported operating system, such as @Amazon Linux 2018.03@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The version of the operating system, including the release and edition,
    -- if applicable.
    reportedVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the operating system, such as @Amazon Linux 2018.03@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of a supported operating system, either @Linux@ or @Windows@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | A short name for the operating system manufacturer.
    reportedName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { supported = Prelude.Nothing,
      configurationManagers = Prelude.Nothing,
      id = Prelude.Nothing,
      reportedVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      reportedName = Prelude.Nothing
    }

-- | Indicates that an operating system is not supported for new instances.
operatingSystem_supported :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Bool)
operatingSystem_supported = Lens.lens (\OperatingSystem' {supported} -> supported) (\s@OperatingSystem' {} a -> s {supported = a} :: OperatingSystem)

-- | Supported configuration manager name and versions for an AWS OpsWorks
-- Stacks operating system.
operatingSystem_configurationManagers :: Lens.Lens' OperatingSystem (Prelude.Maybe [OperatingSystemConfigurationManager])
operatingSystem_configurationManagers = Lens.lens (\OperatingSystem' {configurationManagers} -> configurationManagers) (\s@OperatingSystem' {} a -> s {configurationManagers = a} :: OperatingSystem) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of a supported operating system, such as @Amazon Linux 2018.03@.
operatingSystem_id :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_id = Lens.lens (\OperatingSystem' {id} -> id) (\s@OperatingSystem' {} a -> s {id = a} :: OperatingSystem)

-- | The version of the operating system, including the release and edition,
-- if applicable.
operatingSystem_reportedVersion :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_reportedVersion = Lens.lens (\OperatingSystem' {reportedVersion} -> reportedVersion) (\s@OperatingSystem' {} a -> s {reportedVersion = a} :: OperatingSystem)

-- | The name of the operating system, such as @Amazon Linux 2018.03@.
operatingSystem_name :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_name = Lens.lens (\OperatingSystem' {name} -> name) (\s@OperatingSystem' {} a -> s {name = a} :: OperatingSystem)

-- | The type of a supported operating system, either @Linux@ or @Windows@.
operatingSystem_type :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_type = Lens.lens (\OperatingSystem' {type'} -> type') (\s@OperatingSystem' {} a -> s {type' = a} :: OperatingSystem)

-- | A short name for the operating system manufacturer.
operatingSystem_reportedName :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_reportedName = Lens.lens (\OperatingSystem' {reportedName} -> reportedName) (\s@OperatingSystem' {} a -> s {reportedName = a} :: OperatingSystem)

instance Prelude.FromJSON OperatingSystem where
  parseJSON =
    Prelude.withObject
      "OperatingSystem"
      ( \x ->
          OperatingSystem'
            Prelude.<$> (x Prelude..:? "Supported")
            Prelude.<*> ( x Prelude..:? "ConfigurationManagers"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "ReportedVersion")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "ReportedName")
      )

instance Prelude.Hashable OperatingSystem

instance Prelude.NFData OperatingSystem
