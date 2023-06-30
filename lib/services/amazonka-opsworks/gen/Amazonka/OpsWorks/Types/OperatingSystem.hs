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
-- Module      : Amazonka.OpsWorks.Types.OperatingSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.OperatingSystem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.OperatingSystemConfigurationManager
import qualified Amazonka.Prelude as Prelude

-- | Describes supported operating systems in AWS OpsWorks Stacks.
--
-- /See:/ 'newOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { -- | Supported configuration manager name and versions for an AWS OpsWorks
    -- Stacks operating system.
    configurationManagers :: Prelude.Maybe [OperatingSystemConfigurationManager],
    -- | The ID of a supported operating system, such as @Amazon Linux 2018.03@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the operating system, such as @Amazon Linux 2018.03@.
    name :: Prelude.Maybe Prelude.Text,
    -- | A short name for the operating system manufacturer.
    reportedName :: Prelude.Maybe Prelude.Text,
    -- | The version of the operating system, including the release and edition,
    -- if applicable.
    reportedVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicates that an operating system is not supported for new instances.
    supported :: Prelude.Maybe Prelude.Bool,
    -- | The type of a supported operating system, either @Linux@ or @Windows@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OperatingSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationManagers', 'operatingSystem_configurationManagers' - Supported configuration manager name and versions for an AWS OpsWorks
-- Stacks operating system.
--
-- 'id', 'operatingSystem_id' - The ID of a supported operating system, such as @Amazon Linux 2018.03@.
--
-- 'name', 'operatingSystem_name' - The name of the operating system, such as @Amazon Linux 2018.03@.
--
-- 'reportedName', 'operatingSystem_reportedName' - A short name for the operating system manufacturer.
--
-- 'reportedVersion', 'operatingSystem_reportedVersion' - The version of the operating system, including the release and edition,
-- if applicable.
--
-- 'supported', 'operatingSystem_supported' - Indicates that an operating system is not supported for new instances.
--
-- 'type'', 'operatingSystem_type' - The type of a supported operating system, either @Linux@ or @Windows@.
newOperatingSystem ::
  OperatingSystem
newOperatingSystem =
  OperatingSystem'
    { configurationManagers =
        Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      reportedName = Prelude.Nothing,
      reportedVersion = Prelude.Nothing,
      supported = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Supported configuration manager name and versions for an AWS OpsWorks
-- Stacks operating system.
operatingSystem_configurationManagers :: Lens.Lens' OperatingSystem (Prelude.Maybe [OperatingSystemConfigurationManager])
operatingSystem_configurationManagers = Lens.lens (\OperatingSystem' {configurationManagers} -> configurationManagers) (\s@OperatingSystem' {} a -> s {configurationManagers = a} :: OperatingSystem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a supported operating system, such as @Amazon Linux 2018.03@.
operatingSystem_id :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_id = Lens.lens (\OperatingSystem' {id} -> id) (\s@OperatingSystem' {} a -> s {id = a} :: OperatingSystem)

-- | The name of the operating system, such as @Amazon Linux 2018.03@.
operatingSystem_name :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_name = Lens.lens (\OperatingSystem' {name} -> name) (\s@OperatingSystem' {} a -> s {name = a} :: OperatingSystem)

-- | A short name for the operating system manufacturer.
operatingSystem_reportedName :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_reportedName = Lens.lens (\OperatingSystem' {reportedName} -> reportedName) (\s@OperatingSystem' {} a -> s {reportedName = a} :: OperatingSystem)

-- | The version of the operating system, including the release and edition,
-- if applicable.
operatingSystem_reportedVersion :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_reportedVersion = Lens.lens (\OperatingSystem' {reportedVersion} -> reportedVersion) (\s@OperatingSystem' {} a -> s {reportedVersion = a} :: OperatingSystem)

-- | Indicates that an operating system is not supported for new instances.
operatingSystem_supported :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Bool)
operatingSystem_supported = Lens.lens (\OperatingSystem' {supported} -> supported) (\s@OperatingSystem' {} a -> s {supported = a} :: OperatingSystem)

-- | The type of a supported operating system, either @Linux@ or @Windows@.
operatingSystem_type :: Lens.Lens' OperatingSystem (Prelude.Maybe Prelude.Text)
operatingSystem_type = Lens.lens (\OperatingSystem' {type'} -> type') (\s@OperatingSystem' {} a -> s {type' = a} :: OperatingSystem)

instance Data.FromJSON OperatingSystem where
  parseJSON =
    Data.withObject
      "OperatingSystem"
      ( \x ->
          OperatingSystem'
            Prelude.<$> ( x
                            Data..:? "ConfigurationManagers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ReportedName")
            Prelude.<*> (x Data..:? "ReportedVersion")
            Prelude.<*> (x Data..:? "Supported")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable OperatingSystem where
  hashWithSalt _salt OperatingSystem' {..} =
    _salt
      `Prelude.hashWithSalt` configurationManagers
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reportedName
      `Prelude.hashWithSalt` reportedVersion
      `Prelude.hashWithSalt` supported
      `Prelude.hashWithSalt` type'

instance Prelude.NFData OperatingSystem where
  rnf OperatingSystem' {..} =
    Prelude.rnf configurationManagers
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reportedName
      `Prelude.seq` Prelude.rnf reportedVersion
      `Prelude.seq` Prelude.rnf supported
      `Prelude.seq` Prelude.rnf type'
