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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseHardware
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseHardware where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the hardware of a database.
--
-- /See:/ 'newRelationalDatabaseHardware' smart constructor.
data RelationalDatabaseHardware = RelationalDatabaseHardware'
  { -- | The amount of RAM in GB for the database.
    ramSizeInGb :: Core.Maybe Core.Double,
    -- | The number of vCPUs for the database.
    cpuCount :: Core.Maybe Core.Int,
    -- | The size of the disk for the database.
    diskSizeInGb :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RelationalDatabaseHardware' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ramSizeInGb', 'relationalDatabaseHardware_ramSizeInGb' - The amount of RAM in GB for the database.
--
-- 'cpuCount', 'relationalDatabaseHardware_cpuCount' - The number of vCPUs for the database.
--
-- 'diskSizeInGb', 'relationalDatabaseHardware_diskSizeInGb' - The size of the disk for the database.
newRelationalDatabaseHardware ::
  RelationalDatabaseHardware
newRelationalDatabaseHardware =
  RelationalDatabaseHardware'
    { ramSizeInGb =
        Core.Nothing,
      cpuCount = Core.Nothing,
      diskSizeInGb = Core.Nothing
    }

-- | The amount of RAM in GB for the database.
relationalDatabaseHardware_ramSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Core.Maybe Core.Double)
relationalDatabaseHardware_ramSizeInGb = Lens.lens (\RelationalDatabaseHardware' {ramSizeInGb} -> ramSizeInGb) (\s@RelationalDatabaseHardware' {} a -> s {ramSizeInGb = a} :: RelationalDatabaseHardware)

-- | The number of vCPUs for the database.
relationalDatabaseHardware_cpuCount :: Lens.Lens' RelationalDatabaseHardware (Core.Maybe Core.Int)
relationalDatabaseHardware_cpuCount = Lens.lens (\RelationalDatabaseHardware' {cpuCount} -> cpuCount) (\s@RelationalDatabaseHardware' {} a -> s {cpuCount = a} :: RelationalDatabaseHardware)

-- | The size of the disk for the database.
relationalDatabaseHardware_diskSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Core.Maybe Core.Int)
relationalDatabaseHardware_diskSizeInGb = Lens.lens (\RelationalDatabaseHardware' {diskSizeInGb} -> diskSizeInGb) (\s@RelationalDatabaseHardware' {} a -> s {diskSizeInGb = a} :: RelationalDatabaseHardware)

instance Core.FromJSON RelationalDatabaseHardware where
  parseJSON =
    Core.withObject
      "RelationalDatabaseHardware"
      ( \x ->
          RelationalDatabaseHardware'
            Core.<$> (x Core..:? "ramSizeInGb")
            Core.<*> (x Core..:? "cpuCount")
            Core.<*> (x Core..:? "diskSizeInGb")
      )

instance Core.Hashable RelationalDatabaseHardware

instance Core.NFData RelationalDatabaseHardware
