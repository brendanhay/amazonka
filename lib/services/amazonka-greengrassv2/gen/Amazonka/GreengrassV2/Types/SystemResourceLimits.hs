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
-- Module      : Amazonka.GreengrassV2.Types.SystemResourceLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.SystemResourceLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about system resource limits that the IoT
-- Greengrass Core software applies to a component\'s processes. For more
-- information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-system-resource-limits Configure system resource limits for components>.
--
-- /See:/ 'newSystemResourceLimits' smart constructor.
data SystemResourceLimits = SystemResourceLimits'
  { -- | The maximum amount of CPU time that a component\'s processes can use on
    -- the core device. A core device\'s total CPU time is equivalent to the
    -- device\'s number of CPU cores. For example, on a core device with 4 CPU
    -- cores, you can set this value to @2@ to limit the component\'s processes
    -- to 50 percent usage of each CPU core. On a device with 1 CPU core, you
    -- can set this value to @0.25@ to limit the component\'s processes to 25
    -- percent usage of the CPU. If you set this value to a number greater than
    -- the number of CPU cores, the IoT Greengrass Core software doesn\'t limit
    -- the component\'s CPU usage.
    cpus :: Prelude.Maybe Prelude.Double,
    -- | The maximum amount of RAM, expressed in kilobytes, that a component\'s
    -- processes can use on the core device.
    memory :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemResourceLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpus', 'systemResourceLimits_cpus' - The maximum amount of CPU time that a component\'s processes can use on
-- the core device. A core device\'s total CPU time is equivalent to the
-- device\'s number of CPU cores. For example, on a core device with 4 CPU
-- cores, you can set this value to @2@ to limit the component\'s processes
-- to 50 percent usage of each CPU core. On a device with 1 CPU core, you
-- can set this value to @0.25@ to limit the component\'s processes to 25
-- percent usage of the CPU. If you set this value to a number greater than
-- the number of CPU cores, the IoT Greengrass Core software doesn\'t limit
-- the component\'s CPU usage.
--
-- 'memory', 'systemResourceLimits_memory' - The maximum amount of RAM, expressed in kilobytes, that a component\'s
-- processes can use on the core device.
newSystemResourceLimits ::
  SystemResourceLimits
newSystemResourceLimits =
  SystemResourceLimits'
    { cpus = Prelude.Nothing,
      memory = Prelude.Nothing
    }

-- | The maximum amount of CPU time that a component\'s processes can use on
-- the core device. A core device\'s total CPU time is equivalent to the
-- device\'s number of CPU cores. For example, on a core device with 4 CPU
-- cores, you can set this value to @2@ to limit the component\'s processes
-- to 50 percent usage of each CPU core. On a device with 1 CPU core, you
-- can set this value to @0.25@ to limit the component\'s processes to 25
-- percent usage of the CPU. If you set this value to a number greater than
-- the number of CPU cores, the IoT Greengrass Core software doesn\'t limit
-- the component\'s CPU usage.
systemResourceLimits_cpus :: Lens.Lens' SystemResourceLimits (Prelude.Maybe Prelude.Double)
systemResourceLimits_cpus = Lens.lens (\SystemResourceLimits' {cpus} -> cpus) (\s@SystemResourceLimits' {} a -> s {cpus = a} :: SystemResourceLimits)

-- | The maximum amount of RAM, expressed in kilobytes, that a component\'s
-- processes can use on the core device.
systemResourceLimits_memory :: Lens.Lens' SystemResourceLimits (Prelude.Maybe Prelude.Natural)
systemResourceLimits_memory = Lens.lens (\SystemResourceLimits' {memory} -> memory) (\s@SystemResourceLimits' {} a -> s {memory = a} :: SystemResourceLimits)

instance Data.FromJSON SystemResourceLimits where
  parseJSON =
    Data.withObject
      "SystemResourceLimits"
      ( \x ->
          SystemResourceLimits'
            Prelude.<$> (x Data..:? "cpus")
            Prelude.<*> (x Data..:? "memory")
      )

instance Prelude.Hashable SystemResourceLimits where
  hashWithSalt _salt SystemResourceLimits' {..} =
    _salt
      `Prelude.hashWithSalt` cpus
      `Prelude.hashWithSalt` memory

instance Prelude.NFData SystemResourceLimits where
  rnf SystemResourceLimits' {..} =
    Prelude.rnf cpus `Prelude.seq` Prelude.rnf memory

instance Data.ToJSON SystemResourceLimits where
  toJSON SystemResourceLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cpus" Data..=) Prelude.<$> cpus,
            ("memory" Data..=) Prelude.<$> memory
          ]
      )
