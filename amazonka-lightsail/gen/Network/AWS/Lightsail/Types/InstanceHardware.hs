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
-- Module      : Network.AWS.Lightsail.Types.InstanceHardware
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHardware where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Disk

-- | Describes the hardware for the instance.
--
-- /See:/ 'newInstanceHardware' smart constructor.
data InstanceHardware = InstanceHardware'
  { -- | The amount of RAM in GB on the instance (e.g., @1.0@).
    ramSizeInGb :: Core.Maybe Core.Double,
    -- | The disks attached to the instance.
    disks :: Core.Maybe [Disk],
    -- | The number of vCPUs the instance has.
    cpuCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceHardware' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ramSizeInGb', 'instanceHardware_ramSizeInGb' - The amount of RAM in GB on the instance (e.g., @1.0@).
--
-- 'disks', 'instanceHardware_disks' - The disks attached to the instance.
--
-- 'cpuCount', 'instanceHardware_cpuCount' - The number of vCPUs the instance has.
newInstanceHardware ::
  InstanceHardware
newInstanceHardware =
  InstanceHardware'
    { ramSizeInGb = Core.Nothing,
      disks = Core.Nothing,
      cpuCount = Core.Nothing
    }

-- | The amount of RAM in GB on the instance (e.g., @1.0@).
instanceHardware_ramSizeInGb :: Lens.Lens' InstanceHardware (Core.Maybe Core.Double)
instanceHardware_ramSizeInGb = Lens.lens (\InstanceHardware' {ramSizeInGb} -> ramSizeInGb) (\s@InstanceHardware' {} a -> s {ramSizeInGb = a} :: InstanceHardware)

-- | The disks attached to the instance.
instanceHardware_disks :: Lens.Lens' InstanceHardware (Core.Maybe [Disk])
instanceHardware_disks = Lens.lens (\InstanceHardware' {disks} -> disks) (\s@InstanceHardware' {} a -> s {disks = a} :: InstanceHardware) Core.. Lens.mapping Lens._Coerce

-- | The number of vCPUs the instance has.
instanceHardware_cpuCount :: Lens.Lens' InstanceHardware (Core.Maybe Core.Int)
instanceHardware_cpuCount = Lens.lens (\InstanceHardware' {cpuCount} -> cpuCount) (\s@InstanceHardware' {} a -> s {cpuCount = a} :: InstanceHardware)

instance Core.FromJSON InstanceHardware where
  parseJSON =
    Core.withObject
      "InstanceHardware"
      ( \x ->
          InstanceHardware'
            Core.<$> (x Core..:? "ramSizeInGb")
            Core.<*> (x Core..:? "disks" Core..!= Core.mempty)
            Core.<*> (x Core..:? "cpuCount")
      )

instance Core.Hashable InstanceHardware

instance Core.NFData InstanceHardware
