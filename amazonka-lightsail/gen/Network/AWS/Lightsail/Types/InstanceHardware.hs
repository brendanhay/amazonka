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
-- Module      : Network.AWS.Lightsail.Types.InstanceHardware
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHardware where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Disk
import qualified Network.AWS.Prelude as Prelude

-- | Describes the hardware for the instance.
--
-- /See:/ 'newInstanceHardware' smart constructor.
data InstanceHardware = InstanceHardware'
  { -- | The amount of RAM in GB on the instance (e.g., @1.0@).
    ramSizeInGb :: Prelude.Maybe Prelude.Double,
    -- | The disks attached to the instance.
    disks :: Prelude.Maybe [Disk],
    -- | The number of vCPUs the instance has.
    cpuCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ramSizeInGb = Prelude.Nothing,
      disks = Prelude.Nothing,
      cpuCount = Prelude.Nothing
    }

-- | The amount of RAM in GB on the instance (e.g., @1.0@).
instanceHardware_ramSizeInGb :: Lens.Lens' InstanceHardware (Prelude.Maybe Prelude.Double)
instanceHardware_ramSizeInGb = Lens.lens (\InstanceHardware' {ramSizeInGb} -> ramSizeInGb) (\s@InstanceHardware' {} a -> s {ramSizeInGb = a} :: InstanceHardware)

-- | The disks attached to the instance.
instanceHardware_disks :: Lens.Lens' InstanceHardware (Prelude.Maybe [Disk])
instanceHardware_disks = Lens.lens (\InstanceHardware' {disks} -> disks) (\s@InstanceHardware' {} a -> s {disks = a} :: InstanceHardware) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of vCPUs the instance has.
instanceHardware_cpuCount :: Lens.Lens' InstanceHardware (Prelude.Maybe Prelude.Int)
instanceHardware_cpuCount = Lens.lens (\InstanceHardware' {cpuCount} -> cpuCount) (\s@InstanceHardware' {} a -> s {cpuCount = a} :: InstanceHardware)

instance Prelude.FromJSON InstanceHardware where
  parseJSON =
    Prelude.withObject
      "InstanceHardware"
      ( \x ->
          InstanceHardware'
            Prelude.<$> (x Prelude..:? "ramSizeInGb")
            Prelude.<*> (x Prelude..:? "disks" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "cpuCount")
      )

instance Prelude.Hashable InstanceHardware

instance Prelude.NFData InstanceHardware
