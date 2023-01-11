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
-- Module      : Amazonka.Lightsail.Types.InstanceHardware
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceHardware where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.Disk
import qualified Amazonka.Prelude as Prelude

-- | Describes the hardware for the instance.
--
-- /See:/ 'newInstanceHardware' smart constructor.
data InstanceHardware = InstanceHardware'
  { -- | The number of vCPUs the instance has.
    cpuCount :: Prelude.Maybe Prelude.Int,
    -- | The disks attached to the instance.
    disks :: Prelude.Maybe [Disk],
    -- | The amount of RAM in GB on the instance (e.g., @1.0@).
    ramSizeInGb :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceHardware' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCount', 'instanceHardware_cpuCount' - The number of vCPUs the instance has.
--
-- 'disks', 'instanceHardware_disks' - The disks attached to the instance.
--
-- 'ramSizeInGb', 'instanceHardware_ramSizeInGb' - The amount of RAM in GB on the instance (e.g., @1.0@).
newInstanceHardware ::
  InstanceHardware
newInstanceHardware =
  InstanceHardware'
    { cpuCount = Prelude.Nothing,
      disks = Prelude.Nothing,
      ramSizeInGb = Prelude.Nothing
    }

-- | The number of vCPUs the instance has.
instanceHardware_cpuCount :: Lens.Lens' InstanceHardware (Prelude.Maybe Prelude.Int)
instanceHardware_cpuCount = Lens.lens (\InstanceHardware' {cpuCount} -> cpuCount) (\s@InstanceHardware' {} a -> s {cpuCount = a} :: InstanceHardware)

-- | The disks attached to the instance.
instanceHardware_disks :: Lens.Lens' InstanceHardware (Prelude.Maybe [Disk])
instanceHardware_disks = Lens.lens (\InstanceHardware' {disks} -> disks) (\s@InstanceHardware' {} a -> s {disks = a} :: InstanceHardware) Prelude.. Lens.mapping Lens.coerced

-- | The amount of RAM in GB on the instance (e.g., @1.0@).
instanceHardware_ramSizeInGb :: Lens.Lens' InstanceHardware (Prelude.Maybe Prelude.Double)
instanceHardware_ramSizeInGb = Lens.lens (\InstanceHardware' {ramSizeInGb} -> ramSizeInGb) (\s@InstanceHardware' {} a -> s {ramSizeInGb = a} :: InstanceHardware)

instance Data.FromJSON InstanceHardware where
  parseJSON =
    Data.withObject
      "InstanceHardware"
      ( \x ->
          InstanceHardware'
            Prelude.<$> (x Data..:? "cpuCount")
            Prelude.<*> (x Data..:? "disks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ramSizeInGb")
      )

instance Prelude.Hashable InstanceHardware where
  hashWithSalt _salt InstanceHardware' {..} =
    _salt `Prelude.hashWithSalt` cpuCount
      `Prelude.hashWithSalt` disks
      `Prelude.hashWithSalt` ramSizeInGb

instance Prelude.NFData InstanceHardware where
  rnf InstanceHardware' {..} =
    Prelude.rnf cpuCount
      `Prelude.seq` Prelude.rnf disks
      `Prelude.seq` Prelude.rnf ramSizeInGb
