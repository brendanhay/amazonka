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
-- Module      : Amazonka.ElasticBeanstalk.Types.CPUUtilization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.CPUUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | CPU utilization metrics for an instance.
--
-- /See:/ 'newCPUUtilization' smart constructor.
data CPUUtilization = CPUUtilization'
  { -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @Nice@ state over the
    -- last 10 seconds.
    nice :: Prelude.Maybe Prelude.Double,
    -- | Percentage of time that the CPU has spent in the @User@ state over the
    -- last 10 seconds.
    user :: Prelude.Maybe Prelude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @SoftIRQ@ state over
    -- the last 10 seconds.
    softIRQ :: Prelude.Maybe Prelude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @I\/O Wait@ state over
    -- the last 10 seconds.
    iOWait :: Prelude.Maybe Prelude.Double,
    -- | Available on Windows environments only.
    --
    -- Percentage of time that the CPU has spent in the @Privileged@ state over
    -- the last 10 seconds.
    privileged :: Prelude.Maybe Prelude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @IRQ@ state over the
    -- last 10 seconds.
    irq :: Prelude.Maybe Prelude.Double,
    -- | Available on Linux environments only.
    --
    -- Percentage of time that the CPU has spent in the @System@ state over the
    -- last 10 seconds.
    system :: Prelude.Maybe Prelude.Double,
    -- | Percentage of time that the CPU has spent in the @Idle@ state over the
    -- last 10 seconds.
    idle :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CPUUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nice', 'cPUUtilization_nice' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @Nice@ state over the
-- last 10 seconds.
--
-- 'user', 'cPUUtilization_user' - Percentage of time that the CPU has spent in the @User@ state over the
-- last 10 seconds.
--
-- 'softIRQ', 'cPUUtilization_softIRQ' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @SoftIRQ@ state over
-- the last 10 seconds.
--
-- 'iOWait', 'cPUUtilization_iOWait' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @I\/O Wait@ state over
-- the last 10 seconds.
--
-- 'privileged', 'cPUUtilization_privileged' - Available on Windows environments only.
--
-- Percentage of time that the CPU has spent in the @Privileged@ state over
-- the last 10 seconds.
--
-- 'irq', 'cPUUtilization_irq' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @IRQ@ state over the
-- last 10 seconds.
--
-- 'system', 'cPUUtilization_system' - Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @System@ state over the
-- last 10 seconds.
--
-- 'idle', 'cPUUtilization_idle' - Percentage of time that the CPU has spent in the @Idle@ state over the
-- last 10 seconds.
newCPUUtilization ::
  CPUUtilization
newCPUUtilization =
  CPUUtilization'
    { nice = Prelude.Nothing,
      user = Prelude.Nothing,
      softIRQ = Prelude.Nothing,
      iOWait = Prelude.Nothing,
      privileged = Prelude.Nothing,
      irq = Prelude.Nothing,
      system = Prelude.Nothing,
      idle = Prelude.Nothing
    }

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @Nice@ state over the
-- last 10 seconds.
cPUUtilization_nice :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_nice = Lens.lens (\CPUUtilization' {nice} -> nice) (\s@CPUUtilization' {} a -> s {nice = a} :: CPUUtilization)

-- | Percentage of time that the CPU has spent in the @User@ state over the
-- last 10 seconds.
cPUUtilization_user :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_user = Lens.lens (\CPUUtilization' {user} -> user) (\s@CPUUtilization' {} a -> s {user = a} :: CPUUtilization)

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @SoftIRQ@ state over
-- the last 10 seconds.
cPUUtilization_softIRQ :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_softIRQ = Lens.lens (\CPUUtilization' {softIRQ} -> softIRQ) (\s@CPUUtilization' {} a -> s {softIRQ = a} :: CPUUtilization)

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @I\/O Wait@ state over
-- the last 10 seconds.
cPUUtilization_iOWait :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_iOWait = Lens.lens (\CPUUtilization' {iOWait} -> iOWait) (\s@CPUUtilization' {} a -> s {iOWait = a} :: CPUUtilization)

-- | Available on Windows environments only.
--
-- Percentage of time that the CPU has spent in the @Privileged@ state over
-- the last 10 seconds.
cPUUtilization_privileged :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_privileged = Lens.lens (\CPUUtilization' {privileged} -> privileged) (\s@CPUUtilization' {} a -> s {privileged = a} :: CPUUtilization)

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @IRQ@ state over the
-- last 10 seconds.
cPUUtilization_irq :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_irq = Lens.lens (\CPUUtilization' {irq} -> irq) (\s@CPUUtilization' {} a -> s {irq = a} :: CPUUtilization)

-- | Available on Linux environments only.
--
-- Percentage of time that the CPU has spent in the @System@ state over the
-- last 10 seconds.
cPUUtilization_system :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_system = Lens.lens (\CPUUtilization' {system} -> system) (\s@CPUUtilization' {} a -> s {system = a} :: CPUUtilization)

-- | Percentage of time that the CPU has spent in the @Idle@ state over the
-- last 10 seconds.
cPUUtilization_idle :: Lens.Lens' CPUUtilization (Prelude.Maybe Prelude.Double)
cPUUtilization_idle = Lens.lens (\CPUUtilization' {idle} -> idle) (\s@CPUUtilization' {} a -> s {idle = a} :: CPUUtilization)

instance Data.FromXML CPUUtilization where
  parseXML x =
    CPUUtilization'
      Prelude.<$> (x Data..@? "Nice")
      Prelude.<*> (x Data..@? "User")
      Prelude.<*> (x Data..@? "SoftIRQ")
      Prelude.<*> (x Data..@? "IOWait")
      Prelude.<*> (x Data..@? "Privileged")
      Prelude.<*> (x Data..@? "IRQ")
      Prelude.<*> (x Data..@? "System")
      Prelude.<*> (x Data..@? "Idle")

instance Prelude.Hashable CPUUtilization where
  hashWithSalt _salt CPUUtilization' {..} =
    _salt `Prelude.hashWithSalt` nice
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` softIRQ
      `Prelude.hashWithSalt` iOWait
      `Prelude.hashWithSalt` privileged
      `Prelude.hashWithSalt` irq
      `Prelude.hashWithSalt` system
      `Prelude.hashWithSalt` idle

instance Prelude.NFData CPUUtilization where
  rnf CPUUtilization' {..} =
    Prelude.rnf nice
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf softIRQ
      `Prelude.seq` Prelude.rnf iOWait
      `Prelude.seq` Prelude.rnf privileged
      `Prelude.seq` Prelude.rnf irq
      `Prelude.seq` Prelude.rnf system
      `Prelude.seq` Prelude.rnf idle
