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
-- Module      : Network.AWS.DeviceFarm.Types.CPU
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.CPU where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the amount of CPU that an app is using on a physical device.
-- Does not represent system-wide CPU usage.
--
-- /See:/ 'newCPU' smart constructor.
data CPU = CPU'
  { -- | The CPU\'s architecture (for example, x86 or ARM).
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The CPU\'s frequency.
    frequency :: Prelude.Maybe Prelude.Text,
    -- | The clock speed of the device\'s CPU, expressed in hertz (Hz). For
    -- example, a 1.2 GHz CPU is expressed as 1200000000.
    clock :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CPU' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'cpu_architecture' - The CPU\'s architecture (for example, x86 or ARM).
--
-- 'frequency', 'cpu_frequency' - The CPU\'s frequency.
--
-- 'clock', 'cpu_clock' - The clock speed of the device\'s CPU, expressed in hertz (Hz). For
-- example, a 1.2 GHz CPU is expressed as 1200000000.
newCPU ::
  CPU
newCPU =
  CPU'
    { architecture = Prelude.Nothing,
      frequency = Prelude.Nothing,
      clock = Prelude.Nothing
    }

-- | The CPU\'s architecture (for example, x86 or ARM).
cpu_architecture :: Lens.Lens' CPU (Prelude.Maybe Prelude.Text)
cpu_architecture = Lens.lens (\CPU' {architecture} -> architecture) (\s@CPU' {} a -> s {architecture = a} :: CPU)

-- | The CPU\'s frequency.
cpu_frequency :: Lens.Lens' CPU (Prelude.Maybe Prelude.Text)
cpu_frequency = Lens.lens (\CPU' {frequency} -> frequency) (\s@CPU' {} a -> s {frequency = a} :: CPU)

-- | The clock speed of the device\'s CPU, expressed in hertz (Hz). For
-- example, a 1.2 GHz CPU is expressed as 1200000000.
cpu_clock :: Lens.Lens' CPU (Prelude.Maybe Prelude.Double)
cpu_clock = Lens.lens (\CPU' {clock} -> clock) (\s@CPU' {} a -> s {clock = a} :: CPU)

instance Core.FromJSON CPU where
  parseJSON =
    Core.withObject
      "CPU"
      ( \x ->
          CPU'
            Prelude.<$> (x Core..:? "architecture")
            Prelude.<*> (x Core..:? "frequency")
            Prelude.<*> (x Core..:? "clock")
      )

instance Prelude.Hashable CPU

instance Prelude.NFData CPU
