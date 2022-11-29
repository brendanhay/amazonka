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
-- Module      : Amazonka.AutoScaling.Types.AcceleratorTotalMemoryMiBRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.AcceleratorTotalMemoryMiBRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @AcceleratorTotalMemoryMiB@
-- object when you specify InstanceRequirements for an Auto Scaling group.
--
-- /See:/ 'newAcceleratorTotalMemoryMiBRequest' smart constructor.
data AcceleratorTotalMemoryMiBRequest = AcceleratorTotalMemoryMiBRequest'
  { -- | The memory maximum in MiB.
    max :: Prelude.Maybe Prelude.Natural,
    -- | The memory minimum in MiB.
    min :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceleratorTotalMemoryMiBRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'acceleratorTotalMemoryMiBRequest_max' - The memory maximum in MiB.
--
-- 'min', 'acceleratorTotalMemoryMiBRequest_min' - The memory minimum in MiB.
newAcceleratorTotalMemoryMiBRequest ::
  AcceleratorTotalMemoryMiBRequest
newAcceleratorTotalMemoryMiBRequest =
  AcceleratorTotalMemoryMiBRequest'
    { max =
        Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The memory maximum in MiB.
acceleratorTotalMemoryMiBRequest_max :: Lens.Lens' AcceleratorTotalMemoryMiBRequest (Prelude.Maybe Prelude.Natural)
acceleratorTotalMemoryMiBRequest_max = Lens.lens (\AcceleratorTotalMemoryMiBRequest' {max} -> max) (\s@AcceleratorTotalMemoryMiBRequest' {} a -> s {max = a} :: AcceleratorTotalMemoryMiBRequest)

-- | The memory minimum in MiB.
acceleratorTotalMemoryMiBRequest_min :: Lens.Lens' AcceleratorTotalMemoryMiBRequest (Prelude.Maybe Prelude.Natural)
acceleratorTotalMemoryMiBRequest_min = Lens.lens (\AcceleratorTotalMemoryMiBRequest' {min} -> min) (\s@AcceleratorTotalMemoryMiBRequest' {} a -> s {min = a} :: AcceleratorTotalMemoryMiBRequest)

instance
  Core.FromXML
    AcceleratorTotalMemoryMiBRequest
  where
  parseXML x =
    AcceleratorTotalMemoryMiBRequest'
      Prelude.<$> (x Core..@? "Max") Prelude.<*> (x Core..@? "Min")

instance
  Prelude.Hashable
    AcceleratorTotalMemoryMiBRequest
  where
  hashWithSalt
    _salt
    AcceleratorTotalMemoryMiBRequest' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AcceleratorTotalMemoryMiBRequest
  where
  rnf AcceleratorTotalMemoryMiBRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Core.ToQuery
    AcceleratorTotalMemoryMiBRequest
  where
  toQuery AcceleratorTotalMemoryMiBRequest' {..} =
    Prelude.mconcat
      ["Max" Core.=: max, "Min" Core.=: min]
