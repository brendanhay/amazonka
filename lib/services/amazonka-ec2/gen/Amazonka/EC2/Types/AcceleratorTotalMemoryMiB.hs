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
-- Module      : Amazonka.EC2.Types.AcceleratorTotalMemoryMiB
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AcceleratorTotalMemoryMiB where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of total accelerator memory, in MiB.
--
-- /See:/ 'newAcceleratorTotalMemoryMiB' smart constructor.
data AcceleratorTotalMemoryMiB = AcceleratorTotalMemoryMiB'
  { -- | The maximum amount of accelerator memory, in MiB. If this parameter is
    -- not specified, there is no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of accelerator memory, in MiB. If this parameter is
    -- not specified, there is no minimum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceleratorTotalMemoryMiB' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'acceleratorTotalMemoryMiB_max' - The maximum amount of accelerator memory, in MiB. If this parameter is
-- not specified, there is no maximum limit.
--
-- 'min', 'acceleratorTotalMemoryMiB_min' - The minimum amount of accelerator memory, in MiB. If this parameter is
-- not specified, there is no minimum limit.
newAcceleratorTotalMemoryMiB ::
  AcceleratorTotalMemoryMiB
newAcceleratorTotalMemoryMiB =
  AcceleratorTotalMemoryMiB'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of accelerator memory, in MiB. If this parameter is
-- not specified, there is no maximum limit.
acceleratorTotalMemoryMiB_max :: Lens.Lens' AcceleratorTotalMemoryMiB (Prelude.Maybe Prelude.Int)
acceleratorTotalMemoryMiB_max = Lens.lens (\AcceleratorTotalMemoryMiB' {max} -> max) (\s@AcceleratorTotalMemoryMiB' {} a -> s {max = a} :: AcceleratorTotalMemoryMiB)

-- | The minimum amount of accelerator memory, in MiB. If this parameter is
-- not specified, there is no minimum limit.
acceleratorTotalMemoryMiB_min :: Lens.Lens' AcceleratorTotalMemoryMiB (Prelude.Maybe Prelude.Int)
acceleratorTotalMemoryMiB_min = Lens.lens (\AcceleratorTotalMemoryMiB' {min} -> min) (\s@AcceleratorTotalMemoryMiB' {} a -> s {min = a} :: AcceleratorTotalMemoryMiB)

instance Data.FromXML AcceleratorTotalMemoryMiB where
  parseXML x =
    AcceleratorTotalMemoryMiB'
      Prelude.<$> (x Data..@? "max") Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable AcceleratorTotalMemoryMiB where
  hashWithSalt _salt AcceleratorTotalMemoryMiB' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData AcceleratorTotalMemoryMiB where
  rnf AcceleratorTotalMemoryMiB' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery AcceleratorTotalMemoryMiB where
  toQuery AcceleratorTotalMemoryMiB' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
