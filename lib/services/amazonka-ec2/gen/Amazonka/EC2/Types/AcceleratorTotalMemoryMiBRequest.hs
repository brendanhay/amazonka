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
-- Module      : Amazonka.EC2.Types.AcceleratorTotalMemoryMiBRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AcceleratorTotalMemoryMiBRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of total accelerator memory, in MiB.
--
-- /See:/ 'newAcceleratorTotalMemoryMiBRequest' smart constructor.
data AcceleratorTotalMemoryMiBRequest = AcceleratorTotalMemoryMiBRequest'
  { -- | The maximum amount of accelerator memory, in MiB. To specify no maximum
    -- limit, omit this parameter.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of accelerator memory, in MiB. To specify no minimum
    -- limit, omit this parameter.
    min :: Prelude.Maybe Prelude.Int
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
-- 'max', 'acceleratorTotalMemoryMiBRequest_max' - The maximum amount of accelerator memory, in MiB. To specify no maximum
-- limit, omit this parameter.
--
-- 'min', 'acceleratorTotalMemoryMiBRequest_min' - The minimum amount of accelerator memory, in MiB. To specify no minimum
-- limit, omit this parameter.
newAcceleratorTotalMemoryMiBRequest ::
  AcceleratorTotalMemoryMiBRequest
newAcceleratorTotalMemoryMiBRequest =
  AcceleratorTotalMemoryMiBRequest'
    { max =
        Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of accelerator memory, in MiB. To specify no maximum
-- limit, omit this parameter.
acceleratorTotalMemoryMiBRequest_max :: Lens.Lens' AcceleratorTotalMemoryMiBRequest (Prelude.Maybe Prelude.Int)
acceleratorTotalMemoryMiBRequest_max = Lens.lens (\AcceleratorTotalMemoryMiBRequest' {max} -> max) (\s@AcceleratorTotalMemoryMiBRequest' {} a -> s {max = a} :: AcceleratorTotalMemoryMiBRequest)

-- | The minimum amount of accelerator memory, in MiB. To specify no minimum
-- limit, omit this parameter.
acceleratorTotalMemoryMiBRequest_min :: Lens.Lens' AcceleratorTotalMemoryMiBRequest (Prelude.Maybe Prelude.Int)
acceleratorTotalMemoryMiBRequest_min = Lens.lens (\AcceleratorTotalMemoryMiBRequest' {min} -> min) (\s@AcceleratorTotalMemoryMiBRequest' {} a -> s {min = a} :: AcceleratorTotalMemoryMiBRequest)

instance
  Prelude.Hashable
    AcceleratorTotalMemoryMiBRequest
  where
  hashWithSalt
    _salt
    AcceleratorTotalMemoryMiBRequest' {..} =
      _salt
        `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AcceleratorTotalMemoryMiBRequest
  where
  rnf AcceleratorTotalMemoryMiBRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToQuery
    AcceleratorTotalMemoryMiBRequest
  where
  toQuery AcceleratorTotalMemoryMiBRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
