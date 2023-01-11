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
-- Module      : Amazonka.EC2.Types.MemoryGiBPerVCpu
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.MemoryGiBPerVCpu where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of memory per vCPU, in GiB.
--
-- /See:/ 'newMemoryGiBPerVCpu' smart constructor.
data MemoryGiBPerVCpu = MemoryGiBPerVCpu'
  { -- | The maximum amount of memory per vCPU, in GiB. If this parameter is not
    -- specified, there is no maximum limit.
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum amount of memory per vCPU, in GiB. If this parameter is not
    -- specified, there is no minimum limit.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemoryGiBPerVCpu' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'memoryGiBPerVCpu_max' - The maximum amount of memory per vCPU, in GiB. If this parameter is not
-- specified, there is no maximum limit.
--
-- 'min', 'memoryGiBPerVCpu_min' - The minimum amount of memory per vCPU, in GiB. If this parameter is not
-- specified, there is no minimum limit.
newMemoryGiBPerVCpu ::
  MemoryGiBPerVCpu
newMemoryGiBPerVCpu =
  MemoryGiBPerVCpu'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of memory per vCPU, in GiB. If this parameter is not
-- specified, there is no maximum limit.
memoryGiBPerVCpu_max :: Lens.Lens' MemoryGiBPerVCpu (Prelude.Maybe Prelude.Double)
memoryGiBPerVCpu_max = Lens.lens (\MemoryGiBPerVCpu' {max} -> max) (\s@MemoryGiBPerVCpu' {} a -> s {max = a} :: MemoryGiBPerVCpu)

-- | The minimum amount of memory per vCPU, in GiB. If this parameter is not
-- specified, there is no minimum limit.
memoryGiBPerVCpu_min :: Lens.Lens' MemoryGiBPerVCpu (Prelude.Maybe Prelude.Double)
memoryGiBPerVCpu_min = Lens.lens (\MemoryGiBPerVCpu' {min} -> min) (\s@MemoryGiBPerVCpu' {} a -> s {min = a} :: MemoryGiBPerVCpu)

instance Data.FromXML MemoryGiBPerVCpu where
  parseXML x =
    MemoryGiBPerVCpu'
      Prelude.<$> (x Data..@? "max") Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable MemoryGiBPerVCpu where
  hashWithSalt _salt MemoryGiBPerVCpu' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData MemoryGiBPerVCpu where
  rnf MemoryGiBPerVCpu' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery MemoryGiBPerVCpu where
  toQuery MemoryGiBPerVCpu' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
