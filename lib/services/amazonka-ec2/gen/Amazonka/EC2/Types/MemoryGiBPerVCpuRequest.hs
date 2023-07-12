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
-- Module      : Amazonka.EC2.Types.MemoryGiBPerVCpuRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.MemoryGiBPerVCpuRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of memory per vCPU, in GiB.
--
-- /See:/ 'newMemoryGiBPerVCpuRequest' smart constructor.
data MemoryGiBPerVCpuRequest = MemoryGiBPerVCpuRequest'
  { -- | The maximum amount of memory per vCPU, in GiB. To specify no maximum
    -- limit, omit this parameter.
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum amount of memory per vCPU, in GiB. To specify no minimum
    -- limit, omit this parameter.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemoryGiBPerVCpuRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'memoryGiBPerVCpuRequest_max' - The maximum amount of memory per vCPU, in GiB. To specify no maximum
-- limit, omit this parameter.
--
-- 'min', 'memoryGiBPerVCpuRequest_min' - The minimum amount of memory per vCPU, in GiB. To specify no minimum
-- limit, omit this parameter.
newMemoryGiBPerVCpuRequest ::
  MemoryGiBPerVCpuRequest
newMemoryGiBPerVCpuRequest =
  MemoryGiBPerVCpuRequest'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of memory per vCPU, in GiB. To specify no maximum
-- limit, omit this parameter.
memoryGiBPerVCpuRequest_max :: Lens.Lens' MemoryGiBPerVCpuRequest (Prelude.Maybe Prelude.Double)
memoryGiBPerVCpuRequest_max = Lens.lens (\MemoryGiBPerVCpuRequest' {max} -> max) (\s@MemoryGiBPerVCpuRequest' {} a -> s {max = a} :: MemoryGiBPerVCpuRequest)

-- | The minimum amount of memory per vCPU, in GiB. To specify no minimum
-- limit, omit this parameter.
memoryGiBPerVCpuRequest_min :: Lens.Lens' MemoryGiBPerVCpuRequest (Prelude.Maybe Prelude.Double)
memoryGiBPerVCpuRequest_min = Lens.lens (\MemoryGiBPerVCpuRequest' {min} -> min) (\s@MemoryGiBPerVCpuRequest' {} a -> s {min = a} :: MemoryGiBPerVCpuRequest)

instance Prelude.Hashable MemoryGiBPerVCpuRequest where
  hashWithSalt _salt MemoryGiBPerVCpuRequest' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData MemoryGiBPerVCpuRequest where
  rnf MemoryGiBPerVCpuRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery MemoryGiBPerVCpuRequest where
  toQuery MemoryGiBPerVCpuRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
