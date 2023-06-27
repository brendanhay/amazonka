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
-- Module      : Amazonka.SnowDeviceManagement.Types.CpuOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.CpuOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options for how a device\'s CPU is configured.
--
-- /See:/ 'newCpuOptions' smart constructor.
data CpuOptions = CpuOptions'
  { -- | The number of cores that the CPU can use.
    coreCount :: Prelude.Maybe Prelude.Int,
    -- | The number of threads per core in the CPU.
    threadsPerCore :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CpuOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreCount', 'cpuOptions_coreCount' - The number of cores that the CPU can use.
--
-- 'threadsPerCore', 'cpuOptions_threadsPerCore' - The number of threads per core in the CPU.
newCpuOptions ::
  CpuOptions
newCpuOptions =
  CpuOptions'
    { coreCount = Prelude.Nothing,
      threadsPerCore = Prelude.Nothing
    }

-- | The number of cores that the CPU can use.
cpuOptions_coreCount :: Lens.Lens' CpuOptions (Prelude.Maybe Prelude.Int)
cpuOptions_coreCount = Lens.lens (\CpuOptions' {coreCount} -> coreCount) (\s@CpuOptions' {} a -> s {coreCount = a} :: CpuOptions)

-- | The number of threads per core in the CPU.
cpuOptions_threadsPerCore :: Lens.Lens' CpuOptions (Prelude.Maybe Prelude.Int)
cpuOptions_threadsPerCore = Lens.lens (\CpuOptions' {threadsPerCore} -> threadsPerCore) (\s@CpuOptions' {} a -> s {threadsPerCore = a} :: CpuOptions)

instance Data.FromJSON CpuOptions where
  parseJSON =
    Data.withObject
      "CpuOptions"
      ( \x ->
          CpuOptions'
            Prelude.<$> (x Data..:? "coreCount")
            Prelude.<*> (x Data..:? "threadsPerCore")
      )

instance Prelude.Hashable CpuOptions where
  hashWithSalt _salt CpuOptions' {..} =
    _salt
      `Prelude.hashWithSalt` coreCount
      `Prelude.hashWithSalt` threadsPerCore

instance Prelude.NFData CpuOptions where
  rnf CpuOptions' {..} =
    Prelude.rnf coreCount
      `Prelude.seq` Prelude.rnf threadsPerCore
