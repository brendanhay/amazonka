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
-- Module      : Amazonka.EC2.Types.GpuInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.GpuInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.GpuDeviceInfo
import qualified Amazonka.Prelude as Prelude

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'newGpuInfo' smart constructor.
data GpuInfo = GpuInfo'
  { -- | Describes the GPU accelerators for the instance type.
    gpus :: Prelude.Maybe [GpuDeviceInfo],
    -- | The total size of the memory for the GPU accelerators for the instance
    -- type, in MiB.
    totalGpuMemoryInMiB :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GpuInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gpus', 'gpuInfo_gpus' - Describes the GPU accelerators for the instance type.
--
-- 'totalGpuMemoryInMiB', 'gpuInfo_totalGpuMemoryInMiB' - The total size of the memory for the GPU accelerators for the instance
-- type, in MiB.
newGpuInfo ::
  GpuInfo
newGpuInfo =
  GpuInfo'
    { gpus = Prelude.Nothing,
      totalGpuMemoryInMiB = Prelude.Nothing
    }

-- | Describes the GPU accelerators for the instance type.
gpuInfo_gpus :: Lens.Lens' GpuInfo (Prelude.Maybe [GpuDeviceInfo])
gpuInfo_gpus = Lens.lens (\GpuInfo' {gpus} -> gpus) (\s@GpuInfo' {} a -> s {gpus = a} :: GpuInfo) Prelude.. Lens.mapping Lens.coerced

-- | The total size of the memory for the GPU accelerators for the instance
-- type, in MiB.
gpuInfo_totalGpuMemoryInMiB :: Lens.Lens' GpuInfo (Prelude.Maybe Prelude.Int)
gpuInfo_totalGpuMemoryInMiB = Lens.lens (\GpuInfo' {totalGpuMemoryInMiB} -> totalGpuMemoryInMiB) (\s@GpuInfo' {} a -> s {totalGpuMemoryInMiB = a} :: GpuInfo)

instance Data.FromXML GpuInfo where
  parseXML x =
    GpuInfo'
      Prelude.<$> ( x Data..@? "gpus" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "totalGpuMemoryInMiB")

instance Prelude.Hashable GpuInfo where
  hashWithSalt _salt GpuInfo' {..} =
    _salt `Prelude.hashWithSalt` gpus
      `Prelude.hashWithSalt` totalGpuMemoryInMiB

instance Prelude.NFData GpuInfo where
  rnf GpuInfo' {..} =
    Prelude.rnf gpus
      `Prelude.seq` Prelude.rnf totalGpuMemoryInMiB
