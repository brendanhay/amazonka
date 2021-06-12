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
-- Module      : Network.AWS.EC2.Types.CpuOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CpuOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The CPU options for the instance.
--
-- /See:/ 'newCpuOptions' smart constructor.
data CpuOptions = CpuOptions'
  { -- | The number of threads per CPU core.
    threadsPerCore :: Core.Maybe Core.Int,
    -- | The number of CPU cores for the instance.
    coreCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CpuOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threadsPerCore', 'cpuOptions_threadsPerCore' - The number of threads per CPU core.
--
-- 'coreCount', 'cpuOptions_coreCount' - The number of CPU cores for the instance.
newCpuOptions ::
  CpuOptions
newCpuOptions =
  CpuOptions'
    { threadsPerCore = Core.Nothing,
      coreCount = Core.Nothing
    }

-- | The number of threads per CPU core.
cpuOptions_threadsPerCore :: Lens.Lens' CpuOptions (Core.Maybe Core.Int)
cpuOptions_threadsPerCore = Lens.lens (\CpuOptions' {threadsPerCore} -> threadsPerCore) (\s@CpuOptions' {} a -> s {threadsPerCore = a} :: CpuOptions)

-- | The number of CPU cores for the instance.
cpuOptions_coreCount :: Lens.Lens' CpuOptions (Core.Maybe Core.Int)
cpuOptions_coreCount = Lens.lens (\CpuOptions' {coreCount} -> coreCount) (\s@CpuOptions' {} a -> s {coreCount = a} :: CpuOptions)

instance Core.FromXML CpuOptions where
  parseXML x =
    CpuOptions'
      Core.<$> (x Core..@? "threadsPerCore")
      Core.<*> (x Core..@? "coreCount")

instance Core.Hashable CpuOptions

instance Core.NFData CpuOptions
