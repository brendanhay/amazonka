{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The CPU options for the instance.
--
-- /See:/ 'newCpuOptions' smart constructor.
data CpuOptions = CpuOptions'
  { -- | The number of threads per CPU core.
    threadsPerCore :: Prelude.Maybe Prelude.Int,
    -- | The number of CPU cores for the instance.
    coreCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { threadsPerCore = Prelude.Nothing,
      coreCount = Prelude.Nothing
    }

-- | The number of threads per CPU core.
cpuOptions_threadsPerCore :: Lens.Lens' CpuOptions (Prelude.Maybe Prelude.Int)
cpuOptions_threadsPerCore = Lens.lens (\CpuOptions' {threadsPerCore} -> threadsPerCore) (\s@CpuOptions' {} a -> s {threadsPerCore = a} :: CpuOptions)

-- | The number of CPU cores for the instance.
cpuOptions_coreCount :: Lens.Lens' CpuOptions (Prelude.Maybe Prelude.Int)
cpuOptions_coreCount = Lens.lens (\CpuOptions' {coreCount} -> coreCount) (\s@CpuOptions' {} a -> s {coreCount = a} :: CpuOptions)

instance Prelude.FromXML CpuOptions where
  parseXML x =
    CpuOptions'
      Prelude.<$> (x Prelude..@? "threadsPerCore")
      Prelude.<*> (x Prelude..@? "coreCount")

instance Prelude.Hashable CpuOptions

instance Prelude.NFData CpuOptions
