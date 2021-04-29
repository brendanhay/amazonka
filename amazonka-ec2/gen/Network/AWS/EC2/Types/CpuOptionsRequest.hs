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
-- Module      : Network.AWS.EC2.Types.CpuOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CpuOptionsRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The CPU options for the instance. Both the core count and threads per
-- core must be specified in the request.
--
-- /See:/ 'newCpuOptionsRequest' smart constructor.
data CpuOptionsRequest = CpuOptionsRequest'
  { -- | The number of threads per CPU core. To disable multithreading for the
    -- instance, specify a value of @1@. Otherwise, specify the default value
    -- of @2@.
    threadsPerCore :: Prelude.Maybe Prelude.Int,
    -- | The number of CPU cores for the instance.
    coreCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CpuOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threadsPerCore', 'cpuOptionsRequest_threadsPerCore' - The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of @1@. Otherwise, specify the default value
-- of @2@.
--
-- 'coreCount', 'cpuOptionsRequest_coreCount' - The number of CPU cores for the instance.
newCpuOptionsRequest ::
  CpuOptionsRequest
newCpuOptionsRequest =
  CpuOptionsRequest'
    { threadsPerCore =
        Prelude.Nothing,
      coreCount = Prelude.Nothing
    }

-- | The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of @1@. Otherwise, specify the default value
-- of @2@.
cpuOptionsRequest_threadsPerCore :: Lens.Lens' CpuOptionsRequest (Prelude.Maybe Prelude.Int)
cpuOptionsRequest_threadsPerCore = Lens.lens (\CpuOptionsRequest' {threadsPerCore} -> threadsPerCore) (\s@CpuOptionsRequest' {} a -> s {threadsPerCore = a} :: CpuOptionsRequest)

-- | The number of CPU cores for the instance.
cpuOptionsRequest_coreCount :: Lens.Lens' CpuOptionsRequest (Prelude.Maybe Prelude.Int)
cpuOptionsRequest_coreCount = Lens.lens (\CpuOptionsRequest' {coreCount} -> coreCount) (\s@CpuOptionsRequest' {} a -> s {coreCount = a} :: CpuOptionsRequest)

instance Prelude.Hashable CpuOptionsRequest

instance Prelude.NFData CpuOptionsRequest

instance Prelude.ToQuery CpuOptionsRequest where
  toQuery CpuOptionsRequest' {..} =
    Prelude.mconcat
      [ "ThreadsPerCore" Prelude.=: threadsPerCore,
        "CoreCount" Prelude.=: coreCount
      ]
