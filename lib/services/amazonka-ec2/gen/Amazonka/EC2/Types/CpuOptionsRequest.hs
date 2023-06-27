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
-- Module      : Amazonka.EC2.Types.CpuOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CpuOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AmdSevSnpSpecification
import qualified Amazonka.Prelude as Prelude

-- | The CPU options for the instance. Both the core count and threads per
-- core must be specified in the request.
--
-- /See:/ 'newCpuOptionsRequest' smart constructor.
data CpuOptionsRequest = CpuOptionsRequest'
  { -- | Indicates whether to enable the instance for AMD SEV-SNP. AMD SEV-SNP is
    -- supported with M6a, R6a, and C6a instance types only. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/sev-snp.html AMD SEV-SNP>.
    amdSevSnp :: Prelude.Maybe AmdSevSnpSpecification,
    -- | The number of CPU cores for the instance.
    coreCount :: Prelude.Maybe Prelude.Int,
    -- | The number of threads per CPU core. To disable multithreading for the
    -- instance, specify a value of @1@. Otherwise, specify the default value
    -- of @2@.
    threadsPerCore :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CpuOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amdSevSnp', 'cpuOptionsRequest_amdSevSnp' - Indicates whether to enable the instance for AMD SEV-SNP. AMD SEV-SNP is
-- supported with M6a, R6a, and C6a instance types only. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/sev-snp.html AMD SEV-SNP>.
--
-- 'coreCount', 'cpuOptionsRequest_coreCount' - The number of CPU cores for the instance.
--
-- 'threadsPerCore', 'cpuOptionsRequest_threadsPerCore' - The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of @1@. Otherwise, specify the default value
-- of @2@.
newCpuOptionsRequest ::
  CpuOptionsRequest
newCpuOptionsRequest =
  CpuOptionsRequest'
    { amdSevSnp = Prelude.Nothing,
      coreCount = Prelude.Nothing,
      threadsPerCore = Prelude.Nothing
    }

-- | Indicates whether to enable the instance for AMD SEV-SNP. AMD SEV-SNP is
-- supported with M6a, R6a, and C6a instance types only. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/sev-snp.html AMD SEV-SNP>.
cpuOptionsRequest_amdSevSnp :: Lens.Lens' CpuOptionsRequest (Prelude.Maybe AmdSevSnpSpecification)
cpuOptionsRequest_amdSevSnp = Lens.lens (\CpuOptionsRequest' {amdSevSnp} -> amdSevSnp) (\s@CpuOptionsRequest' {} a -> s {amdSevSnp = a} :: CpuOptionsRequest)

-- | The number of CPU cores for the instance.
cpuOptionsRequest_coreCount :: Lens.Lens' CpuOptionsRequest (Prelude.Maybe Prelude.Int)
cpuOptionsRequest_coreCount = Lens.lens (\CpuOptionsRequest' {coreCount} -> coreCount) (\s@CpuOptionsRequest' {} a -> s {coreCount = a} :: CpuOptionsRequest)

-- | The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of @1@. Otherwise, specify the default value
-- of @2@.
cpuOptionsRequest_threadsPerCore :: Lens.Lens' CpuOptionsRequest (Prelude.Maybe Prelude.Int)
cpuOptionsRequest_threadsPerCore = Lens.lens (\CpuOptionsRequest' {threadsPerCore} -> threadsPerCore) (\s@CpuOptionsRequest' {} a -> s {threadsPerCore = a} :: CpuOptionsRequest)

instance Prelude.Hashable CpuOptionsRequest where
  hashWithSalt _salt CpuOptionsRequest' {..} =
    _salt
      `Prelude.hashWithSalt` amdSevSnp
      `Prelude.hashWithSalt` coreCount
      `Prelude.hashWithSalt` threadsPerCore

instance Prelude.NFData CpuOptionsRequest where
  rnf CpuOptionsRequest' {..} =
    Prelude.rnf amdSevSnp
      `Prelude.seq` Prelude.rnf coreCount
      `Prelude.seq` Prelude.rnf threadsPerCore

instance Data.ToQuery CpuOptionsRequest where
  toQuery CpuOptionsRequest' {..} =
    Prelude.mconcat
      [ "AmdSevSnp" Data.=: amdSevSnp,
        "CoreCount" Data.=: coreCount,
        "ThreadsPerCore" Data.=: threadsPerCore
      ]
