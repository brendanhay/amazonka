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
-- Module      : Amazonka.EC2.Types.VCpuInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VCpuInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the vCPU configurations for the instance type.
--
-- /See:/ 'newVCpuInfo' smart constructor.
data VCpuInfo = VCpuInfo'
  { -- | The default number of cores for the instance type.
    defaultCores :: Prelude.Maybe Prelude.Int,
    -- | The default number of threads per core for the instance type.
    defaultThreadsPerCore :: Prelude.Maybe Prelude.Int,
    -- | The default number of vCPUs for the instance type.
    defaultVCpus :: Prelude.Maybe Prelude.Int,
    -- | The valid number of cores that can be configured for the instance type.
    validCores :: Prelude.Maybe [Prelude.Int],
    -- | The valid number of threads per core that can be configured for the
    -- instance type.
    validThreadsPerCore :: Prelude.Maybe [Prelude.Int]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VCpuInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultCores', 'vCpuInfo_defaultCores' - The default number of cores for the instance type.
--
-- 'defaultThreadsPerCore', 'vCpuInfo_defaultThreadsPerCore' - The default number of threads per core for the instance type.
--
-- 'defaultVCpus', 'vCpuInfo_defaultVCpus' - The default number of vCPUs for the instance type.
--
-- 'validCores', 'vCpuInfo_validCores' - The valid number of cores that can be configured for the instance type.
--
-- 'validThreadsPerCore', 'vCpuInfo_validThreadsPerCore' - The valid number of threads per core that can be configured for the
-- instance type.
newVCpuInfo ::
  VCpuInfo
newVCpuInfo =
  VCpuInfo'
    { defaultCores = Prelude.Nothing,
      defaultThreadsPerCore = Prelude.Nothing,
      defaultVCpus = Prelude.Nothing,
      validCores = Prelude.Nothing,
      validThreadsPerCore = Prelude.Nothing
    }

-- | The default number of cores for the instance type.
vCpuInfo_defaultCores :: Lens.Lens' VCpuInfo (Prelude.Maybe Prelude.Int)
vCpuInfo_defaultCores = Lens.lens (\VCpuInfo' {defaultCores} -> defaultCores) (\s@VCpuInfo' {} a -> s {defaultCores = a} :: VCpuInfo)

-- | The default number of threads per core for the instance type.
vCpuInfo_defaultThreadsPerCore :: Lens.Lens' VCpuInfo (Prelude.Maybe Prelude.Int)
vCpuInfo_defaultThreadsPerCore = Lens.lens (\VCpuInfo' {defaultThreadsPerCore} -> defaultThreadsPerCore) (\s@VCpuInfo' {} a -> s {defaultThreadsPerCore = a} :: VCpuInfo)

-- | The default number of vCPUs for the instance type.
vCpuInfo_defaultVCpus :: Lens.Lens' VCpuInfo (Prelude.Maybe Prelude.Int)
vCpuInfo_defaultVCpus = Lens.lens (\VCpuInfo' {defaultVCpus} -> defaultVCpus) (\s@VCpuInfo' {} a -> s {defaultVCpus = a} :: VCpuInfo)

-- | The valid number of cores that can be configured for the instance type.
vCpuInfo_validCores :: Lens.Lens' VCpuInfo (Prelude.Maybe [Prelude.Int])
vCpuInfo_validCores = Lens.lens (\VCpuInfo' {validCores} -> validCores) (\s@VCpuInfo' {} a -> s {validCores = a} :: VCpuInfo) Prelude.. Lens.mapping Lens.coerced

-- | The valid number of threads per core that can be configured for the
-- instance type.
vCpuInfo_validThreadsPerCore :: Lens.Lens' VCpuInfo (Prelude.Maybe [Prelude.Int])
vCpuInfo_validThreadsPerCore = Lens.lens (\VCpuInfo' {validThreadsPerCore} -> validThreadsPerCore) (\s@VCpuInfo' {} a -> s {validThreadsPerCore = a} :: VCpuInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML VCpuInfo where
  parseXML x =
    VCpuInfo'
      Prelude.<$> (x Data..@? "defaultCores")
      Prelude.<*> (x Data..@? "defaultThreadsPerCore")
      Prelude.<*> (x Data..@? "defaultVCpus")
      Prelude.<*> ( x
                      Data..@? "validCores"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "validThreadsPerCore"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable VCpuInfo where
  hashWithSalt _salt VCpuInfo' {..} =
    _salt
      `Prelude.hashWithSalt` defaultCores
      `Prelude.hashWithSalt` defaultThreadsPerCore
      `Prelude.hashWithSalt` defaultVCpus
      `Prelude.hashWithSalt` validCores
      `Prelude.hashWithSalt` validThreadsPerCore

instance Prelude.NFData VCpuInfo where
  rnf VCpuInfo' {..} =
    Prelude.rnf defaultCores
      `Prelude.seq` Prelude.rnf defaultThreadsPerCore
      `Prelude.seq` Prelude.rnf defaultVCpus
      `Prelude.seq` Prelude.rnf validCores
      `Prelude.seq` Prelude.rnf validThreadsPerCore
