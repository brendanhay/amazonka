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
-- Module      : Network.AWS.EC2.Types.VCpuInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VCpuInfo where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the vCPU configurations for the instance type.
--
-- /See:/ 'newVCpuInfo' smart constructor.
data VCpuInfo = VCpuInfo'
  { -- | The default number of cores for the instance type.
    defaultCores :: Prelude.Maybe Prelude.Int,
    -- | The default number of vCPUs for the instance type.
    defaultVCpus :: Prelude.Maybe Prelude.Int,
    -- | The valid number of threads per core that can be configured for the
    -- instance type.
    validThreadsPerCore :: Prelude.Maybe [Prelude.Int],
    -- | The valid number of cores that can be configured for the instance type.
    validCores :: Prelude.Maybe [Prelude.Int],
    -- | The default number of threads per core for the instance type.
    defaultThreadsPerCore :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'defaultVCpus', 'vCpuInfo_defaultVCpus' - The default number of vCPUs for the instance type.
--
-- 'validThreadsPerCore', 'vCpuInfo_validThreadsPerCore' - The valid number of threads per core that can be configured for the
-- instance type.
--
-- 'validCores', 'vCpuInfo_validCores' - The valid number of cores that can be configured for the instance type.
--
-- 'defaultThreadsPerCore', 'vCpuInfo_defaultThreadsPerCore' - The default number of threads per core for the instance type.
newVCpuInfo ::
  VCpuInfo
newVCpuInfo =
  VCpuInfo'
    { defaultCores = Prelude.Nothing,
      defaultVCpus = Prelude.Nothing,
      validThreadsPerCore = Prelude.Nothing,
      validCores = Prelude.Nothing,
      defaultThreadsPerCore = Prelude.Nothing
    }

-- | The default number of cores for the instance type.
vCpuInfo_defaultCores :: Lens.Lens' VCpuInfo (Prelude.Maybe Prelude.Int)
vCpuInfo_defaultCores = Lens.lens (\VCpuInfo' {defaultCores} -> defaultCores) (\s@VCpuInfo' {} a -> s {defaultCores = a} :: VCpuInfo)

-- | The default number of vCPUs for the instance type.
vCpuInfo_defaultVCpus :: Lens.Lens' VCpuInfo (Prelude.Maybe Prelude.Int)
vCpuInfo_defaultVCpus = Lens.lens (\VCpuInfo' {defaultVCpus} -> defaultVCpus) (\s@VCpuInfo' {} a -> s {defaultVCpus = a} :: VCpuInfo)

-- | The valid number of threads per core that can be configured for the
-- instance type.
vCpuInfo_validThreadsPerCore :: Lens.Lens' VCpuInfo (Prelude.Maybe [Prelude.Int])
vCpuInfo_validThreadsPerCore = Lens.lens (\VCpuInfo' {validThreadsPerCore} -> validThreadsPerCore) (\s@VCpuInfo' {} a -> s {validThreadsPerCore = a} :: VCpuInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The valid number of cores that can be configured for the instance type.
vCpuInfo_validCores :: Lens.Lens' VCpuInfo (Prelude.Maybe [Prelude.Int])
vCpuInfo_validCores = Lens.lens (\VCpuInfo' {validCores} -> validCores) (\s@VCpuInfo' {} a -> s {validCores = a} :: VCpuInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The default number of threads per core for the instance type.
vCpuInfo_defaultThreadsPerCore :: Lens.Lens' VCpuInfo (Prelude.Maybe Prelude.Int)
vCpuInfo_defaultThreadsPerCore = Lens.lens (\VCpuInfo' {defaultThreadsPerCore} -> defaultThreadsPerCore) (\s@VCpuInfo' {} a -> s {defaultThreadsPerCore = a} :: VCpuInfo)

instance Prelude.FromXML VCpuInfo where
  parseXML x =
    VCpuInfo'
      Prelude.<$> (x Prelude..@? "defaultCores")
      Prelude.<*> (x Prelude..@? "defaultVCpus")
      Prelude.<*> ( x Prelude..@? "validThreadsPerCore"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "validCores"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "defaultThreadsPerCore")

instance Prelude.Hashable VCpuInfo

instance Prelude.NFData VCpuInfo
