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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the vCPU configurations for the instance type.
--
-- /See:/ 'newVCpuInfo' smart constructor.
data VCpuInfo = VCpuInfo'
  { -- | The default number of cores for the instance type.
    defaultCores :: Core.Maybe Core.Int,
    -- | The default number of vCPUs for the instance type.
    defaultVCpus :: Core.Maybe Core.Int,
    -- | The valid number of threads per core that can be configured for the
    -- instance type.
    validThreadsPerCore :: Core.Maybe [Core.Int],
    -- | The valid number of cores that can be configured for the instance type.
    validCores :: Core.Maybe [Core.Int],
    -- | The default number of threads per core for the instance type.
    defaultThreadsPerCore :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { defaultCores = Core.Nothing,
      defaultVCpus = Core.Nothing,
      validThreadsPerCore = Core.Nothing,
      validCores = Core.Nothing,
      defaultThreadsPerCore = Core.Nothing
    }

-- | The default number of cores for the instance type.
vCpuInfo_defaultCores :: Lens.Lens' VCpuInfo (Core.Maybe Core.Int)
vCpuInfo_defaultCores = Lens.lens (\VCpuInfo' {defaultCores} -> defaultCores) (\s@VCpuInfo' {} a -> s {defaultCores = a} :: VCpuInfo)

-- | The default number of vCPUs for the instance type.
vCpuInfo_defaultVCpus :: Lens.Lens' VCpuInfo (Core.Maybe Core.Int)
vCpuInfo_defaultVCpus = Lens.lens (\VCpuInfo' {defaultVCpus} -> defaultVCpus) (\s@VCpuInfo' {} a -> s {defaultVCpus = a} :: VCpuInfo)

-- | The valid number of threads per core that can be configured for the
-- instance type.
vCpuInfo_validThreadsPerCore :: Lens.Lens' VCpuInfo (Core.Maybe [Core.Int])
vCpuInfo_validThreadsPerCore = Lens.lens (\VCpuInfo' {validThreadsPerCore} -> validThreadsPerCore) (\s@VCpuInfo' {} a -> s {validThreadsPerCore = a} :: VCpuInfo) Core.. Lens.mapping Lens._Coerce

-- | The valid number of cores that can be configured for the instance type.
vCpuInfo_validCores :: Lens.Lens' VCpuInfo (Core.Maybe [Core.Int])
vCpuInfo_validCores = Lens.lens (\VCpuInfo' {validCores} -> validCores) (\s@VCpuInfo' {} a -> s {validCores = a} :: VCpuInfo) Core.. Lens.mapping Lens._Coerce

-- | The default number of threads per core for the instance type.
vCpuInfo_defaultThreadsPerCore :: Lens.Lens' VCpuInfo (Core.Maybe Core.Int)
vCpuInfo_defaultThreadsPerCore = Lens.lens (\VCpuInfo' {defaultThreadsPerCore} -> defaultThreadsPerCore) (\s@VCpuInfo' {} a -> s {defaultThreadsPerCore = a} :: VCpuInfo)

instance Core.FromXML VCpuInfo where
  parseXML x =
    VCpuInfo'
      Core.<$> (x Core..@? "defaultCores")
      Core.<*> (x Core..@? "defaultVCpus")
      Core.<*> ( x Core..@? "validThreadsPerCore"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "validCores" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "defaultThreadsPerCore")

instance Core.Hashable VCpuInfo

instance Core.NFData VCpuInfo
