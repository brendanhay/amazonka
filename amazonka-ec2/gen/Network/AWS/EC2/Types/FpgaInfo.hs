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
-- Module      : Network.AWS.EC2.Types.FpgaInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaDeviceInfo
import qualified Network.AWS.Lens as Lens

-- | Describes the FPGAs for the instance type.
--
-- /See:/ 'newFpgaInfo' smart constructor.
data FpgaInfo = FpgaInfo'
  { -- | The total memory of all FPGA accelerators for the instance type.
    totalFpgaMemoryInMiB :: Core.Maybe Core.Int,
    -- | Describes the FPGAs for the instance type.
    fpgas :: Core.Maybe [FpgaDeviceInfo]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FpgaInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalFpgaMemoryInMiB', 'fpgaInfo_totalFpgaMemoryInMiB' - The total memory of all FPGA accelerators for the instance type.
--
-- 'fpgas', 'fpgaInfo_fpgas' - Describes the FPGAs for the instance type.
newFpgaInfo ::
  FpgaInfo
newFpgaInfo =
  FpgaInfo'
    { totalFpgaMemoryInMiB = Core.Nothing,
      fpgas = Core.Nothing
    }

-- | The total memory of all FPGA accelerators for the instance type.
fpgaInfo_totalFpgaMemoryInMiB :: Lens.Lens' FpgaInfo (Core.Maybe Core.Int)
fpgaInfo_totalFpgaMemoryInMiB = Lens.lens (\FpgaInfo' {totalFpgaMemoryInMiB} -> totalFpgaMemoryInMiB) (\s@FpgaInfo' {} a -> s {totalFpgaMemoryInMiB = a} :: FpgaInfo)

-- | Describes the FPGAs for the instance type.
fpgaInfo_fpgas :: Lens.Lens' FpgaInfo (Core.Maybe [FpgaDeviceInfo])
fpgaInfo_fpgas = Lens.lens (\FpgaInfo' {fpgas} -> fpgas) (\s@FpgaInfo' {} a -> s {fpgas = a} :: FpgaInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML FpgaInfo where
  parseXML x =
    FpgaInfo'
      Core.<$> (x Core..@? "totalFpgaMemoryInMiB")
      Core.<*> ( x Core..@? "fpgas" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable FpgaInfo

instance Core.NFData FpgaInfo
