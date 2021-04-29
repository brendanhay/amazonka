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
-- Module      : Network.AWS.EC2.Types.FpgaInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaDeviceInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the FPGAs for the instance type.
--
-- /See:/ 'newFpgaInfo' smart constructor.
data FpgaInfo = FpgaInfo'
  { -- | The total memory of all FPGA accelerators for the instance type.
    totalFpgaMemoryInMiB :: Prelude.Maybe Prelude.Int,
    -- | Describes the FPGAs for the instance type.
    fpgas :: Prelude.Maybe [FpgaDeviceInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { totalFpgaMemoryInMiB = Prelude.Nothing,
      fpgas = Prelude.Nothing
    }

-- | The total memory of all FPGA accelerators for the instance type.
fpgaInfo_totalFpgaMemoryInMiB :: Lens.Lens' FpgaInfo (Prelude.Maybe Prelude.Int)
fpgaInfo_totalFpgaMemoryInMiB = Lens.lens (\FpgaInfo' {totalFpgaMemoryInMiB} -> totalFpgaMemoryInMiB) (\s@FpgaInfo' {} a -> s {totalFpgaMemoryInMiB = a} :: FpgaInfo)

-- | Describes the FPGAs for the instance type.
fpgaInfo_fpgas :: Lens.Lens' FpgaInfo (Prelude.Maybe [FpgaDeviceInfo])
fpgaInfo_fpgas = Lens.lens (\FpgaInfo' {fpgas} -> fpgas) (\s@FpgaInfo' {} a -> s {fpgas = a} :: FpgaInfo) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML FpgaInfo where
  parseXML x =
    FpgaInfo'
      Prelude.<$> (x Prelude..@? "totalFpgaMemoryInMiB")
      Prelude.<*> ( x Prelude..@? "fpgas" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable FpgaInfo

instance Prelude.NFData FpgaInfo
