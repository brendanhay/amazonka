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
-- Module      : Network.AWS.EC2.Types.FpgaDeviceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaDeviceInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the FPGA accelerator for the instance type.
--
-- /See:/ 'newFpgaDeviceInfo' smart constructor.
data FpgaDeviceInfo = FpgaDeviceInfo'
  { -- | Describes the memory for the FPGA accelerator for the instance type.
    memoryInfo :: Prelude.Maybe FpgaDeviceMemoryInfo,
    -- | The manufacturer of the FPGA accelerator.
    manufacturer :: Prelude.Maybe Prelude.Text,
    -- | The name of the FPGA accelerator.
    name :: Prelude.Maybe Prelude.Text,
    -- | The count of FPGA accelerators for the instance type.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FpgaDeviceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memoryInfo', 'fpgaDeviceInfo_memoryInfo' - Describes the memory for the FPGA accelerator for the instance type.
--
-- 'manufacturer', 'fpgaDeviceInfo_manufacturer' - The manufacturer of the FPGA accelerator.
--
-- 'name', 'fpgaDeviceInfo_name' - The name of the FPGA accelerator.
--
-- 'count', 'fpgaDeviceInfo_count' - The count of FPGA accelerators for the instance type.
newFpgaDeviceInfo ::
  FpgaDeviceInfo
newFpgaDeviceInfo =
  FpgaDeviceInfo'
    { memoryInfo = Prelude.Nothing,
      manufacturer = Prelude.Nothing,
      name = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | Describes the memory for the FPGA accelerator for the instance type.
fpgaDeviceInfo_memoryInfo :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe FpgaDeviceMemoryInfo)
fpgaDeviceInfo_memoryInfo = Lens.lens (\FpgaDeviceInfo' {memoryInfo} -> memoryInfo) (\s@FpgaDeviceInfo' {} a -> s {memoryInfo = a} :: FpgaDeviceInfo)

-- | The manufacturer of the FPGA accelerator.
fpgaDeviceInfo_manufacturer :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe Prelude.Text)
fpgaDeviceInfo_manufacturer = Lens.lens (\FpgaDeviceInfo' {manufacturer} -> manufacturer) (\s@FpgaDeviceInfo' {} a -> s {manufacturer = a} :: FpgaDeviceInfo)

-- | The name of the FPGA accelerator.
fpgaDeviceInfo_name :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe Prelude.Text)
fpgaDeviceInfo_name = Lens.lens (\FpgaDeviceInfo' {name} -> name) (\s@FpgaDeviceInfo' {} a -> s {name = a} :: FpgaDeviceInfo)

-- | The count of FPGA accelerators for the instance type.
fpgaDeviceInfo_count :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe Prelude.Int)
fpgaDeviceInfo_count = Lens.lens (\FpgaDeviceInfo' {count} -> count) (\s@FpgaDeviceInfo' {} a -> s {count = a} :: FpgaDeviceInfo)

instance Prelude.FromXML FpgaDeviceInfo where
  parseXML x =
    FpgaDeviceInfo'
      Prelude.<$> (x Prelude..@? "memoryInfo")
      Prelude.<*> (x Prelude..@? "manufacturer")
      Prelude.<*> (x Prelude..@? "name")
      Prelude.<*> (x Prelude..@? "count")

instance Prelude.Hashable FpgaDeviceInfo

instance Prelude.NFData FpgaDeviceInfo
