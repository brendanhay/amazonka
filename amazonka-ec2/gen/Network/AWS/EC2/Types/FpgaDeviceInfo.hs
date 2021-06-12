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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
import qualified Network.AWS.Lens as Lens

-- | Describes the FPGA accelerator for the instance type.
--
-- /See:/ 'newFpgaDeviceInfo' smart constructor.
data FpgaDeviceInfo = FpgaDeviceInfo'
  { -- | Describes the memory for the FPGA accelerator for the instance type.
    memoryInfo :: Core.Maybe FpgaDeviceMemoryInfo,
    -- | The manufacturer of the FPGA accelerator.
    manufacturer :: Core.Maybe Core.Text,
    -- | The name of the FPGA accelerator.
    name :: Core.Maybe Core.Text,
    -- | The count of FPGA accelerators for the instance type.
    count :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { memoryInfo = Core.Nothing,
      manufacturer = Core.Nothing,
      name = Core.Nothing,
      count = Core.Nothing
    }

-- | Describes the memory for the FPGA accelerator for the instance type.
fpgaDeviceInfo_memoryInfo :: Lens.Lens' FpgaDeviceInfo (Core.Maybe FpgaDeviceMemoryInfo)
fpgaDeviceInfo_memoryInfo = Lens.lens (\FpgaDeviceInfo' {memoryInfo} -> memoryInfo) (\s@FpgaDeviceInfo' {} a -> s {memoryInfo = a} :: FpgaDeviceInfo)

-- | The manufacturer of the FPGA accelerator.
fpgaDeviceInfo_manufacturer :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Core.Text)
fpgaDeviceInfo_manufacturer = Lens.lens (\FpgaDeviceInfo' {manufacturer} -> manufacturer) (\s@FpgaDeviceInfo' {} a -> s {manufacturer = a} :: FpgaDeviceInfo)

-- | The name of the FPGA accelerator.
fpgaDeviceInfo_name :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Core.Text)
fpgaDeviceInfo_name = Lens.lens (\FpgaDeviceInfo' {name} -> name) (\s@FpgaDeviceInfo' {} a -> s {name = a} :: FpgaDeviceInfo)

-- | The count of FPGA accelerators for the instance type.
fpgaDeviceInfo_count :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Core.Int)
fpgaDeviceInfo_count = Lens.lens (\FpgaDeviceInfo' {count} -> count) (\s@FpgaDeviceInfo' {} a -> s {count = a} :: FpgaDeviceInfo)

instance Core.FromXML FpgaDeviceInfo where
  parseXML x =
    FpgaDeviceInfo'
      Core.<$> (x Core..@? "memoryInfo")
      Core.<*> (x Core..@? "manufacturer")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@? "count")

instance Core.Hashable FpgaDeviceInfo

instance Core.NFData FpgaDeviceInfo
