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
-- Module      : Amazonka.EC2.Types.FpgaDeviceInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FpgaDeviceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FpgaDeviceMemoryInfo
import qualified Amazonka.Prelude as Prelude

-- | Describes the FPGA accelerator for the instance type.
--
-- /See:/ 'newFpgaDeviceInfo' smart constructor.
data FpgaDeviceInfo = FpgaDeviceInfo'
  { -- | The count of FPGA accelerators for the instance type.
    count :: Prelude.Maybe Prelude.Int,
    -- | The manufacturer of the FPGA accelerator.
    manufacturer :: Prelude.Maybe Prelude.Text,
    -- | Describes the memory for the FPGA accelerator for the instance type.
    memoryInfo :: Prelude.Maybe FpgaDeviceMemoryInfo,
    -- | The name of the FPGA accelerator.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FpgaDeviceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'fpgaDeviceInfo_count' - The count of FPGA accelerators for the instance type.
--
-- 'manufacturer', 'fpgaDeviceInfo_manufacturer' - The manufacturer of the FPGA accelerator.
--
-- 'memoryInfo', 'fpgaDeviceInfo_memoryInfo' - Describes the memory for the FPGA accelerator for the instance type.
--
-- 'name', 'fpgaDeviceInfo_name' - The name of the FPGA accelerator.
newFpgaDeviceInfo ::
  FpgaDeviceInfo
newFpgaDeviceInfo =
  FpgaDeviceInfo'
    { count = Prelude.Nothing,
      manufacturer = Prelude.Nothing,
      memoryInfo = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The count of FPGA accelerators for the instance type.
fpgaDeviceInfo_count :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe Prelude.Int)
fpgaDeviceInfo_count = Lens.lens (\FpgaDeviceInfo' {count} -> count) (\s@FpgaDeviceInfo' {} a -> s {count = a} :: FpgaDeviceInfo)

-- | The manufacturer of the FPGA accelerator.
fpgaDeviceInfo_manufacturer :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe Prelude.Text)
fpgaDeviceInfo_manufacturer = Lens.lens (\FpgaDeviceInfo' {manufacturer} -> manufacturer) (\s@FpgaDeviceInfo' {} a -> s {manufacturer = a} :: FpgaDeviceInfo)

-- | Describes the memory for the FPGA accelerator for the instance type.
fpgaDeviceInfo_memoryInfo :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe FpgaDeviceMemoryInfo)
fpgaDeviceInfo_memoryInfo = Lens.lens (\FpgaDeviceInfo' {memoryInfo} -> memoryInfo) (\s@FpgaDeviceInfo' {} a -> s {memoryInfo = a} :: FpgaDeviceInfo)

-- | The name of the FPGA accelerator.
fpgaDeviceInfo_name :: Lens.Lens' FpgaDeviceInfo (Prelude.Maybe Prelude.Text)
fpgaDeviceInfo_name = Lens.lens (\FpgaDeviceInfo' {name} -> name) (\s@FpgaDeviceInfo' {} a -> s {name = a} :: FpgaDeviceInfo)

instance Data.FromXML FpgaDeviceInfo where
  parseXML x =
    FpgaDeviceInfo'
      Prelude.<$> (x Data..@? "count")
      Prelude.<*> (x Data..@? "manufacturer")
      Prelude.<*> (x Data..@? "memoryInfo")
      Prelude.<*> (x Data..@? "name")

instance Prelude.Hashable FpgaDeviceInfo where
  hashWithSalt _salt FpgaDeviceInfo' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` manufacturer
      `Prelude.hashWithSalt` memoryInfo
      `Prelude.hashWithSalt` name

instance Prelude.NFData FpgaDeviceInfo where
  rnf FpgaDeviceInfo' {..} =
    Prelude.rnf count `Prelude.seq`
      Prelude.rnf manufacturer `Prelude.seq`
        Prelude.rnf memoryInfo `Prelude.seq`
          Prelude.rnf name
