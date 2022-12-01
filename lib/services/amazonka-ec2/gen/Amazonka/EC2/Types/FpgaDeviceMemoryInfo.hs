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
-- Module      : Amazonka.EC2.Types.FpgaDeviceMemoryInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FpgaDeviceMemoryInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the memory for the FPGA accelerator for the instance type.
--
-- /See:/ 'newFpgaDeviceMemoryInfo' smart constructor.
data FpgaDeviceMemoryInfo = FpgaDeviceMemoryInfo'
  { -- | The size of the memory available to the FPGA accelerator, in MiB.
    sizeInMiB :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FpgaDeviceMemoryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInMiB', 'fpgaDeviceMemoryInfo_sizeInMiB' - The size of the memory available to the FPGA accelerator, in MiB.
newFpgaDeviceMemoryInfo ::
  FpgaDeviceMemoryInfo
newFpgaDeviceMemoryInfo =
  FpgaDeviceMemoryInfo' {sizeInMiB = Prelude.Nothing}

-- | The size of the memory available to the FPGA accelerator, in MiB.
fpgaDeviceMemoryInfo_sizeInMiB :: Lens.Lens' FpgaDeviceMemoryInfo (Prelude.Maybe Prelude.Int)
fpgaDeviceMemoryInfo_sizeInMiB = Lens.lens (\FpgaDeviceMemoryInfo' {sizeInMiB} -> sizeInMiB) (\s@FpgaDeviceMemoryInfo' {} a -> s {sizeInMiB = a} :: FpgaDeviceMemoryInfo)

instance Core.FromXML FpgaDeviceMemoryInfo where
  parseXML x =
    FpgaDeviceMemoryInfo'
      Prelude.<$> (x Core..@? "sizeInMiB")

instance Prelude.Hashable FpgaDeviceMemoryInfo where
  hashWithSalt _salt FpgaDeviceMemoryInfo' {..} =
    _salt `Prelude.hashWithSalt` sizeInMiB

instance Prelude.NFData FpgaDeviceMemoryInfo where
  rnf FpgaDeviceMemoryInfo' {..} = Prelude.rnf sizeInMiB
