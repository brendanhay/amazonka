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
-- Module      : Network.AWS.EC2.Types.GpuDeviceMemoryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuDeviceMemoryInfo where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the memory available to the GPU accelerator.
--
-- /See:/ 'newGpuDeviceMemoryInfo' smart constructor.
data GpuDeviceMemoryInfo = GpuDeviceMemoryInfo'
  { -- | The size of the memory available to the GPU accelerator, in MiB.
    sizeInMiB :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GpuDeviceMemoryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInMiB', 'gpuDeviceMemoryInfo_sizeInMiB' - The size of the memory available to the GPU accelerator, in MiB.
newGpuDeviceMemoryInfo ::
  GpuDeviceMemoryInfo
newGpuDeviceMemoryInfo =
  GpuDeviceMemoryInfo' {sizeInMiB = Prelude.Nothing}

-- | The size of the memory available to the GPU accelerator, in MiB.
gpuDeviceMemoryInfo_sizeInMiB :: Lens.Lens' GpuDeviceMemoryInfo (Prelude.Maybe Prelude.Int)
gpuDeviceMemoryInfo_sizeInMiB = Lens.lens (\GpuDeviceMemoryInfo' {sizeInMiB} -> sizeInMiB) (\s@GpuDeviceMemoryInfo' {} a -> s {sizeInMiB = a} :: GpuDeviceMemoryInfo)

instance Prelude.FromXML GpuDeviceMemoryInfo where
  parseXML x =
    GpuDeviceMemoryInfo'
      Prelude.<$> (x Prelude..@? "sizeInMiB")

instance Prelude.Hashable GpuDeviceMemoryInfo

instance Prelude.NFData GpuDeviceMemoryInfo
