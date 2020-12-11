-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuInfo
  ( GpuInfo (..),

    -- * Smart constructor
    mkGpuInfo,

    -- * Lenses
    giTotalGpuMemoryInMiB,
    giGpus,
  )
where

import Network.AWS.EC2.Types.GpuDeviceInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'mkGpuInfo' smart constructor.
data GpuInfo = GpuInfo'
  { totalGpuMemoryInMiB :: Lude.Maybe Lude.Int,
    gpus :: Lude.Maybe [GpuDeviceInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GpuInfo' with the minimum fields required to make a request.
--
-- * 'gpus' - Describes the GPU accelerators for the instance type.
-- * 'totalGpuMemoryInMiB' - The total size of the memory for the GPU accelerators for the instance type, in MiB.
mkGpuInfo ::
  GpuInfo
mkGpuInfo =
  GpuInfo' {totalGpuMemoryInMiB = Lude.Nothing, gpus = Lude.Nothing}

-- | The total size of the memory for the GPU accelerators for the instance type, in MiB.
--
-- /Note:/ Consider using 'totalGpuMemoryInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giTotalGpuMemoryInMiB :: Lens.Lens' GpuInfo (Lude.Maybe Lude.Int)
giTotalGpuMemoryInMiB = Lens.lens (totalGpuMemoryInMiB :: GpuInfo -> Lude.Maybe Lude.Int) (\s a -> s {totalGpuMemoryInMiB = a} :: GpuInfo)
{-# DEPRECATED giTotalGpuMemoryInMiB "Use generic-lens or generic-optics with 'totalGpuMemoryInMiB' instead." #-}

-- | Describes the GPU accelerators for the instance type.
--
-- /Note:/ Consider using 'gpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGpus :: Lens.Lens' GpuInfo (Lude.Maybe [GpuDeviceInfo])
giGpus = Lens.lens (gpus :: GpuInfo -> Lude.Maybe [GpuDeviceInfo]) (\s a -> s {gpus = a} :: GpuInfo)
{-# DEPRECATED giGpus "Use generic-lens or generic-optics with 'gpus' instead." #-}

instance Lude.FromXML GpuInfo where
  parseXML x =
    GpuInfo'
      Lude.<$> (x Lude..@? "totalGpuMemoryInMiB")
      Lude.<*> ( x Lude..@? "gpus" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
