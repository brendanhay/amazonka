{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuDeviceInfo
  ( GpuDeviceInfo (..),

    -- * Smart constructor
    mkGpuDeviceInfo,

    -- * Lenses
    gdiMemoryInfo,
    gdiManufacturer,
    gdiCount,
    gdiName,
  )
where

import Network.AWS.EC2.Types.GpuDeviceMemoryInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'mkGpuDeviceInfo' smart constructor.
data GpuDeviceInfo = GpuDeviceInfo'
  { memoryInfo ::
      Lude.Maybe GpuDeviceMemoryInfo,
    manufacturer :: Lude.Maybe Lude.Text,
    count :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GpuDeviceInfo' with the minimum fields required to make a request.
--
-- * 'count' - The number of GPUs for the instance type.
-- * 'manufacturer' - The manufacturer of the GPU accelerator.
-- * 'memoryInfo' - Describes the memory available to the GPU accelerator.
-- * 'name' - The name of the GPU accelerator.
mkGpuDeviceInfo ::
  GpuDeviceInfo
mkGpuDeviceInfo =
  GpuDeviceInfo'
    { memoryInfo = Lude.Nothing,
      manufacturer = Lude.Nothing,
      count = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Describes the memory available to the GPU accelerator.
--
-- /Note:/ Consider using 'memoryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiMemoryInfo :: Lens.Lens' GpuDeviceInfo (Lude.Maybe GpuDeviceMemoryInfo)
gdiMemoryInfo = Lens.lens (memoryInfo :: GpuDeviceInfo -> Lude.Maybe GpuDeviceMemoryInfo) (\s a -> s {memoryInfo = a} :: GpuDeviceInfo)
{-# DEPRECATED gdiMemoryInfo "Use generic-lens or generic-optics with 'memoryInfo' instead." #-}

-- | The manufacturer of the GPU accelerator.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiManufacturer :: Lens.Lens' GpuDeviceInfo (Lude.Maybe Lude.Text)
gdiManufacturer = Lens.lens (manufacturer :: GpuDeviceInfo -> Lude.Maybe Lude.Text) (\s a -> s {manufacturer = a} :: GpuDeviceInfo)
{-# DEPRECATED gdiManufacturer "Use generic-lens or generic-optics with 'manufacturer' instead." #-}

-- | The number of GPUs for the instance type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiCount :: Lens.Lens' GpuDeviceInfo (Lude.Maybe Lude.Int)
gdiCount = Lens.lens (count :: GpuDeviceInfo -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: GpuDeviceInfo)
{-# DEPRECATED gdiCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The name of the GPU accelerator.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiName :: Lens.Lens' GpuDeviceInfo (Lude.Maybe Lude.Text)
gdiName = Lens.lens (name :: GpuDeviceInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GpuDeviceInfo)
{-# DEPRECATED gdiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML GpuDeviceInfo where
  parseXML x =
    GpuDeviceInfo'
      Lude.<$> (x Lude..@? "memoryInfo")
      Lude.<*> (x Lude..@? "manufacturer")
      Lude.<*> (x Lude..@? "count")
      Lude.<*> (x Lude..@? "name")
