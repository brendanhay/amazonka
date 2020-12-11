-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaDeviceInfo
  ( FpgaDeviceInfo (..),

    -- * Smart constructor
    mkFpgaDeviceInfo,

    -- * Lenses
    fdiMemoryInfo,
    fdiManufacturer,
    fdiCount,
    fdiName,
  )
where

import Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the FPGA accelerator for the instance type.
--
-- /See:/ 'mkFpgaDeviceInfo' smart constructor.
data FpgaDeviceInfo = FpgaDeviceInfo'
  { memoryInfo ::
      Lude.Maybe FpgaDeviceMemoryInfo,
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

-- | Creates a value of 'FpgaDeviceInfo' with the minimum fields required to make a request.
--
-- * 'count' - The count of FPGA accelerators for the instance type.
-- * 'manufacturer' - The manufacturer of the FPGA accelerator.
-- * 'memoryInfo' - Describes the memory for the FPGA accelerator for the instance type.
-- * 'name' - The name of the FPGA accelerator.
mkFpgaDeviceInfo ::
  FpgaDeviceInfo
mkFpgaDeviceInfo =
  FpgaDeviceInfo'
    { memoryInfo = Lude.Nothing,
      manufacturer = Lude.Nothing,
      count = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Describes the memory for the FPGA accelerator for the instance type.
--
-- /Note:/ Consider using 'memoryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiMemoryInfo :: Lens.Lens' FpgaDeviceInfo (Lude.Maybe FpgaDeviceMemoryInfo)
fdiMemoryInfo = Lens.lens (memoryInfo :: FpgaDeviceInfo -> Lude.Maybe FpgaDeviceMemoryInfo) (\s a -> s {memoryInfo = a} :: FpgaDeviceInfo)
{-# DEPRECATED fdiMemoryInfo "Use generic-lens or generic-optics with 'memoryInfo' instead." #-}

-- | The manufacturer of the FPGA accelerator.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiManufacturer :: Lens.Lens' FpgaDeviceInfo (Lude.Maybe Lude.Text)
fdiManufacturer = Lens.lens (manufacturer :: FpgaDeviceInfo -> Lude.Maybe Lude.Text) (\s a -> s {manufacturer = a} :: FpgaDeviceInfo)
{-# DEPRECATED fdiManufacturer "Use generic-lens or generic-optics with 'manufacturer' instead." #-}

-- | The count of FPGA accelerators for the instance type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiCount :: Lens.Lens' FpgaDeviceInfo (Lude.Maybe Lude.Int)
fdiCount = Lens.lens (count :: FpgaDeviceInfo -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: FpgaDeviceInfo)
{-# DEPRECATED fdiCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The name of the FPGA accelerator.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiName :: Lens.Lens' FpgaDeviceInfo (Lude.Maybe Lude.Text)
fdiName = Lens.lens (name :: FpgaDeviceInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FpgaDeviceInfo)
{-# DEPRECATED fdiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML FpgaDeviceInfo where
  parseXML x =
    FpgaDeviceInfo'
      Lude.<$> (x Lude..@? "memoryInfo")
      Lude.<*> (x Lude..@? "manufacturer")
      Lude.<*> (x Lude..@? "count")
      Lude.<*> (x Lude..@? "name")
