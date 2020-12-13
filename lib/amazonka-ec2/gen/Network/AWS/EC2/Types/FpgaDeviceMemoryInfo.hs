{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
  ( FpgaDeviceMemoryInfo (..),

    -- * Smart constructor
    mkFpgaDeviceMemoryInfo,

    -- * Lenses
    fdmiSizeInMiB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the memory for the FPGA accelerator for the instance type.
--
-- /See:/ 'mkFpgaDeviceMemoryInfo' smart constructor.
newtype FpgaDeviceMemoryInfo = FpgaDeviceMemoryInfo'
  { -- | The size of the memory available to the FPGA accelerator, in MiB.
    sizeInMiB :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FpgaDeviceMemoryInfo' with the minimum fields required to make a request.
--
-- * 'sizeInMiB' - The size of the memory available to the FPGA accelerator, in MiB.
mkFpgaDeviceMemoryInfo ::
  FpgaDeviceMemoryInfo
mkFpgaDeviceMemoryInfo =
  FpgaDeviceMemoryInfo' {sizeInMiB = Lude.Nothing}

-- | The size of the memory available to the FPGA accelerator, in MiB.
--
-- /Note:/ Consider using 'sizeInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdmiSizeInMiB :: Lens.Lens' FpgaDeviceMemoryInfo (Lude.Maybe Lude.Int)
fdmiSizeInMiB = Lens.lens (sizeInMiB :: FpgaDeviceMemoryInfo -> Lude.Maybe Lude.Int) (\s a -> s {sizeInMiB = a} :: FpgaDeviceMemoryInfo)
{-# DEPRECATED fdmiSizeInMiB "Use generic-lens or generic-optics with 'sizeInMiB' instead." #-}

instance Lude.FromXML FpgaDeviceMemoryInfo where
  parseXML x =
    FpgaDeviceMemoryInfo' Lude.<$> (x Lude..@? "sizeInMiB")
