-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaInfo
  ( FpgaInfo (..),

    -- * Smart constructor
    mkFpgaInfo,

    -- * Lenses
    fiTotalFpgaMemoryInMiB,
    fiFpgas,
  )
where

import Network.AWS.EC2.Types.FpgaDeviceInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the FPGAs for the instance type.
--
-- /See:/ 'mkFpgaInfo' smart constructor.
data FpgaInfo = FpgaInfo'
  { totalFpgaMemoryInMiB ::
      Lude.Maybe Lude.Int,
    fpgas :: Lude.Maybe [FpgaDeviceInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FpgaInfo' with the minimum fields required to make a request.
--
-- * 'fpgas' - Describes the FPGAs for the instance type.
-- * 'totalFpgaMemoryInMiB' - The total memory of all FPGA accelerators for the instance type.
mkFpgaInfo ::
  FpgaInfo
mkFpgaInfo =
  FpgaInfo'
    { totalFpgaMemoryInMiB = Lude.Nothing,
      fpgas = Lude.Nothing
    }

-- | The total memory of all FPGA accelerators for the instance type.
--
-- /Note:/ Consider using 'totalFpgaMemoryInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiTotalFpgaMemoryInMiB :: Lens.Lens' FpgaInfo (Lude.Maybe Lude.Int)
fiTotalFpgaMemoryInMiB = Lens.lens (totalFpgaMemoryInMiB :: FpgaInfo -> Lude.Maybe Lude.Int) (\s a -> s {totalFpgaMemoryInMiB = a} :: FpgaInfo)
{-# DEPRECATED fiTotalFpgaMemoryInMiB "Use generic-lens or generic-optics with 'totalFpgaMemoryInMiB' instead." #-}

-- | Describes the FPGAs for the instance type.
--
-- /Note:/ Consider using 'fpgas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgas :: Lens.Lens' FpgaInfo (Lude.Maybe [FpgaDeviceInfo])
fiFpgas = Lens.lens (fpgas :: FpgaInfo -> Lude.Maybe [FpgaDeviceInfo]) (\s a -> s {fpgas = a} :: FpgaInfo)
{-# DEPRECATED fiFpgas "Use generic-lens or generic-optics with 'fpgas' instead." #-}

instance Lude.FromXML FpgaInfo where
  parseXML x =
    FpgaInfo'
      Lude.<$> (x Lude..@? "totalFpgaMemoryInMiB")
      Lude.<*> ( x Lude..@? "fpgas" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
