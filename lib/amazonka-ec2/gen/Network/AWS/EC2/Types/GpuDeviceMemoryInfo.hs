{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuDeviceMemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuDeviceMemoryInfo
  ( GpuDeviceMemoryInfo (..),

    -- * Smart constructor
    mkGpuDeviceMemoryInfo,

    -- * Lenses
    gdmiSizeInMiB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the memory available to the GPU accelerator.
--
-- /See:/ 'mkGpuDeviceMemoryInfo' smart constructor.
newtype GpuDeviceMemoryInfo = GpuDeviceMemoryInfo'
  { sizeInMiB ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GpuDeviceMemoryInfo' with the minimum fields required to make a request.
--
-- * 'sizeInMiB' - The size of the memory available to the GPU accelerator, in MiB.
mkGpuDeviceMemoryInfo ::
  GpuDeviceMemoryInfo
mkGpuDeviceMemoryInfo =
  GpuDeviceMemoryInfo' {sizeInMiB = Lude.Nothing}

-- | The size of the memory available to the GPU accelerator, in MiB.
--
-- /Note:/ Consider using 'sizeInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdmiSizeInMiB :: Lens.Lens' GpuDeviceMemoryInfo (Lude.Maybe Lude.Int)
gdmiSizeInMiB = Lens.lens (sizeInMiB :: GpuDeviceMemoryInfo -> Lude.Maybe Lude.Int) (\s a -> s {sizeInMiB = a} :: GpuDeviceMemoryInfo)
{-# DEPRECATED gdmiSizeInMiB "Use generic-lens or generic-optics with 'sizeInMiB' instead." #-}

instance Lude.FromXML GpuDeviceMemoryInfo where
  parseXML x = GpuDeviceMemoryInfo' Lude.<$> (x Lude..@? "sizeInMiB")
