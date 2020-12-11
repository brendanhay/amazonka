-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStorageInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStorageInfo
  ( InstanceStorageInfo (..),

    -- * Smart constructor
    mkInstanceStorageInfo,

    -- * Lenses
    isiTotalSizeInGB,
    isiNvmeSupport,
    isiDisks,
  )
where

import Network.AWS.EC2.Types.DiskInfo
import Network.AWS.EC2.Types.EphemeralNvmeSupport
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the disks that are available for the instance type.
--
-- /See:/ 'mkInstanceStorageInfo' smart constructor.
data InstanceStorageInfo = InstanceStorageInfo'
  { totalSizeInGB ::
      Lude.Maybe Lude.Integer,
    nvmeSupport :: Lude.Maybe EphemeralNvmeSupport,
    disks :: Lude.Maybe [DiskInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStorageInfo' with the minimum fields required to make a request.
--
-- * 'disks' - Describes the disks that are available for the instance type.
-- * 'nvmeSupport' - Indicates whether non-volatile memory express (NVMe) is supported for instance store.
-- * 'totalSizeInGB' - The total size of the disks, in GB.
mkInstanceStorageInfo ::
  InstanceStorageInfo
mkInstanceStorageInfo =
  InstanceStorageInfo'
    { totalSizeInGB = Lude.Nothing,
      nvmeSupport = Lude.Nothing,
      disks = Lude.Nothing
    }

-- | The total size of the disks, in GB.
--
-- /Note:/ Consider using 'totalSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiTotalSizeInGB :: Lens.Lens' InstanceStorageInfo (Lude.Maybe Lude.Integer)
isiTotalSizeInGB = Lens.lens (totalSizeInGB :: InstanceStorageInfo -> Lude.Maybe Lude.Integer) (\s a -> s {totalSizeInGB = a} :: InstanceStorageInfo)
{-# DEPRECATED isiTotalSizeInGB "Use generic-lens or generic-optics with 'totalSizeInGB' instead." #-}

-- | Indicates whether non-volatile memory express (NVMe) is supported for instance store.
--
-- /Note:/ Consider using 'nvmeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiNvmeSupport :: Lens.Lens' InstanceStorageInfo (Lude.Maybe EphemeralNvmeSupport)
isiNvmeSupport = Lens.lens (nvmeSupport :: InstanceStorageInfo -> Lude.Maybe EphemeralNvmeSupport) (\s a -> s {nvmeSupport = a} :: InstanceStorageInfo)
{-# DEPRECATED isiNvmeSupport "Use generic-lens or generic-optics with 'nvmeSupport' instead." #-}

-- | Describes the disks that are available for the instance type.
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiDisks :: Lens.Lens' InstanceStorageInfo (Lude.Maybe [DiskInfo])
isiDisks = Lens.lens (disks :: InstanceStorageInfo -> Lude.Maybe [DiskInfo]) (\s a -> s {disks = a} :: InstanceStorageInfo)
{-# DEPRECATED isiDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

instance Lude.FromXML InstanceStorageInfo where
  parseXML x =
    InstanceStorageInfo'
      Lude.<$> (x Lude..@? "totalSizeInGB")
      Lude.<*> (x Lude..@? "nvmeSupport")
      Lude.<*> ( x Lude..@? "disks" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
