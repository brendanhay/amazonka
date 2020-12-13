{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshotInfo
  ( DiskSnapshotInfo (..),

    -- * Smart constructor
    mkDiskSnapshotInfo,

    -- * Lenses
    dsiSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a disk snapshot.
--
-- /See:/ 'mkDiskSnapshotInfo' smart constructor.
newtype DiskSnapshotInfo = DiskSnapshotInfo'
  { -- | The size of the disk in GB (e.g., @32@ ).
    sizeInGb :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskSnapshotInfo' with the minimum fields required to make a request.
--
-- * 'sizeInGb' - The size of the disk in GB (e.g., @32@ ).
mkDiskSnapshotInfo ::
  DiskSnapshotInfo
mkDiskSnapshotInfo = DiskSnapshotInfo' {sizeInGb = Lude.Nothing}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiSizeInGb :: Lens.Lens' DiskSnapshotInfo (Lude.Maybe Lude.Int)
dsiSizeInGb = Lens.lens (sizeInGb :: DiskSnapshotInfo -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: DiskSnapshotInfo)
{-# DEPRECATED dsiSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

instance Lude.FromJSON DiskSnapshotInfo where
  parseJSON =
    Lude.withObject
      "DiskSnapshotInfo"
      (\x -> DiskSnapshotInfo' Lude.<$> (x Lude..:? "sizeInGb"))
