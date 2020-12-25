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
import qualified Network.AWS.Prelude as Core

-- | Describes a disk snapshot.
--
-- /See:/ 'mkDiskSnapshotInfo' smart constructor.
newtype DiskSnapshotInfo = DiskSnapshotInfo'
  { -- | The size of the disk in GB (e.g., @32@ ).
    sizeInGb :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DiskSnapshotInfo' value with any optional fields omitted.
mkDiskSnapshotInfo ::
  DiskSnapshotInfo
mkDiskSnapshotInfo = DiskSnapshotInfo' {sizeInGb = Core.Nothing}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiSizeInGb :: Lens.Lens' DiskSnapshotInfo (Core.Maybe Core.Int)
dsiSizeInGb = Lens.field @"sizeInGb"
{-# DEPRECATED dsiSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

instance Core.FromJSON DiskSnapshotInfo where
  parseJSON =
    Core.withObject "DiskSnapshotInfo" Core.$
      \x -> DiskSnapshotInfo' Core.<$> (x Core..:? "sizeInGb")
