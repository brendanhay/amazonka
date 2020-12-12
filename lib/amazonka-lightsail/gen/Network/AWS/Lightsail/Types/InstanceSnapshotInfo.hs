{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceSnapshotInfo
  ( InstanceSnapshotInfo (..),

    -- * Smart constructor
    mkInstanceSnapshotInfo,

    -- * Lenses
    isiFromBlueprintId,
    isiFromBundleId,
    isiFromDiskInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DiskInfo
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance snapshot.
--
-- /See:/ 'mkInstanceSnapshotInfo' smart constructor.
data InstanceSnapshotInfo = InstanceSnapshotInfo'
  { fromBlueprintId ::
      Lude.Maybe Lude.Text,
    fromBundleId :: Lude.Maybe Lude.Text,
    fromDiskInfo :: Lude.Maybe [DiskInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceSnapshotInfo' with the minimum fields required to make a request.
--
-- * 'fromBlueprintId' - The blueprint ID from which the source instance (e.g., @os_debian_8_3@ ).
-- * 'fromBundleId' - The bundle ID from which the source instance was created (e.g., @micro_1_0@ ).
-- * 'fromDiskInfo' - A list of objects describing the disks that were attached to the source instance.
mkInstanceSnapshotInfo ::
  InstanceSnapshotInfo
mkInstanceSnapshotInfo =
  InstanceSnapshotInfo'
    { fromBlueprintId = Lude.Nothing,
      fromBundleId = Lude.Nothing,
      fromDiskInfo = Lude.Nothing
    }

-- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@ ).
--
-- /Note:/ Consider using 'fromBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiFromBlueprintId :: Lens.Lens' InstanceSnapshotInfo (Lude.Maybe Lude.Text)
isiFromBlueprintId = Lens.lens (fromBlueprintId :: InstanceSnapshotInfo -> Lude.Maybe Lude.Text) (\s a -> s {fromBlueprintId = a} :: InstanceSnapshotInfo)
{-# DEPRECATED isiFromBlueprintId "Use generic-lens or generic-optics with 'fromBlueprintId' instead." #-}

-- | The bundle ID from which the source instance was created (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'fromBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiFromBundleId :: Lens.Lens' InstanceSnapshotInfo (Lude.Maybe Lude.Text)
isiFromBundleId = Lens.lens (fromBundleId :: InstanceSnapshotInfo -> Lude.Maybe Lude.Text) (\s a -> s {fromBundleId = a} :: InstanceSnapshotInfo)
{-# DEPRECATED isiFromBundleId "Use generic-lens or generic-optics with 'fromBundleId' instead." #-}

-- | A list of objects describing the disks that were attached to the source instance.
--
-- /Note:/ Consider using 'fromDiskInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiFromDiskInfo :: Lens.Lens' InstanceSnapshotInfo (Lude.Maybe [DiskInfo])
isiFromDiskInfo = Lens.lens (fromDiskInfo :: InstanceSnapshotInfo -> Lude.Maybe [DiskInfo]) (\s a -> s {fromDiskInfo = a} :: InstanceSnapshotInfo)
{-# DEPRECATED isiFromDiskInfo "Use generic-lens or generic-optics with 'fromDiskInfo' instead." #-}

instance Lude.FromJSON InstanceSnapshotInfo where
  parseJSON =
    Lude.withObject
      "InstanceSnapshotInfo"
      ( \x ->
          InstanceSnapshotInfo'
            Lude.<$> (x Lude..:? "fromBlueprintId")
            Lude.<*> (x Lude..:? "fromBundleId")
            Lude.<*> (x Lude..:? "fromDiskInfo" Lude..!= Lude.mempty)
      )
