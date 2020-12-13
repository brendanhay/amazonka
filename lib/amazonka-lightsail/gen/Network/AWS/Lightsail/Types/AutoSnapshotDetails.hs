{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotDetails
  ( AutoSnapshotDetails (..),

    -- * Smart constructor
    mkAutoSnapshotDetails,

    -- * Lenses
    asdStatus,
    asdFromAttachedDisks,
    asdCreatedAt,
    asdDate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotStatus
import qualified Network.AWS.Prelude as Lude

-- | Describes an automatic snapshot.
--
-- /See:/ 'mkAutoSnapshotDetails' smart constructor.
data AutoSnapshotDetails = AutoSnapshotDetails'
  { -- | The status of the automatic snapshot.
    status :: Lude.Maybe AutoSnapshotStatus,
    -- | An array of objects that describe the block storage disks attached to the instance when the automatic snapshot was created.
    fromAttachedDisks :: Lude.Maybe [AttachedDisk],
    -- | The timestamp when the automatic snapshot was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
    date :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoSnapshotDetails' with the minimum fields required to make a request.
--
-- * 'status' - The status of the automatic snapshot.
-- * 'fromAttachedDisks' - An array of objects that describe the block storage disks attached to the instance when the automatic snapshot was created.
-- * 'createdAt' - The timestamp when the automatic snapshot was created.
-- * 'date' - The date of the automatic snapshot in @YYYY-MM-DD@ format.
mkAutoSnapshotDetails ::
  AutoSnapshotDetails
mkAutoSnapshotDetails =
  AutoSnapshotDetails'
    { status = Lude.Nothing,
      fromAttachedDisks = Lude.Nothing,
      createdAt = Lude.Nothing,
      date = Lude.Nothing
    }

-- | The status of the automatic snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdStatus :: Lens.Lens' AutoSnapshotDetails (Lude.Maybe AutoSnapshotStatus)
asdStatus = Lens.lens (status :: AutoSnapshotDetails -> Lude.Maybe AutoSnapshotStatus) (\s a -> s {status = a} :: AutoSnapshotDetails)
{-# DEPRECATED asdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An array of objects that describe the block storage disks attached to the instance when the automatic snapshot was created.
--
-- /Note:/ Consider using 'fromAttachedDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdFromAttachedDisks :: Lens.Lens' AutoSnapshotDetails (Lude.Maybe [AttachedDisk])
asdFromAttachedDisks = Lens.lens (fromAttachedDisks :: AutoSnapshotDetails -> Lude.Maybe [AttachedDisk]) (\s a -> s {fromAttachedDisks = a} :: AutoSnapshotDetails)
{-# DEPRECATED asdFromAttachedDisks "Use generic-lens or generic-optics with 'fromAttachedDisks' instead." #-}

-- | The timestamp when the automatic snapshot was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdCreatedAt :: Lens.Lens' AutoSnapshotDetails (Lude.Maybe Lude.Timestamp)
asdCreatedAt = Lens.lens (createdAt :: AutoSnapshotDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: AutoSnapshotDetails)
{-# DEPRECATED asdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdDate :: Lens.Lens' AutoSnapshotDetails (Lude.Maybe Lude.Text)
asdDate = Lens.lens (date :: AutoSnapshotDetails -> Lude.Maybe Lude.Text) (\s a -> s {date = a} :: AutoSnapshotDetails)
{-# DEPRECATED asdDate "Use generic-lens or generic-optics with 'date' instead." #-}

instance Lude.FromJSON AutoSnapshotDetails where
  parseJSON =
    Lude.withObject
      "AutoSnapshotDetails"
      ( \x ->
          AutoSnapshotDetails'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "fromAttachedDisks" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "date")
      )
