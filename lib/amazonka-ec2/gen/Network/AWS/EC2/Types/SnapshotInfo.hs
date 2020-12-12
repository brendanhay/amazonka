{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotInfo
  ( SnapshotInfo (..),

    -- * Smart constructor
    mkSnapshotInfo,

    -- * Lenses
    siState,
    siProgress,
    siStartTime,
    siVolumeSize,
    siEncrypted,
    siOwnerId,
    siVolumeId,
    siDescription,
    siTags,
    siSnapshotId,
  )
where

import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a snapshot.
--
-- /See:/ 'mkSnapshotInfo' smart constructor.
data SnapshotInfo = SnapshotInfo'
  { state ::
      Lude.Maybe SnapshotState,
    progress :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.DateTime,
    volumeSize :: Lude.Maybe Lude.Int,
    encrypted :: Lude.Maybe Lude.Bool,
    ownerId :: Lude.Maybe Lude.Text,
    volumeId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotInfo' with the minimum fields required to make a request.
--
-- * 'description' - Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
-- * 'encrypted' - Indicates whether the snapshot is encrypted.
-- * 'ownerId' - Account id used when creating this snapshot.
-- * 'progress' - Progress this snapshot has made towards completing.
-- * 'snapshotId' - Snapshot id that can be used to describe this snapshot.
-- * 'startTime' - Time this snapshot was started. This is the same for all snapshots initiated by the same request.
-- * 'state' - Current state of the snapshot.
-- * 'tags' - Tags associated with this snapshot.
-- * 'volumeId' - Source volume from which this snapshot was created.
-- * 'volumeSize' - Size of the volume from which this snapshot was created.
mkSnapshotInfo ::
  SnapshotInfo
mkSnapshotInfo =
  SnapshotInfo'
    { state = Lude.Nothing,
      progress = Lude.Nothing,
      startTime = Lude.Nothing,
      volumeSize = Lude.Nothing,
      encrypted = Lude.Nothing,
      ownerId = Lude.Nothing,
      volumeId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | Current state of the snapshot.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siState :: Lens.Lens' SnapshotInfo (Lude.Maybe SnapshotState)
siState = Lens.lens (state :: SnapshotInfo -> Lude.Maybe SnapshotState) (\s a -> s {state = a} :: SnapshotInfo)
{-# DEPRECATED siState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Progress this snapshot has made towards completing.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siProgress :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.Text)
siProgress = Lens.lens (progress :: SnapshotInfo -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: SnapshotInfo)
{-# DEPRECATED siProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | Time this snapshot was started. This is the same for all snapshots initiated by the same request.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStartTime :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.DateTime)
siStartTime = Lens.lens (startTime :: SnapshotInfo -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: SnapshotInfo)
{-# DEPRECATED siStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Size of the volume from which this snapshot was created.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVolumeSize :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.Int)
siVolumeSize = Lens.lens (volumeSize :: SnapshotInfo -> Lude.Maybe Lude.Int) (\s a -> s {volumeSize = a} :: SnapshotInfo)
{-# DEPRECATED siVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siEncrypted :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.Bool)
siEncrypted = Lens.lens (encrypted :: SnapshotInfo -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: SnapshotInfo)
{-# DEPRECATED siEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | Account id used when creating this snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siOwnerId :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.Text)
siOwnerId = Lens.lens (ownerId :: SnapshotInfo -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: SnapshotInfo)
{-# DEPRECATED siOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Source volume from which this snapshot was created.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siVolumeId :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.Text)
siVolumeId = Lens.lens (volumeId :: SnapshotInfo -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: SnapshotInfo)
{-# DEPRECATED siVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDescription :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.Text)
siDescription = Lens.lens (description :: SnapshotInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SnapshotInfo)
{-# DEPRECATED siDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Tags associated with this snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTags :: Lens.Lens' SnapshotInfo (Lude.Maybe [Tag])
siTags = Lens.lens (tags :: SnapshotInfo -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SnapshotInfo)
{-# DEPRECATED siTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Snapshot id that can be used to describe this snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSnapshotId :: Lens.Lens' SnapshotInfo (Lude.Maybe Lude.Text)
siSnapshotId = Lens.lens (snapshotId :: SnapshotInfo -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: SnapshotInfo)
{-# DEPRECATED siSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML SnapshotInfo where
  parseXML x =
    SnapshotInfo'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "progress")
      Lude.<*> (x Lude..@? "startTime")
      Lude.<*> (x Lude..@? "volumeSize")
      Lude.<*> (x Lude..@? "encrypted")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "volumeId")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "snapshotId")
