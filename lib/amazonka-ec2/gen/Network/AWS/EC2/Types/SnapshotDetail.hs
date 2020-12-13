{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotDetail
  ( SnapshotDetail (..),

    -- * Smart constructor
    mkSnapshotDetail,

    -- * Lenses
    sdStatus,
    sdProgress,
    sdFormat,
    sdURL,
    sdDeviceName,
    sdStatusMessage,
    sdUserBucket,
    sdDiskImageSize,
    sdDescription,
    sdSnapshotId,
  )
where

import Network.AWS.EC2.Types.UserBucketDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the snapshot created from the imported disk.
--
-- /See:/ 'mkSnapshotDetail' smart constructor.
data SnapshotDetail = SnapshotDetail'
  { -- | A brief status of the snapshot creation.
    status :: Lude.Maybe Lude.Text,
    -- | The percentage of progress for the task.
    progress :: Lude.Maybe Lude.Text,
    -- | The format of the disk image from which the snapshot is created.
    format :: Lude.Maybe Lude.Text,
    -- | The URL used to access the disk image.
    url :: Lude.Maybe Lude.Text,
    -- | The block device mapping for the snapshot.
    deviceName :: Lude.Maybe Lude.Text,
    -- | A detailed status message for the snapshot creation.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Lude.Maybe UserBucketDetails,
    -- | The size of the disk in the snapshot, in GiB.
    diskImageSize :: Lude.Maybe Lude.Double,
    -- | A description for the snapshot.
    description :: Lude.Maybe Lude.Text,
    -- | The snapshot ID of the disk being imported.
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotDetail' with the minimum fields required to make a request.
--
-- * 'status' - A brief status of the snapshot creation.
-- * 'progress' - The percentage of progress for the task.
-- * 'format' - The format of the disk image from which the snapshot is created.
-- * 'url' - The URL used to access the disk image.
-- * 'deviceName' - The block device mapping for the snapshot.
-- * 'statusMessage' - A detailed status message for the snapshot creation.
-- * 'userBucket' - The Amazon S3 bucket for the disk image.
-- * 'diskImageSize' - The size of the disk in the snapshot, in GiB.
-- * 'description' - A description for the snapshot.
-- * 'snapshotId' - The snapshot ID of the disk being imported.
mkSnapshotDetail ::
  SnapshotDetail
mkSnapshotDetail =
  SnapshotDetail'
    { status = Lude.Nothing,
      progress = Lude.Nothing,
      format = Lude.Nothing,
      url = Lude.Nothing,
      deviceName = Lude.Nothing,
      statusMessage = Lude.Nothing,
      userBucket = Lude.Nothing,
      diskImageSize = Lude.Nothing,
      description = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | A brief status of the snapshot creation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStatus :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdStatus = Lens.lens (status :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: SnapshotDetail)
{-# DEPRECATED sdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The percentage of progress for the task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdProgress :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdProgress = Lens.lens (progress :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: SnapshotDetail)
{-# DEPRECATED sdProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The format of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdFormat :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdFormat = Lens.lens (format :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: SnapshotDetail)
{-# DEPRECATED sdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URL used to access the disk image.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdURL :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdURL = Lens.lens (url :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: SnapshotDetail)
{-# DEPRECATED sdURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The block device mapping for the snapshot.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeviceName :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdDeviceName = Lens.lens (deviceName :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: SnapshotDetail)
{-# DEPRECATED sdDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | A detailed status message for the snapshot creation.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStatusMessage :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdStatusMessage = Lens.lens (statusMessage :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: SnapshotDetail)
{-# DEPRECATED sdStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The Amazon S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdUserBucket :: Lens.Lens' SnapshotDetail (Lude.Maybe UserBucketDetails)
sdUserBucket = Lens.lens (userBucket :: SnapshotDetail -> Lude.Maybe UserBucketDetails) (\s a -> s {userBucket = a} :: SnapshotDetail)
{-# DEPRECATED sdUserBucket "Use generic-lens or generic-optics with 'userBucket' instead." #-}

-- | The size of the disk in the snapshot, in GiB.
--
-- /Note:/ Consider using 'diskImageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDiskImageSize :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Double)
sdDiskImageSize = Lens.lens (diskImageSize :: SnapshotDetail -> Lude.Maybe Lude.Double) (\s a -> s {diskImageSize = a} :: SnapshotDetail)
{-# DEPRECATED sdDiskImageSize "Use generic-lens or generic-optics with 'diskImageSize' instead." #-}

-- | A description for the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDescription :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdDescription = Lens.lens (description :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SnapshotDetail)
{-# DEPRECATED sdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The snapshot ID of the disk being imported.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSnapshotId :: Lens.Lens' SnapshotDetail (Lude.Maybe Lude.Text)
sdSnapshotId = Lens.lens (snapshotId :: SnapshotDetail -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: SnapshotDetail)
{-# DEPRECATED sdSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML SnapshotDetail where
  parseXML x =
    SnapshotDetail'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "progress")
      Lude.<*> (x Lude..@? "format")
      Lude.<*> (x Lude..@? "url")
      Lude.<*> (x Lude..@? "deviceName")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "userBucket")
      Lude.<*> (x Lude..@? "diskImageSize")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "snapshotId")
