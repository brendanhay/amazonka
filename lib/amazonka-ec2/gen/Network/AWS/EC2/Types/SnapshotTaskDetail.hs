{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotTaskDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotTaskDetail
  ( SnapshotTaskDetail (..),

    -- * Smart constructor
    mkSnapshotTaskDetail,

    -- * Lenses
    stdStatus,
    stdProgress,
    stdFormat,
    stdURL,
    stdEncrypted,
    stdKMSKeyId,
    stdStatusMessage,
    stdUserBucket,
    stdDiskImageSize,
    stdDescription,
    stdSnapshotId,
  )
where

import Network.AWS.EC2.Types.UserBucketDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the import snapshot task.
--
-- /See:/ 'mkSnapshotTaskDetail' smart constructor.
data SnapshotTaskDetail = SnapshotTaskDetail'
  { status ::
      Lude.Maybe Lude.Text,
    progress :: Lude.Maybe Lude.Text,
    format :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    kmsKeyId :: Lude.Maybe Lude.Text,
    statusMessage :: Lude.Maybe Lude.Text,
    userBucket :: Lude.Maybe UserBucketDetails,
    diskImageSize :: Lude.Maybe Lude.Double,
    description :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SnapshotTaskDetail' with the minimum fields required to make a request.
--
-- * 'description' - The description of the snapshot.
-- * 'diskImageSize' - The size of the disk in the snapshot, in GiB.
-- * 'encrypted' - Indicates whether the snapshot is encrypted.
-- * 'format' - The format of the disk image from which the snapshot is created.
-- * 'kmsKeyId' - The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted snapshot.
-- * 'progress' - The percentage of completion for the import snapshot task.
-- * 'snapshotId' - The snapshot ID of the disk being imported.
-- * 'status' - A brief status for the import snapshot task.
-- * 'statusMessage' - A detailed status message for the import snapshot task.
-- * 'url' - The URL of the disk image from which the snapshot is created.
-- * 'userBucket' - The Amazon S3 bucket for the disk image.
mkSnapshotTaskDetail ::
  SnapshotTaskDetail
mkSnapshotTaskDetail =
  SnapshotTaskDetail'
    { status = Lude.Nothing,
      progress = Lude.Nothing,
      format = Lude.Nothing,
      url = Lude.Nothing,
      encrypted = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      statusMessage = Lude.Nothing,
      userBucket = Lude.Nothing,
      diskImageSize = Lude.Nothing,
      description = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | A brief status for the import snapshot task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdStatus :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdStatus = Lens.lens (status :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The percentage of completion for the import snapshot task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdProgress :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdProgress = Lens.lens (progress :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The format of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdFormat :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdFormat = Lens.lens (format :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URL of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdURL :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdURL = Lens.lens (url :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdEncrypted :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Bool)
stdEncrypted = Lens.lens (encrypted :: SnapshotTaskDetail -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdKMSKeyId :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdKMSKeyId = Lens.lens (kmsKeyId :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A detailed status message for the import snapshot task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdStatusMessage :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdStatusMessage = Lens.lens (statusMessage :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The Amazon S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdUserBucket :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe UserBucketDetails)
stdUserBucket = Lens.lens (userBucket :: SnapshotTaskDetail -> Lude.Maybe UserBucketDetails) (\s a -> s {userBucket = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdUserBucket "Use generic-lens or generic-optics with 'userBucket' instead." #-}

-- | The size of the disk in the snapshot, in GiB.
--
-- /Note:/ Consider using 'diskImageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDiskImageSize :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Double)
stdDiskImageSize = Lens.lens (diskImageSize :: SnapshotTaskDetail -> Lude.Maybe Lude.Double) (\s a -> s {diskImageSize = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdDiskImageSize "Use generic-lens or generic-optics with 'diskImageSize' instead." #-}

-- | The description of the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDescription :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdDescription = Lens.lens (description :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The snapshot ID of the disk being imported.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdSnapshotId :: Lens.Lens' SnapshotTaskDetail (Lude.Maybe Lude.Text)
stdSnapshotId = Lens.lens (snapshotId :: SnapshotTaskDetail -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: SnapshotTaskDetail)
{-# DEPRECATED stdSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML SnapshotTaskDetail where
  parseXML x =
    SnapshotTaskDetail'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "progress")
      Lude.<*> (x Lude..@? "format")
      Lude.<*> (x Lude..@? "url")
      Lude.<*> (x Lude..@? "encrypted")
      Lude.<*> (x Lude..@? "kmsKeyId")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "userBucket")
      Lude.<*> (x Lude..@? "diskImageSize")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "snapshotId")
