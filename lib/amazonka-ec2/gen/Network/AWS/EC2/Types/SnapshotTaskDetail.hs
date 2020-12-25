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
    stdDescription,
    stdDiskImageSize,
    stdEncrypted,
    stdFormat,
    stdKmsKeyId,
    stdProgress,
    stdSnapshotId,
    stdStatus,
    stdStatusMessage,
    stdUrl,
    stdUserBucket,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.UserBucketDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the import snapshot task.
--
-- /See:/ 'mkSnapshotTaskDetail' smart constructor.
data SnapshotTaskDetail = SnapshotTaskDetail'
  { -- | The description of the snapshot.
    description :: Core.Maybe Types.String,
    -- | The size of the disk in the snapshot, in GiB.
    diskImageSize :: Core.Maybe Core.Double,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | The format of the disk image from which the snapshot is created.
    format :: Core.Maybe Types.String,
    -- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted snapshot.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The percentage of completion for the import snapshot task.
    progress :: Core.Maybe Types.String,
    -- | The snapshot ID of the disk being imported.
    snapshotId :: Core.Maybe Types.String,
    -- | A brief status for the import snapshot task.
    status :: Core.Maybe Types.String,
    -- | A detailed status message for the import snapshot task.
    statusMessage :: Core.Maybe Types.String,
    -- | The URL of the disk image from which the snapshot is created.
    url :: Core.Maybe Types.String,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Core.Maybe Types.UserBucketDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotTaskDetail' value with any optional fields omitted.
mkSnapshotTaskDetail ::
  SnapshotTaskDetail
mkSnapshotTaskDetail =
  SnapshotTaskDetail'
    { description = Core.Nothing,
      diskImageSize = Core.Nothing,
      encrypted = Core.Nothing,
      format = Core.Nothing,
      kmsKeyId = Core.Nothing,
      progress = Core.Nothing,
      snapshotId = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      url = Core.Nothing,
      userBucket = Core.Nothing
    }

-- | The description of the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDescription :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdDescription = Lens.field @"description"
{-# DEPRECATED stdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The size of the disk in the snapshot, in GiB.
--
-- /Note:/ Consider using 'diskImageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDiskImageSize :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Double)
stdDiskImageSize = Lens.field @"diskImageSize"
{-# DEPRECATED stdDiskImageSize "Use generic-lens or generic-optics with 'diskImageSize' instead." #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdEncrypted :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Bool)
stdEncrypted = Lens.field @"encrypted"
{-# DEPRECATED stdEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The format of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdFormat :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdFormat = Lens.field @"format"
{-# DEPRECATED stdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdKmsKeyId :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED stdKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The percentage of completion for the import snapshot task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdProgress :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdProgress = Lens.field @"progress"
{-# DEPRECATED stdProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The snapshot ID of the disk being imported.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdSnapshotId :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED stdSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | A brief status for the import snapshot task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdStatus :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdStatus = Lens.field @"status"
{-# DEPRECATED stdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A detailed status message for the import snapshot task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdStatusMessage :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED stdStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The URL of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdUrl :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.String)
stdUrl = Lens.field @"url"
{-# DEPRECATED stdUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The Amazon S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdUserBucket :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.UserBucketDetails)
stdUserBucket = Lens.field @"userBucket"
{-# DEPRECATED stdUserBucket "Use generic-lens or generic-optics with 'userBucket' instead." #-}

instance Core.FromXML SnapshotTaskDetail where
  parseXML x =
    SnapshotTaskDetail'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "diskImageSize")
      Core.<*> (x Core..@? "encrypted")
      Core.<*> (x Core..@? "format")
      Core.<*> (x Core..@? "kmsKeyId")
      Core.<*> (x Core..@? "progress")
      Core.<*> (x Core..@? "snapshotId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "url")
      Core.<*> (x Core..@? "userBucket")
