{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotTaskDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SnapshotTaskDetail
  ( SnapshotTaskDetail (..)
  -- * Smart constructor
  , mkSnapshotTaskDetail
  -- * Lenses
  , stdDescription
  , stdDiskImageSize
  , stdEncrypted
  , stdFormat
  , stdKmsKeyId
  , stdProgress
  , stdSnapshotId
  , stdStatus
  , stdStatusMessage
  , stdUrl
  , stdUserBucket
  ) where

import qualified Network.AWS.EC2.Types.UserBucketDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the import snapshot task.
--
-- /See:/ 'mkSnapshotTaskDetail' smart constructor.
data SnapshotTaskDetail = SnapshotTaskDetail'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the snapshot.
  , diskImageSize :: Core.Maybe Core.Double
    -- ^ The size of the disk in the snapshot, in GiB.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the snapshot is encrypted.
  , format :: Core.Maybe Core.Text
    -- ^ The format of the disk image from which the snapshot is created.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted snapshot.
  , progress :: Core.Maybe Core.Text
    -- ^ The percentage of completion for the import snapshot task.
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The snapshot ID of the disk being imported.
  , status :: Core.Maybe Core.Text
    -- ^ A brief status for the import snapshot task.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A detailed status message for the import snapshot task.
  , url :: Core.Maybe Core.Text
    -- ^ The URL of the disk image from which the snapshot is created.
  , userBucket :: Core.Maybe Types.UserBucketDetails
    -- ^ The Amazon S3 bucket for the disk image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotTaskDetail' value with any optional fields omitted.
mkSnapshotTaskDetail
    :: SnapshotTaskDetail
mkSnapshotTaskDetail
  = SnapshotTaskDetail'{description = Core.Nothing,
                        diskImageSize = Core.Nothing, encrypted = Core.Nothing,
                        format = Core.Nothing, kmsKeyId = Core.Nothing,
                        progress = Core.Nothing, snapshotId = Core.Nothing,
                        status = Core.Nothing, statusMessage = Core.Nothing,
                        url = Core.Nothing, userBucket = Core.Nothing}

-- | The description of the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDescription :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdDescription = Lens.field @"description"
{-# INLINEABLE stdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The size of the disk in the snapshot, in GiB.
--
-- /Note:/ Consider using 'diskImageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdDiskImageSize :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Double)
stdDiskImageSize = Lens.field @"diskImageSize"
{-# INLINEABLE stdDiskImageSize #-}
{-# DEPRECATED diskImageSize "Use generic-lens or generic-optics with 'diskImageSize' instead"  #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdEncrypted :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Bool)
stdEncrypted = Lens.field @"encrypted"
{-# INLINEABLE stdEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The format of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdFormat :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdFormat = Lens.field @"format"
{-# INLINEABLE stdFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdKmsKeyId :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE stdKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The percentage of completion for the import snapshot task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdProgress :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdProgress = Lens.field @"progress"
{-# INLINEABLE stdProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The snapshot ID of the disk being imported.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdSnapshotId :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE stdSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | A brief status for the import snapshot task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdStatus :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdStatus = Lens.field @"status"
{-# INLINEABLE stdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A detailed status message for the import snapshot task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdStatusMessage :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE stdStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The URL of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdUrl :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Core.Text)
stdUrl = Lens.field @"url"
{-# INLINEABLE stdUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The Amazon S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdUserBucket :: Lens.Lens' SnapshotTaskDetail (Core.Maybe Types.UserBucketDetails)
stdUserBucket = Lens.field @"userBucket"
{-# INLINEABLE stdUserBucket #-}
{-# DEPRECATED userBucket "Use generic-lens or generic-optics with 'userBucket' instead"  #-}

instance Core.FromXML SnapshotTaskDetail where
        parseXML x
          = SnapshotTaskDetail' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "diskImageSize"
                Core.<*> x Core..@? "encrypted"
                Core.<*> x Core..@? "format"
                Core.<*> x Core..@? "kmsKeyId"
                Core.<*> x Core..@? "progress"
                Core.<*> x Core..@? "snapshotId"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "statusMessage"
                Core.<*> x Core..@? "url"
                Core.<*> x Core..@? "userBucket"
