{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SnapshotDetail
  ( SnapshotDetail (..)
  -- * Smart constructor
  , mkSnapshotDetail
  -- * Lenses
  , sdDescription
  , sdDeviceName
  , sdDiskImageSize
  , sdFormat
  , sdProgress
  , sdSnapshotId
  , sdStatus
  , sdStatusMessage
  , sdUrl
  , sdUserBucket
  ) where

import qualified Network.AWS.EC2.Types.UserBucketDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the snapshot created from the imported disk.
--
-- /See:/ 'mkSnapshotDetail' smart constructor.
data SnapshotDetail = SnapshotDetail'
  { description :: Core.Maybe Core.Text
    -- ^ A description for the snapshot.
  , deviceName :: Core.Maybe Core.Text
    -- ^ The block device mapping for the snapshot.
  , diskImageSize :: Core.Maybe Core.Double
    -- ^ The size of the disk in the snapshot, in GiB.
  , format :: Core.Maybe Core.Text
    -- ^ The format of the disk image from which the snapshot is created.
  , progress :: Core.Maybe Core.Text
    -- ^ The percentage of progress for the task.
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The snapshot ID of the disk being imported.
  , status :: Core.Maybe Core.Text
    -- ^ A brief status of the snapshot creation.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A detailed status message for the snapshot creation.
  , url :: Core.Maybe Core.Text
    -- ^ The URL used to access the disk image.
  , userBucket :: Core.Maybe Types.UserBucketDetails
    -- ^ The Amazon S3 bucket for the disk image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotDetail' value with any optional fields omitted.
mkSnapshotDetail
    :: SnapshotDetail
mkSnapshotDetail
  = SnapshotDetail'{description = Core.Nothing,
                    deviceName = Core.Nothing, diskImageSize = Core.Nothing,
                    format = Core.Nothing, progress = Core.Nothing,
                    snapshotId = Core.Nothing, status = Core.Nothing,
                    statusMessage = Core.Nothing, url = Core.Nothing,
                    userBucket = Core.Nothing}

-- | A description for the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDescription :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdDescription = Lens.field @"description"
{-# INLINEABLE sdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The block device mapping for the snapshot.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeviceName :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdDeviceName = Lens.field @"deviceName"
{-# INLINEABLE sdDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The size of the disk in the snapshot, in GiB.
--
-- /Note:/ Consider using 'diskImageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDiskImageSize :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Double)
sdDiskImageSize = Lens.field @"diskImageSize"
{-# INLINEABLE sdDiskImageSize #-}
{-# DEPRECATED diskImageSize "Use generic-lens or generic-optics with 'diskImageSize' instead"  #-}

-- | The format of the disk image from which the snapshot is created.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdFormat :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdFormat = Lens.field @"format"
{-# INLINEABLE sdFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The percentage of progress for the task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdProgress :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdProgress = Lens.field @"progress"
{-# INLINEABLE sdProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The snapshot ID of the disk being imported.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSnapshotId :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE sdSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | A brief status of the snapshot creation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStatus :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdStatus = Lens.field @"status"
{-# INLINEABLE sdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A detailed status message for the snapshot creation.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStatusMessage :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE sdStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The URL used to access the disk image.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdUrl :: Lens.Lens' SnapshotDetail (Core.Maybe Core.Text)
sdUrl = Lens.field @"url"
{-# INLINEABLE sdUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The Amazon S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdUserBucket :: Lens.Lens' SnapshotDetail (Core.Maybe Types.UserBucketDetails)
sdUserBucket = Lens.field @"userBucket"
{-# INLINEABLE sdUserBucket #-}
{-# DEPRECATED userBucket "Use generic-lens or generic-optics with 'userBucket' instead"  #-}

instance Core.FromXML SnapshotDetail where
        parseXML x
          = SnapshotDetail' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "deviceName"
                Core.<*> x Core..@? "diskImageSize"
                Core.<*> x Core..@? "format"
                Core.<*> x Core..@? "progress"
                Core.<*> x Core..@? "snapshotId"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "statusMessage"
                Core.<*> x Core..@? "url"
                Core.<*> x Core..@? "userBucket"
