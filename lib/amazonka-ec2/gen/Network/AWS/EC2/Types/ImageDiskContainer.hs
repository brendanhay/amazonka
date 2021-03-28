{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageDiskContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImageDiskContainer
  ( ImageDiskContainer (..)
  -- * Smart constructor
  , mkImageDiskContainer
  -- * Lenses
  , idcDescription
  , idcDeviceName
  , idcFormat
  , idcSnapshotId
  , idcUrl
  , idcUserBucket
  ) where

import qualified Network.AWS.EC2.Types.SnapshotId as Types
import qualified Network.AWS.EC2.Types.UserBucket as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the disk container object for an import image task.
--
-- /See:/ 'mkImageDiskContainer' smart constructor.
data ImageDiskContainer = ImageDiskContainer'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the disk image.
  , deviceName :: Core.Maybe Core.Text
    -- ^ The block device mapping for the disk.
  , format :: Core.Maybe Core.Text
    -- ^ The format of the disk image being imported.
--
-- Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@ 
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The ID of the EBS snapshot to be used for importing the snapshot.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
  , userBucket :: Core.Maybe Types.UserBucket
    -- ^ The S3 bucket for the disk image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageDiskContainer' value with any optional fields omitted.
mkImageDiskContainer
    :: ImageDiskContainer
mkImageDiskContainer
  = ImageDiskContainer'{description = Core.Nothing,
                        deviceName = Core.Nothing, format = Core.Nothing,
                        snapshotId = Core.Nothing, url = Core.Nothing,
                        userBucket = Core.Nothing}

-- | The description of the disk image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDescription :: Lens.Lens' ImageDiskContainer (Core.Maybe Core.Text)
idcDescription = Lens.field @"description"
{-# INLINEABLE idcDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The block device mapping for the disk.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDeviceName :: Lens.Lens' ImageDiskContainer (Core.Maybe Core.Text)
idcDeviceName = Lens.field @"deviceName"
{-# INLINEABLE idcDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The format of the disk image being imported.
--
-- Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@ 
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcFormat :: Lens.Lens' ImageDiskContainer (Core.Maybe Core.Text)
idcFormat = Lens.field @"format"
{-# INLINEABLE idcFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The ID of the EBS snapshot to be used for importing the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcSnapshotId :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.SnapshotId)
idcSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE idcSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcUrl :: Lens.Lens' ImageDiskContainer (Core.Maybe Core.Text)
idcUrl = Lens.field @"url"
{-# INLINEABLE idcUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcUserBucket :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.UserBucket)
idcUserBucket = Lens.field @"userBucket"
{-# INLINEABLE idcUserBucket #-}
{-# DEPRECATED userBucket "Use generic-lens or generic-optics with 'userBucket' instead"  #-}

instance Core.ToQuery ImageDiskContainer where
        toQuery ImageDiskContainer{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Description")
              description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeviceName") deviceName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Format") format
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotId") snapshotId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Url") url
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserBucket") userBucket
