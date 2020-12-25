{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageDiskContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageDiskContainer
  ( ImageDiskContainer (..),

    -- * Smart constructor
    mkImageDiskContainer,

    -- * Lenses
    idcDescription,
    idcDeviceName,
    idcFormat,
    idcSnapshotId,
    idcUrl,
    idcUserBucket,
  )
where

import qualified Network.AWS.EC2.Types.SnapshotId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.UserBucket as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the disk container object for an import image task.
--
-- /See:/ 'mkImageDiskContainer' smart constructor.
data ImageDiskContainer = ImageDiskContainer'
  { -- | The description of the disk image.
    description :: Core.Maybe Types.String,
    -- | The block device mapping for the disk.
    deviceName :: Core.Maybe Types.String,
    -- | The format of the disk image being imported.
    --
    -- Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@
    format :: Core.Maybe Types.String,
    -- | The ID of the EBS snapshot to be used for importing the snapshot.
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
    url :: Core.Maybe Types.String,
    -- | The S3 bucket for the disk image.
    userBucket :: Core.Maybe Types.UserBucket
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageDiskContainer' value with any optional fields omitted.
mkImageDiskContainer ::
  ImageDiskContainer
mkImageDiskContainer =
  ImageDiskContainer'
    { description = Core.Nothing,
      deviceName = Core.Nothing,
      format = Core.Nothing,
      snapshotId = Core.Nothing,
      url = Core.Nothing,
      userBucket = Core.Nothing
    }

-- | The description of the disk image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDescription :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.String)
idcDescription = Lens.field @"description"
{-# DEPRECATED idcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The block device mapping for the disk.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDeviceName :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.String)
idcDeviceName = Lens.field @"deviceName"
{-# DEPRECATED idcDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The format of the disk image being imported.
--
-- Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcFormat :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.String)
idcFormat = Lens.field @"format"
{-# DEPRECATED idcFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The ID of the EBS snapshot to be used for importing the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcSnapshotId :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.SnapshotId)
idcSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED idcSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcUrl :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.String)
idcUrl = Lens.field @"url"
{-# DEPRECATED idcUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcUserBucket :: Lens.Lens' ImageDiskContainer (Core.Maybe Types.UserBucket)
idcUserBucket = Lens.field @"userBucket"
{-# DEPRECATED idcUserBucket "Use generic-lens or generic-optics with 'userBucket' instead." #-}
