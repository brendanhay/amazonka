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
    idcFormat,
    idcURL,
    idcDeviceName,
    idcUserBucket,
    idcDescription,
    idcSnapshotId,
  )
where

import Network.AWS.EC2.Types.UserBucket
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the disk container object for an import image task.
--
-- /See:/ 'mkImageDiskContainer' smart constructor.
data ImageDiskContainer = ImageDiskContainer'
  { format ::
      Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    deviceName :: Lude.Maybe Lude.Text,
    userBucket :: Lude.Maybe UserBucket,
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

-- | Creates a value of 'ImageDiskContainer' with the minimum fields required to make a request.
--
-- * 'description' - The description of the disk image.
-- * 'deviceName' - The block device mapping for the disk.
-- * 'format' - The format of the disk image being imported.
--
-- Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@
-- * 'snapshotId' - The ID of the EBS snapshot to be used for importing the snapshot.
-- * 'url' - The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
-- * 'userBucket' - The S3 bucket for the disk image.
mkImageDiskContainer ::
  ImageDiskContainer
mkImageDiskContainer =
  ImageDiskContainer'
    { format = Lude.Nothing,
      url = Lude.Nothing,
      deviceName = Lude.Nothing,
      userBucket = Lude.Nothing,
      description = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | The format of the disk image being imported.
--
-- Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcFormat :: Lens.Lens' ImageDiskContainer (Lude.Maybe Lude.Text)
idcFormat = Lens.lens (format :: ImageDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: ImageDiskContainer)
{-# DEPRECATED idcFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcURL :: Lens.Lens' ImageDiskContainer (Lude.Maybe Lude.Text)
idcURL = Lens.lens (url :: ImageDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: ImageDiskContainer)
{-# DEPRECATED idcURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The block device mapping for the disk.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDeviceName :: Lens.Lens' ImageDiskContainer (Lude.Maybe Lude.Text)
idcDeviceName = Lens.lens (deviceName :: ImageDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: ImageDiskContainer)
{-# DEPRECATED idcDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcUserBucket :: Lens.Lens' ImageDiskContainer (Lude.Maybe UserBucket)
idcUserBucket = Lens.lens (userBucket :: ImageDiskContainer -> Lude.Maybe UserBucket) (\s a -> s {userBucket = a} :: ImageDiskContainer)
{-# DEPRECATED idcUserBucket "Use generic-lens or generic-optics with 'userBucket' instead." #-}

-- | The description of the disk image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDescription :: Lens.Lens' ImageDiskContainer (Lude.Maybe Lude.Text)
idcDescription = Lens.lens (description :: ImageDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImageDiskContainer)
{-# DEPRECATED idcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the EBS snapshot to be used for importing the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcSnapshotId :: Lens.Lens' ImageDiskContainer (Lude.Maybe Lude.Text)
idcSnapshotId = Lens.lens (snapshotId :: ImageDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: ImageDiskContainer)
{-# DEPRECATED idcSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.ToQuery ImageDiskContainer where
  toQuery ImageDiskContainer' {..} =
    Lude.mconcat
      [ "Format" Lude.=: format,
        "Url" Lude.=: url,
        "DeviceName" Lude.=: deviceName,
        "UserBucket" Lude.=: userBucket,
        "Description" Lude.=: description,
        "SnapshotId" Lude.=: snapshotId
      ]
