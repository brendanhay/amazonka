{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotDiskContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotDiskContainer
  ( SnapshotDiskContainer (..),

    -- * Smart constructor
    mkSnapshotDiskContainer,

    -- * Lenses
    sdcFormat,
    sdcURL,
    sdcUserBucket,
    sdcDescription,
  )
where

import Network.AWS.EC2.Types.UserBucket
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The disk container object for the import snapshot request.
--
-- /See:/ 'mkSnapshotDiskContainer' smart constructor.
data SnapshotDiskContainer = SnapshotDiskContainer'
  { -- | The format of the disk image being imported.
    --
    -- Valid values: @VHD@ | @VMDK@
    format :: Lude.Maybe Lude.Text,
    -- | The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
    url :: Lude.Maybe Lude.Text,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Lude.Maybe UserBucket,
    -- | The description of the disk image being imported.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotDiskContainer' with the minimum fields required to make a request.
--
-- * 'format' - The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@
-- * 'url' - The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
-- * 'userBucket' - The Amazon S3 bucket for the disk image.
-- * 'description' - The description of the disk image being imported.
mkSnapshotDiskContainer ::
  SnapshotDiskContainer
mkSnapshotDiskContainer =
  SnapshotDiskContainer'
    { format = Lude.Nothing,
      url = Lude.Nothing,
      userBucket = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcFormat :: Lens.Lens' SnapshotDiskContainer (Lude.Maybe Lude.Text)
sdcFormat = Lens.lens (format :: SnapshotDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: SnapshotDiskContainer)
{-# DEPRECATED sdcFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcURL :: Lens.Lens' SnapshotDiskContainer (Lude.Maybe Lude.Text)
sdcURL = Lens.lens (url :: SnapshotDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: SnapshotDiskContainer)
{-# DEPRECATED sdcURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The Amazon S3 bucket for the disk image.
--
-- /Note:/ Consider using 'userBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcUserBucket :: Lens.Lens' SnapshotDiskContainer (Lude.Maybe UserBucket)
sdcUserBucket = Lens.lens (userBucket :: SnapshotDiskContainer -> Lude.Maybe UserBucket) (\s a -> s {userBucket = a} :: SnapshotDiskContainer)
{-# DEPRECATED sdcUserBucket "Use generic-lens or generic-optics with 'userBucket' instead." #-}

-- | The description of the disk image being imported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcDescription :: Lens.Lens' SnapshotDiskContainer (Lude.Maybe Lude.Text)
sdcDescription = Lens.lens (description :: SnapshotDiskContainer -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SnapshotDiskContainer)
{-# DEPRECATED sdcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToQuery SnapshotDiskContainer where
  toQuery SnapshotDiskContainer' {..} =
    Lude.mconcat
      [ "Format" Lude.=: format,
        "Url" Lude.=: url,
        "UserBucket" Lude.=: userBucket,
        "Description" Lude.=: description
      ]
