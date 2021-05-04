{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageDiskContainer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageDiskContainer where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserBucket
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the disk container object for an import image task.
--
-- /See:/ 'newImageDiskContainer' smart constructor.
data ImageDiskContainer = ImageDiskContainer'
  { -- | The format of the disk image being imported.
    --
    -- Valid values: @OVA@ | @VHD@ | @VHDX@ | @VMDK@ | @RAW@
    format :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket for the disk image.
    userBucket :: Prelude.Maybe UserBucket,
    -- | The block device mapping for the disk.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EBS snapshot to be used for importing the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The description of the disk image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The URL to the Amazon S3-based disk image being imported. The URL can
    -- either be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..)
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImageDiskContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'imageDiskContainer_format' - The format of the disk image being imported.
--
-- Valid values: @OVA@ | @VHD@ | @VHDX@ | @VMDK@ | @RAW@
--
-- 'userBucket', 'imageDiskContainer_userBucket' - The S3 bucket for the disk image.
--
-- 'deviceName', 'imageDiskContainer_deviceName' - The block device mapping for the disk.
--
-- 'snapshotId', 'imageDiskContainer_snapshotId' - The ID of the EBS snapshot to be used for importing the snapshot.
--
-- 'description', 'imageDiskContainer_description' - The description of the disk image.
--
-- 'url', 'imageDiskContainer_url' - The URL to the Amazon S3-based disk image being imported. The URL can
-- either be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..)
newImageDiskContainer ::
  ImageDiskContainer
newImageDiskContainer =
  ImageDiskContainer'
    { format = Prelude.Nothing,
      userBucket = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      description = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The format of the disk image being imported.
--
-- Valid values: @OVA@ | @VHD@ | @VHDX@ | @VMDK@ | @RAW@
imageDiskContainer_format :: Lens.Lens' ImageDiskContainer (Prelude.Maybe Prelude.Text)
imageDiskContainer_format = Lens.lens (\ImageDiskContainer' {format} -> format) (\s@ImageDiskContainer' {} a -> s {format = a} :: ImageDiskContainer)

-- | The S3 bucket for the disk image.
imageDiskContainer_userBucket :: Lens.Lens' ImageDiskContainer (Prelude.Maybe UserBucket)
imageDiskContainer_userBucket = Lens.lens (\ImageDiskContainer' {userBucket} -> userBucket) (\s@ImageDiskContainer' {} a -> s {userBucket = a} :: ImageDiskContainer)

-- | The block device mapping for the disk.
imageDiskContainer_deviceName :: Lens.Lens' ImageDiskContainer (Prelude.Maybe Prelude.Text)
imageDiskContainer_deviceName = Lens.lens (\ImageDiskContainer' {deviceName} -> deviceName) (\s@ImageDiskContainer' {} a -> s {deviceName = a} :: ImageDiskContainer)

-- | The ID of the EBS snapshot to be used for importing the snapshot.
imageDiskContainer_snapshotId :: Lens.Lens' ImageDiskContainer (Prelude.Maybe Prelude.Text)
imageDiskContainer_snapshotId = Lens.lens (\ImageDiskContainer' {snapshotId} -> snapshotId) (\s@ImageDiskContainer' {} a -> s {snapshotId = a} :: ImageDiskContainer)

-- | The description of the disk image.
imageDiskContainer_description :: Lens.Lens' ImageDiskContainer (Prelude.Maybe Prelude.Text)
imageDiskContainer_description = Lens.lens (\ImageDiskContainer' {description} -> description) (\s@ImageDiskContainer' {} a -> s {description = a} :: ImageDiskContainer)

-- | The URL to the Amazon S3-based disk image being imported. The URL can
-- either be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..)
imageDiskContainer_url :: Lens.Lens' ImageDiskContainer (Prelude.Maybe Prelude.Text)
imageDiskContainer_url = Lens.lens (\ImageDiskContainer' {url} -> url) (\s@ImageDiskContainer' {} a -> s {url = a} :: ImageDiskContainer)

instance Prelude.Hashable ImageDiskContainer

instance Prelude.NFData ImageDiskContainer

instance Prelude.ToQuery ImageDiskContainer where
  toQuery ImageDiskContainer' {..} =
    Prelude.mconcat
      [ "Format" Prelude.=: format,
        "UserBucket" Prelude.=: userBucket,
        "DeviceName" Prelude.=: deviceName,
        "SnapshotId" Prelude.=: snapshotId,
        "Description" Prelude.=: description,
        "Url" Prelude.=: url
      ]
