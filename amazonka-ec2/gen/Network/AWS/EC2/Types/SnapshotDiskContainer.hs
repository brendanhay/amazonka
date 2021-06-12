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
-- Module      : Network.AWS.EC2.Types.SnapshotDiskContainer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotDiskContainer where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserBucket
import qualified Network.AWS.Lens as Lens

-- | The disk container object for the import snapshot request.
--
-- /See:/ 'newSnapshotDiskContainer' smart constructor.
data SnapshotDiskContainer = SnapshotDiskContainer'
  { -- | The format of the disk image being imported.
    --
    -- Valid values: @VHD@ | @VMDK@ | @RAW@
    format :: Core.Maybe Core.Text,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Core.Maybe UserBucket,
    -- | The description of the disk image being imported.
    description :: Core.Maybe Core.Text,
    -- | The URL to the Amazon S3-based disk image being imported. It can either
    -- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
    url :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SnapshotDiskContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'snapshotDiskContainer_format' - The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ | @RAW@
--
-- 'userBucket', 'snapshotDiskContainer_userBucket' - The Amazon S3 bucket for the disk image.
--
-- 'description', 'snapshotDiskContainer_description' - The description of the disk image being imported.
--
-- 'url', 'snapshotDiskContainer_url' - The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
newSnapshotDiskContainer ::
  SnapshotDiskContainer
newSnapshotDiskContainer =
  SnapshotDiskContainer'
    { format = Core.Nothing,
      userBucket = Core.Nothing,
      description = Core.Nothing,
      url = Core.Nothing
    }

-- | The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ | @RAW@
snapshotDiskContainer_format :: Lens.Lens' SnapshotDiskContainer (Core.Maybe Core.Text)
snapshotDiskContainer_format = Lens.lens (\SnapshotDiskContainer' {format} -> format) (\s@SnapshotDiskContainer' {} a -> s {format = a} :: SnapshotDiskContainer)

-- | The Amazon S3 bucket for the disk image.
snapshotDiskContainer_userBucket :: Lens.Lens' SnapshotDiskContainer (Core.Maybe UserBucket)
snapshotDiskContainer_userBucket = Lens.lens (\SnapshotDiskContainer' {userBucket} -> userBucket) (\s@SnapshotDiskContainer' {} a -> s {userBucket = a} :: SnapshotDiskContainer)

-- | The description of the disk image being imported.
snapshotDiskContainer_description :: Lens.Lens' SnapshotDiskContainer (Core.Maybe Core.Text)
snapshotDiskContainer_description = Lens.lens (\SnapshotDiskContainer' {description} -> description) (\s@SnapshotDiskContainer' {} a -> s {description = a} :: SnapshotDiskContainer)

-- | The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
snapshotDiskContainer_url :: Lens.Lens' SnapshotDiskContainer (Core.Maybe Core.Text)
snapshotDiskContainer_url = Lens.lens (\SnapshotDiskContainer' {url} -> url) (\s@SnapshotDiskContainer' {} a -> s {url = a} :: SnapshotDiskContainer)

instance Core.Hashable SnapshotDiskContainer

instance Core.NFData SnapshotDiskContainer

instance Core.ToQuery SnapshotDiskContainer where
  toQuery SnapshotDiskContainer' {..} =
    Core.mconcat
      [ "Format" Core.=: format,
        "UserBucket" Core.=: userBucket,
        "Description" Core.=: description,
        "Url" Core.=: url
      ]
