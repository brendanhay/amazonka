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
-- Module      : Network.AWS.EC2.Types.SnapshotDiskContainer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotDiskContainer where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserBucket
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The disk container object for the import snapshot request.
--
-- /See:/ 'newSnapshotDiskContainer' smart constructor.
data SnapshotDiskContainer = SnapshotDiskContainer'
  { -- | The format of the disk image being imported.
    --
    -- Valid values: @VHD@ | @VMDK@ | @RAW@
    format :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Prelude.Maybe UserBucket,
    -- | The description of the disk image being imported.
    description :: Prelude.Maybe Prelude.Text,
    -- | The URL to the Amazon S3-based disk image being imported. It can either
    -- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { format = Prelude.Nothing,
      userBucket = Prelude.Nothing,
      description = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ | @RAW@
snapshotDiskContainer_format :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_format = Lens.lens (\SnapshotDiskContainer' {format} -> format) (\s@SnapshotDiskContainer' {} a -> s {format = a} :: SnapshotDiskContainer)

-- | The Amazon S3 bucket for the disk image.
snapshotDiskContainer_userBucket :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe UserBucket)
snapshotDiskContainer_userBucket = Lens.lens (\SnapshotDiskContainer' {userBucket} -> userBucket) (\s@SnapshotDiskContainer' {} a -> s {userBucket = a} :: SnapshotDiskContainer)

-- | The description of the disk image being imported.
snapshotDiskContainer_description :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_description = Lens.lens (\SnapshotDiskContainer' {description} -> description) (\s@SnapshotDiskContainer' {} a -> s {description = a} :: SnapshotDiskContainer)

-- | The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
snapshotDiskContainer_url :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_url = Lens.lens (\SnapshotDiskContainer' {url} -> url) (\s@SnapshotDiskContainer' {} a -> s {url = a} :: SnapshotDiskContainer)

instance Prelude.Hashable SnapshotDiskContainer

instance Prelude.NFData SnapshotDiskContainer

instance Prelude.ToQuery SnapshotDiskContainer where
  toQuery SnapshotDiskContainer' {..} =
    Prelude.mconcat
      [ "Format" Prelude.=: format,
        "UserBucket" Prelude.=: userBucket,
        "Description" Prelude.=: description,
        "Url" Prelude.=: url
      ]
