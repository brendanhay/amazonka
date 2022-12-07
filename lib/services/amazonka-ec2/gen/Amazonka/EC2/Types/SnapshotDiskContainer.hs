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
-- Module      : Amazonka.EC2.Types.SnapshotDiskContainer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SnapshotDiskContainer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.UserBucket
import qualified Amazonka.Prelude as Prelude

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
    -- | The URL to the Amazon S3-based disk image being imported. It can either
    -- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
    url :: Prelude.Maybe Prelude.Text,
    -- | The description of the disk image being imported.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'url', 'snapshotDiskContainer_url' - The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
--
-- 'description', 'snapshotDiskContainer_description' - The description of the disk image being imported.
newSnapshotDiskContainer ::
  SnapshotDiskContainer
newSnapshotDiskContainer =
  SnapshotDiskContainer'
    { format = Prelude.Nothing,
      userBucket = Prelude.Nothing,
      url = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ | @RAW@
snapshotDiskContainer_format :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_format = Lens.lens (\SnapshotDiskContainer' {format} -> format) (\s@SnapshotDiskContainer' {} a -> s {format = a} :: SnapshotDiskContainer)

-- | The Amazon S3 bucket for the disk image.
snapshotDiskContainer_userBucket :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe UserBucket)
snapshotDiskContainer_userBucket = Lens.lens (\SnapshotDiskContainer' {userBucket} -> userBucket) (\s@SnapshotDiskContainer' {} a -> s {userBucket = a} :: SnapshotDiskContainer)

-- | The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
snapshotDiskContainer_url :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_url = Lens.lens (\SnapshotDiskContainer' {url} -> url) (\s@SnapshotDiskContainer' {} a -> s {url = a} :: SnapshotDiskContainer)

-- | The description of the disk image being imported.
snapshotDiskContainer_description :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_description = Lens.lens (\SnapshotDiskContainer' {description} -> description) (\s@SnapshotDiskContainer' {} a -> s {description = a} :: SnapshotDiskContainer)

instance Prelude.Hashable SnapshotDiskContainer where
  hashWithSalt _salt SnapshotDiskContainer' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` userBucket
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` description

instance Prelude.NFData SnapshotDiskContainer where
  rnf SnapshotDiskContainer' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf userBucket
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf description

instance Data.ToQuery SnapshotDiskContainer where
  toQuery SnapshotDiskContainer' {..} =
    Prelude.mconcat
      [ "Format" Data.=: format,
        "UserBucket" Data.=: userBucket,
        "Url" Data.=: url,
        "Description" Data.=: description
      ]
