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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The description of the disk image being imported.
    description :: Prelude.Maybe Prelude.Text,
    -- | The format of the disk image being imported.
    --
    -- Valid values: @VHD@ | @VMDK@ | @RAW@
    format :: Prelude.Maybe Prelude.Text,
    -- | The URL to the Amazon S3-based disk image being imported. It can either
    -- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
    url :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Prelude.Maybe UserBucket
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotDiskContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'snapshotDiskContainer_description' - The description of the disk image being imported.
--
-- 'format', 'snapshotDiskContainer_format' - The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ | @RAW@
--
-- 'url', 'snapshotDiskContainer_url' - The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
--
-- 'userBucket', 'snapshotDiskContainer_userBucket' - The Amazon S3 bucket for the disk image.
newSnapshotDiskContainer ::
  SnapshotDiskContainer
newSnapshotDiskContainer =
  SnapshotDiskContainer'
    { description =
        Prelude.Nothing,
      format = Prelude.Nothing,
      url = Prelude.Nothing,
      userBucket = Prelude.Nothing
    }

-- | The description of the disk image being imported.
snapshotDiskContainer_description :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_description = Lens.lens (\SnapshotDiskContainer' {description} -> description) (\s@SnapshotDiskContainer' {} a -> s {description = a} :: SnapshotDiskContainer)

-- | The format of the disk image being imported.
--
-- Valid values: @VHD@ | @VMDK@ | @RAW@
snapshotDiskContainer_format :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_format = Lens.lens (\SnapshotDiskContainer' {format} -> format) (\s@SnapshotDiskContainer' {} a -> s {format = a} :: SnapshotDiskContainer)

-- | The URL to the Amazon S3-based disk image being imported. It can either
-- be a https URL (https:\/\/..) or an Amazon S3 URL (s3:\/\/..).
snapshotDiskContainer_url :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe Prelude.Text)
snapshotDiskContainer_url = Lens.lens (\SnapshotDiskContainer' {url} -> url) (\s@SnapshotDiskContainer' {} a -> s {url = a} :: SnapshotDiskContainer) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon S3 bucket for the disk image.
snapshotDiskContainer_userBucket :: Lens.Lens' SnapshotDiskContainer (Prelude.Maybe UserBucket)
snapshotDiskContainer_userBucket = Lens.lens (\SnapshotDiskContainer' {userBucket} -> userBucket) (\s@SnapshotDiskContainer' {} a -> s {userBucket = a} :: SnapshotDiskContainer)

instance Prelude.Hashable SnapshotDiskContainer where
  hashWithSalt _salt SnapshotDiskContainer' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` userBucket

instance Prelude.NFData SnapshotDiskContainer where
  rnf SnapshotDiskContainer' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf userBucket

instance Data.ToQuery SnapshotDiskContainer where
  toQuery SnapshotDiskContainer' {..} =
    Prelude.mconcat
      [ "Description" Data.=: description,
        "Format" Data.=: format,
        "Url" Data.=: url,
        "UserBucket" Data.=: userBucket
      ]
