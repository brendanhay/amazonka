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
-- Module      : Amazonka.EC2.Types.SnapshotTaskDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SnapshotTaskDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.UserBucketDetails
import qualified Amazonka.Prelude as Prelude

-- | Details about the import snapshot task.
--
-- /See:/ 'newSnapshotTaskDetail' smart constructor.
data SnapshotTaskDetail = SnapshotTaskDetail'
  { -- | The percentage of completion for the import snapshot task.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The format of the disk image from which the snapshot is created.
    format :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Prelude.Maybe UserBucketDetails,
    -- | The snapshot ID of the disk being imported.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The URL of the disk image from which the snapshot is created.
    url :: Prelude.Maybe Prelude.Text,
    -- | A brief status for the import snapshot task.
    status :: Prelude.Maybe Prelude.Text,
    -- | The description of the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the KMS key that was used to create the encrypted
    -- snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A detailed status message for the import snapshot task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in the snapshot, in GiB.
    diskImageSize :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotTaskDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progress', 'snapshotTaskDetail_progress' - The percentage of completion for the import snapshot task.
--
-- 'format', 'snapshotTaskDetail_format' - The format of the disk image from which the snapshot is created.
--
-- 'userBucket', 'snapshotTaskDetail_userBucket' - The Amazon S3 bucket for the disk image.
--
-- 'snapshotId', 'snapshotTaskDetail_snapshotId' - The snapshot ID of the disk being imported.
--
-- 'url', 'snapshotTaskDetail_url' - The URL of the disk image from which the snapshot is created.
--
-- 'status', 'snapshotTaskDetail_status' - A brief status for the import snapshot task.
--
-- 'description', 'snapshotTaskDetail_description' - The description of the snapshot.
--
-- 'encrypted', 'snapshotTaskDetail_encrypted' - Indicates whether the snapshot is encrypted.
--
-- 'kmsKeyId', 'snapshotTaskDetail_kmsKeyId' - The identifier for the KMS key that was used to create the encrypted
-- snapshot.
--
-- 'statusMessage', 'snapshotTaskDetail_statusMessage' - A detailed status message for the import snapshot task.
--
-- 'diskImageSize', 'snapshotTaskDetail_diskImageSize' - The size of the disk in the snapshot, in GiB.
newSnapshotTaskDetail ::
  SnapshotTaskDetail
newSnapshotTaskDetail =
  SnapshotTaskDetail'
    { progress = Prelude.Nothing,
      format = Prelude.Nothing,
      userBucket = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      url = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      diskImageSize = Prelude.Nothing
    }

-- | The percentage of completion for the import snapshot task.
snapshotTaskDetail_progress :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_progress = Lens.lens (\SnapshotTaskDetail' {progress} -> progress) (\s@SnapshotTaskDetail' {} a -> s {progress = a} :: SnapshotTaskDetail)

-- | The format of the disk image from which the snapshot is created.
snapshotTaskDetail_format :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_format = Lens.lens (\SnapshotTaskDetail' {format} -> format) (\s@SnapshotTaskDetail' {} a -> s {format = a} :: SnapshotTaskDetail)

-- | The Amazon S3 bucket for the disk image.
snapshotTaskDetail_userBucket :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe UserBucketDetails)
snapshotTaskDetail_userBucket = Lens.lens (\SnapshotTaskDetail' {userBucket} -> userBucket) (\s@SnapshotTaskDetail' {} a -> s {userBucket = a} :: SnapshotTaskDetail)

-- | The snapshot ID of the disk being imported.
snapshotTaskDetail_snapshotId :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_snapshotId = Lens.lens (\SnapshotTaskDetail' {snapshotId} -> snapshotId) (\s@SnapshotTaskDetail' {} a -> s {snapshotId = a} :: SnapshotTaskDetail)

-- | The URL of the disk image from which the snapshot is created.
snapshotTaskDetail_url :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_url = Lens.lens (\SnapshotTaskDetail' {url} -> url) (\s@SnapshotTaskDetail' {} a -> s {url = a} :: SnapshotTaskDetail)

-- | A brief status for the import snapshot task.
snapshotTaskDetail_status :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_status = Lens.lens (\SnapshotTaskDetail' {status} -> status) (\s@SnapshotTaskDetail' {} a -> s {status = a} :: SnapshotTaskDetail)

-- | The description of the snapshot.
snapshotTaskDetail_description :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_description = Lens.lens (\SnapshotTaskDetail' {description} -> description) (\s@SnapshotTaskDetail' {} a -> s {description = a} :: SnapshotTaskDetail)

-- | Indicates whether the snapshot is encrypted.
snapshotTaskDetail_encrypted :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Bool)
snapshotTaskDetail_encrypted = Lens.lens (\SnapshotTaskDetail' {encrypted} -> encrypted) (\s@SnapshotTaskDetail' {} a -> s {encrypted = a} :: SnapshotTaskDetail)

-- | The identifier for the KMS key that was used to create the encrypted
-- snapshot.
snapshotTaskDetail_kmsKeyId :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_kmsKeyId = Lens.lens (\SnapshotTaskDetail' {kmsKeyId} -> kmsKeyId) (\s@SnapshotTaskDetail' {} a -> s {kmsKeyId = a} :: SnapshotTaskDetail)

-- | A detailed status message for the import snapshot task.
snapshotTaskDetail_statusMessage :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_statusMessage = Lens.lens (\SnapshotTaskDetail' {statusMessage} -> statusMessage) (\s@SnapshotTaskDetail' {} a -> s {statusMessage = a} :: SnapshotTaskDetail)

-- | The size of the disk in the snapshot, in GiB.
snapshotTaskDetail_diskImageSize :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Double)
snapshotTaskDetail_diskImageSize = Lens.lens (\SnapshotTaskDetail' {diskImageSize} -> diskImageSize) (\s@SnapshotTaskDetail' {} a -> s {diskImageSize = a} :: SnapshotTaskDetail)

instance Data.FromXML SnapshotTaskDetail where
  parseXML x =
    SnapshotTaskDetail'
      Prelude.<$> (x Data..@? "progress")
      Prelude.<*> (x Data..@? "format")
      Prelude.<*> (x Data..@? "userBucket")
      Prelude.<*> (x Data..@? "snapshotId")
      Prelude.<*> (x Data..@? "url")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "encrypted")
      Prelude.<*> (x Data..@? "kmsKeyId")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> (x Data..@? "diskImageSize")

instance Prelude.Hashable SnapshotTaskDetail where
  hashWithSalt _salt SnapshotTaskDetail' {..} =
    _salt `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` userBucket
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` diskImageSize

instance Prelude.NFData SnapshotTaskDetail where
  rnf SnapshotTaskDetail' {..} =
    Prelude.rnf progress
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf userBucket
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf diskImageSize
