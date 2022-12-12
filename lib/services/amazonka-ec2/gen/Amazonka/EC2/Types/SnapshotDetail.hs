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
-- Module      : Amazonka.EC2.Types.SnapshotDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SnapshotDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.UserBucketDetails
import qualified Amazonka.Prelude as Prelude

-- | Describes the snapshot created from the imported disk.
--
-- /See:/ 'newSnapshotDetail' smart constructor.
data SnapshotDetail = SnapshotDetail'
  { -- | A description for the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The block device mapping for the snapshot.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in the snapshot, in GiB.
    diskImageSize :: Prelude.Maybe Prelude.Double,
    -- | The format of the disk image from which the snapshot is created.
    format :: Prelude.Maybe Prelude.Text,
    -- | The percentage of progress for the task.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The snapshot ID of the disk being imported.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | A brief status of the snapshot creation.
    status :: Prelude.Maybe Prelude.Text,
    -- | A detailed status message for the snapshot creation.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The URL used to access the disk image.
    url :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Prelude.Maybe UserBucketDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'snapshotDetail_description' - A description for the snapshot.
--
-- 'deviceName', 'snapshotDetail_deviceName' - The block device mapping for the snapshot.
--
-- 'diskImageSize', 'snapshotDetail_diskImageSize' - The size of the disk in the snapshot, in GiB.
--
-- 'format', 'snapshotDetail_format' - The format of the disk image from which the snapshot is created.
--
-- 'progress', 'snapshotDetail_progress' - The percentage of progress for the task.
--
-- 'snapshotId', 'snapshotDetail_snapshotId' - The snapshot ID of the disk being imported.
--
-- 'status', 'snapshotDetail_status' - A brief status of the snapshot creation.
--
-- 'statusMessage', 'snapshotDetail_statusMessage' - A detailed status message for the snapshot creation.
--
-- 'url', 'snapshotDetail_url' - The URL used to access the disk image.
--
-- 'userBucket', 'snapshotDetail_userBucket' - The Amazon S3 bucket for the disk image.
newSnapshotDetail ::
  SnapshotDetail
newSnapshotDetail =
  SnapshotDetail'
    { description = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      diskImageSize = Prelude.Nothing,
      format = Prelude.Nothing,
      progress = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      url = Prelude.Nothing,
      userBucket = Prelude.Nothing
    }

-- | A description for the snapshot.
snapshotDetail_description :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_description = Lens.lens (\SnapshotDetail' {description} -> description) (\s@SnapshotDetail' {} a -> s {description = a} :: SnapshotDetail)

-- | The block device mapping for the snapshot.
snapshotDetail_deviceName :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_deviceName = Lens.lens (\SnapshotDetail' {deviceName} -> deviceName) (\s@SnapshotDetail' {} a -> s {deviceName = a} :: SnapshotDetail)

-- | The size of the disk in the snapshot, in GiB.
snapshotDetail_diskImageSize :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Double)
snapshotDetail_diskImageSize = Lens.lens (\SnapshotDetail' {diskImageSize} -> diskImageSize) (\s@SnapshotDetail' {} a -> s {diskImageSize = a} :: SnapshotDetail)

-- | The format of the disk image from which the snapshot is created.
snapshotDetail_format :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_format = Lens.lens (\SnapshotDetail' {format} -> format) (\s@SnapshotDetail' {} a -> s {format = a} :: SnapshotDetail)

-- | The percentage of progress for the task.
snapshotDetail_progress :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_progress = Lens.lens (\SnapshotDetail' {progress} -> progress) (\s@SnapshotDetail' {} a -> s {progress = a} :: SnapshotDetail)

-- | The snapshot ID of the disk being imported.
snapshotDetail_snapshotId :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_snapshotId = Lens.lens (\SnapshotDetail' {snapshotId} -> snapshotId) (\s@SnapshotDetail' {} a -> s {snapshotId = a} :: SnapshotDetail)

-- | A brief status of the snapshot creation.
snapshotDetail_status :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_status = Lens.lens (\SnapshotDetail' {status} -> status) (\s@SnapshotDetail' {} a -> s {status = a} :: SnapshotDetail)

-- | A detailed status message for the snapshot creation.
snapshotDetail_statusMessage :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_statusMessage = Lens.lens (\SnapshotDetail' {statusMessage} -> statusMessage) (\s@SnapshotDetail' {} a -> s {statusMessage = a} :: SnapshotDetail)

-- | The URL used to access the disk image.
snapshotDetail_url :: Lens.Lens' SnapshotDetail (Prelude.Maybe Prelude.Text)
snapshotDetail_url = Lens.lens (\SnapshotDetail' {url} -> url) (\s@SnapshotDetail' {} a -> s {url = a} :: SnapshotDetail)

-- | The Amazon S3 bucket for the disk image.
snapshotDetail_userBucket :: Lens.Lens' SnapshotDetail (Prelude.Maybe UserBucketDetails)
snapshotDetail_userBucket = Lens.lens (\SnapshotDetail' {userBucket} -> userBucket) (\s@SnapshotDetail' {} a -> s {userBucket = a} :: SnapshotDetail)

instance Data.FromXML SnapshotDetail where
  parseXML x =
    SnapshotDetail'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "deviceName")
      Prelude.<*> (x Data..@? "diskImageSize")
      Prelude.<*> (x Data..@? "format")
      Prelude.<*> (x Data..@? "progress")
      Prelude.<*> (x Data..@? "snapshotId")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> (x Data..@? "url")
      Prelude.<*> (x Data..@? "userBucket")

instance Prelude.Hashable SnapshotDetail where
  hashWithSalt _salt SnapshotDetail' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` diskImageSize
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` userBucket

instance Prelude.NFData SnapshotDetail where
  rnf SnapshotDetail' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf diskImageSize
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf userBucket
