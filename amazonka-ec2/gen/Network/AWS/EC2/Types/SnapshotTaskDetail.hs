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
-- Module      : Network.AWS.EC2.Types.SnapshotTaskDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotTaskDetail where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserBucketDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the import snapshot task.
--
-- /See:/ 'newSnapshotTaskDetail' smart constructor.
data SnapshotTaskDetail = SnapshotTaskDetail'
  { -- | The size of the disk in the snapshot, in GiB.
    diskImageSize :: Prelude.Maybe Prelude.Double,
    -- | A detailed status message for the import snapshot task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | A brief status for the import snapshot task.
    status :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The format of the disk image from which the snapshot is created.
    format :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket for the disk image.
    userBucket :: Prelude.Maybe UserBucketDetails,
    -- | The identifier for the AWS Key Management Service (AWS KMS) customer
    -- master key (CMK) that was used to create the encrypted snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The snapshot ID of the disk being imported.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The description of the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The URL of the disk image from which the snapshot is created.
    url :: Prelude.Maybe Prelude.Text,
    -- | The percentage of completion for the import snapshot task.
    progress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SnapshotTaskDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskImageSize', 'snapshotTaskDetail_diskImageSize' - The size of the disk in the snapshot, in GiB.
--
-- 'statusMessage', 'snapshotTaskDetail_statusMessage' - A detailed status message for the import snapshot task.
--
-- 'status', 'snapshotTaskDetail_status' - A brief status for the import snapshot task.
--
-- 'encrypted', 'snapshotTaskDetail_encrypted' - Indicates whether the snapshot is encrypted.
--
-- 'format', 'snapshotTaskDetail_format' - The format of the disk image from which the snapshot is created.
--
-- 'userBucket', 'snapshotTaskDetail_userBucket' - The Amazon S3 bucket for the disk image.
--
-- 'kmsKeyId', 'snapshotTaskDetail_kmsKeyId' - The identifier for the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) that was used to create the encrypted snapshot.
--
-- 'snapshotId', 'snapshotTaskDetail_snapshotId' - The snapshot ID of the disk being imported.
--
-- 'description', 'snapshotTaskDetail_description' - The description of the snapshot.
--
-- 'url', 'snapshotTaskDetail_url' - The URL of the disk image from which the snapshot is created.
--
-- 'progress', 'snapshotTaskDetail_progress' - The percentage of completion for the import snapshot task.
newSnapshotTaskDetail ::
  SnapshotTaskDetail
newSnapshotTaskDetail =
  SnapshotTaskDetail'
    { diskImageSize =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      format = Prelude.Nothing,
      userBucket = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      description = Prelude.Nothing,
      url = Prelude.Nothing,
      progress = Prelude.Nothing
    }

-- | The size of the disk in the snapshot, in GiB.
snapshotTaskDetail_diskImageSize :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Double)
snapshotTaskDetail_diskImageSize = Lens.lens (\SnapshotTaskDetail' {diskImageSize} -> diskImageSize) (\s@SnapshotTaskDetail' {} a -> s {diskImageSize = a} :: SnapshotTaskDetail)

-- | A detailed status message for the import snapshot task.
snapshotTaskDetail_statusMessage :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_statusMessage = Lens.lens (\SnapshotTaskDetail' {statusMessage} -> statusMessage) (\s@SnapshotTaskDetail' {} a -> s {statusMessage = a} :: SnapshotTaskDetail)

-- | A brief status for the import snapshot task.
snapshotTaskDetail_status :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_status = Lens.lens (\SnapshotTaskDetail' {status} -> status) (\s@SnapshotTaskDetail' {} a -> s {status = a} :: SnapshotTaskDetail)

-- | Indicates whether the snapshot is encrypted.
snapshotTaskDetail_encrypted :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Bool)
snapshotTaskDetail_encrypted = Lens.lens (\SnapshotTaskDetail' {encrypted} -> encrypted) (\s@SnapshotTaskDetail' {} a -> s {encrypted = a} :: SnapshotTaskDetail)

-- | The format of the disk image from which the snapshot is created.
snapshotTaskDetail_format :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_format = Lens.lens (\SnapshotTaskDetail' {format} -> format) (\s@SnapshotTaskDetail' {} a -> s {format = a} :: SnapshotTaskDetail)

-- | The Amazon S3 bucket for the disk image.
snapshotTaskDetail_userBucket :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe UserBucketDetails)
snapshotTaskDetail_userBucket = Lens.lens (\SnapshotTaskDetail' {userBucket} -> userBucket) (\s@SnapshotTaskDetail' {} a -> s {userBucket = a} :: SnapshotTaskDetail)

-- | The identifier for the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) that was used to create the encrypted snapshot.
snapshotTaskDetail_kmsKeyId :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_kmsKeyId = Lens.lens (\SnapshotTaskDetail' {kmsKeyId} -> kmsKeyId) (\s@SnapshotTaskDetail' {} a -> s {kmsKeyId = a} :: SnapshotTaskDetail)

-- | The snapshot ID of the disk being imported.
snapshotTaskDetail_snapshotId :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_snapshotId = Lens.lens (\SnapshotTaskDetail' {snapshotId} -> snapshotId) (\s@SnapshotTaskDetail' {} a -> s {snapshotId = a} :: SnapshotTaskDetail)

-- | The description of the snapshot.
snapshotTaskDetail_description :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_description = Lens.lens (\SnapshotTaskDetail' {description} -> description) (\s@SnapshotTaskDetail' {} a -> s {description = a} :: SnapshotTaskDetail)

-- | The URL of the disk image from which the snapshot is created.
snapshotTaskDetail_url :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_url = Lens.lens (\SnapshotTaskDetail' {url} -> url) (\s@SnapshotTaskDetail' {} a -> s {url = a} :: SnapshotTaskDetail)

-- | The percentage of completion for the import snapshot task.
snapshotTaskDetail_progress :: Lens.Lens' SnapshotTaskDetail (Prelude.Maybe Prelude.Text)
snapshotTaskDetail_progress = Lens.lens (\SnapshotTaskDetail' {progress} -> progress) (\s@SnapshotTaskDetail' {} a -> s {progress = a} :: SnapshotTaskDetail)

instance Prelude.FromXML SnapshotTaskDetail where
  parseXML x =
    SnapshotTaskDetail'
      Prelude.<$> (x Prelude..@? "diskImageSize")
      Prelude.<*> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "encrypted")
      Prelude.<*> (x Prelude..@? "format")
      Prelude.<*> (x Prelude..@? "userBucket")
      Prelude.<*> (x Prelude..@? "kmsKeyId")
      Prelude.<*> (x Prelude..@? "snapshotId")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "url")
      Prelude.<*> (x Prelude..@? "progress")

instance Prelude.Hashable SnapshotTaskDetail

instance Prelude.NFData SnapshotTaskDetail
