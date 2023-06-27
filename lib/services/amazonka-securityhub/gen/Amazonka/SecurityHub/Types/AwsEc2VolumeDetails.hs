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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VolumeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VolumeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VolumeAttachment

-- | Details about an EC2 volume.
--
-- /See:/ 'newAwsEc2VolumeDetails' smart constructor.
data AwsEc2VolumeDetails = AwsEc2VolumeDetails'
  { -- | The volume attachments.
    attachments :: Prelude.Maybe [AwsEc2VolumeAttachment],
    -- | Indicates when the volume was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    createTime :: Prelude.Maybe Prelude.Text,
    -- | The device name for the volume that is attached to the instance.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the volume is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the KMS key that was used to protect the volume encryption
    -- key for the volume.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The size of the volume, in GiBs.
    size :: Prelude.Maybe Prelude.Int,
    -- | The snapshot from which the volume was created.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The volume state. Valid values are as follows:
    --
    -- -   @available@
    --
    -- -   @creating@
    --
    -- -   @deleted@
    --
    -- -   @deleting@
    --
    -- -   @error@
    --
    -- -   @in-use@
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the volume.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the volume was scanned or skipped.
    volumeScanStatus :: Prelude.Maybe Prelude.Text,
    -- | The volume type.
    volumeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VolumeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachments', 'awsEc2VolumeDetails_attachments' - The volume attachments.
--
-- 'createTime', 'awsEc2VolumeDetails_createTime' - Indicates when the volume was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
--
-- 'deviceName', 'awsEc2VolumeDetails_deviceName' - The device name for the volume that is attached to the instance.
--
-- 'encrypted', 'awsEc2VolumeDetails_encrypted' - Specifies whether the volume is encrypted.
--
-- 'kmsKeyId', 'awsEc2VolumeDetails_kmsKeyId' - The ARN of the KMS key that was used to protect the volume encryption
-- key for the volume.
--
-- 'size', 'awsEc2VolumeDetails_size' - The size of the volume, in GiBs.
--
-- 'snapshotId', 'awsEc2VolumeDetails_snapshotId' - The snapshot from which the volume was created.
--
-- 'status', 'awsEc2VolumeDetails_status' - The volume state. Valid values are as follows:
--
-- -   @available@
--
-- -   @creating@
--
-- -   @deleted@
--
-- -   @deleting@
--
-- -   @error@
--
-- -   @in-use@
--
-- 'volumeId', 'awsEc2VolumeDetails_volumeId' - The ID of the volume.
--
-- 'volumeScanStatus', 'awsEc2VolumeDetails_volumeScanStatus' - Indicates whether the volume was scanned or skipped.
--
-- 'volumeType', 'awsEc2VolumeDetails_volumeType' - The volume type.
newAwsEc2VolumeDetails ::
  AwsEc2VolumeDetails
newAwsEc2VolumeDetails =
  AwsEc2VolumeDetails'
    { attachments = Prelude.Nothing,
      createTime = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      size = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      status = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      volumeScanStatus = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | The volume attachments.
awsEc2VolumeDetails_attachments :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe [AwsEc2VolumeAttachment])
awsEc2VolumeDetails_attachments = Lens.lens (\AwsEc2VolumeDetails' {attachments} -> attachments) (\s@AwsEc2VolumeDetails' {} a -> s {attachments = a} :: AwsEc2VolumeDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the volume was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsEc2VolumeDetails_createTime :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_createTime = Lens.lens (\AwsEc2VolumeDetails' {createTime} -> createTime) (\s@AwsEc2VolumeDetails' {} a -> s {createTime = a} :: AwsEc2VolumeDetails)

-- | The device name for the volume that is attached to the instance.
awsEc2VolumeDetails_deviceName :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_deviceName = Lens.lens (\AwsEc2VolumeDetails' {deviceName} -> deviceName) (\s@AwsEc2VolumeDetails' {} a -> s {deviceName = a} :: AwsEc2VolumeDetails)

-- | Specifies whether the volume is encrypted.
awsEc2VolumeDetails_encrypted :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Bool)
awsEc2VolumeDetails_encrypted = Lens.lens (\AwsEc2VolumeDetails' {encrypted} -> encrypted) (\s@AwsEc2VolumeDetails' {} a -> s {encrypted = a} :: AwsEc2VolumeDetails)

-- | The ARN of the KMS key that was used to protect the volume encryption
-- key for the volume.
awsEc2VolumeDetails_kmsKeyId :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_kmsKeyId = Lens.lens (\AwsEc2VolumeDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsEc2VolumeDetails' {} a -> s {kmsKeyId = a} :: AwsEc2VolumeDetails)

-- | The size of the volume, in GiBs.
awsEc2VolumeDetails_size :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Int)
awsEc2VolumeDetails_size = Lens.lens (\AwsEc2VolumeDetails' {size} -> size) (\s@AwsEc2VolumeDetails' {} a -> s {size = a} :: AwsEc2VolumeDetails)

-- | The snapshot from which the volume was created.
awsEc2VolumeDetails_snapshotId :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_snapshotId = Lens.lens (\AwsEc2VolumeDetails' {snapshotId} -> snapshotId) (\s@AwsEc2VolumeDetails' {} a -> s {snapshotId = a} :: AwsEc2VolumeDetails)

-- | The volume state. Valid values are as follows:
--
-- -   @available@
--
-- -   @creating@
--
-- -   @deleted@
--
-- -   @deleting@
--
-- -   @error@
--
-- -   @in-use@
awsEc2VolumeDetails_status :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_status = Lens.lens (\AwsEc2VolumeDetails' {status} -> status) (\s@AwsEc2VolumeDetails' {} a -> s {status = a} :: AwsEc2VolumeDetails)

-- | The ID of the volume.
awsEc2VolumeDetails_volumeId :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_volumeId = Lens.lens (\AwsEc2VolumeDetails' {volumeId} -> volumeId) (\s@AwsEc2VolumeDetails' {} a -> s {volumeId = a} :: AwsEc2VolumeDetails)

-- | Indicates whether the volume was scanned or skipped.
awsEc2VolumeDetails_volumeScanStatus :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_volumeScanStatus = Lens.lens (\AwsEc2VolumeDetails' {volumeScanStatus} -> volumeScanStatus) (\s@AwsEc2VolumeDetails' {} a -> s {volumeScanStatus = a} :: AwsEc2VolumeDetails)

-- | The volume type.
awsEc2VolumeDetails_volumeType :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_volumeType = Lens.lens (\AwsEc2VolumeDetails' {volumeType} -> volumeType) (\s@AwsEc2VolumeDetails' {} a -> s {volumeType = a} :: AwsEc2VolumeDetails)

instance Data.FromJSON AwsEc2VolumeDetails where
  parseJSON =
    Data.withObject
      "AwsEc2VolumeDetails"
      ( \x ->
          AwsEc2VolumeDetails'
            Prelude.<$> (x Data..:? "Attachments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "DeviceName")
            Prelude.<*> (x Data..:? "Encrypted")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "Size")
            Prelude.<*> (x Data..:? "SnapshotId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VolumeId")
            Prelude.<*> (x Data..:? "VolumeScanStatus")
            Prelude.<*> (x Data..:? "VolumeType")
      )

instance Prelude.Hashable AwsEc2VolumeDetails where
  hashWithSalt _salt AwsEc2VolumeDetails' {..} =
    _salt
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` volumeScanStatus
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData AwsEc2VolumeDetails where
  rnf AwsEc2VolumeDetails' {..} =
    Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf volumeScanStatus
      `Prelude.seq` Prelude.rnf volumeType

instance Data.ToJSON AwsEc2VolumeDetails where
  toJSON AwsEc2VolumeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attachments" Data..=) Prelude.<$> attachments,
            ("CreateTime" Data..=) Prelude.<$> createTime,
            ("DeviceName" Data..=) Prelude.<$> deviceName,
            ("Encrypted" Data..=) Prelude.<$> encrypted,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Size" Data..=) Prelude.<$> size,
            ("SnapshotId" Data..=) Prelude.<$> snapshotId,
            ("Status" Data..=) Prelude.<$> status,
            ("VolumeId" Data..=) Prelude.<$> volumeId,
            ("VolumeScanStatus" Data..=)
              Prelude.<$> volumeScanStatus,
            ("VolumeType" Data..=) Prelude.<$> volumeType
          ]
      )
