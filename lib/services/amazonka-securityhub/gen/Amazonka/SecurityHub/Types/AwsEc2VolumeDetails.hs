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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VolumeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VolumeAttachment

-- | Details about an EC2 volume.
--
-- /See:/ 'newAwsEc2VolumeDetails' smart constructor.
data AwsEc2VolumeDetails = AwsEc2VolumeDetails'
  { -- | The volume state.
    status :: Prelude.Maybe Prelude.Text,
    -- | The volume attachments.
    attachments :: Prelude.Maybe [AwsEc2VolumeAttachment],
    -- | The size of the volume, in GiBs.
    size :: Prelude.Maybe Prelude.Int,
    -- | Whether the volume is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the KMS key that was used to protect the volume encryption
    -- key for the volume.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the volume was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createTime :: Prelude.Maybe Prelude.Text,
    -- | The snapshot from which the volume was created.
    snapshotId :: Prelude.Maybe Prelude.Text
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
-- 'status', 'awsEc2VolumeDetails_status' - The volume state.
--
-- 'attachments', 'awsEc2VolumeDetails_attachments' - The volume attachments.
--
-- 'size', 'awsEc2VolumeDetails_size' - The size of the volume, in GiBs.
--
-- 'encrypted', 'awsEc2VolumeDetails_encrypted' - Whether the volume is encrypted.
--
-- 'kmsKeyId', 'awsEc2VolumeDetails_kmsKeyId' - The ARN of the KMS key that was used to protect the volume encryption
-- key for the volume.
--
-- 'createTime', 'awsEc2VolumeDetails_createTime' - Indicates when the volume was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'snapshotId', 'awsEc2VolumeDetails_snapshotId' - The snapshot from which the volume was created.
newAwsEc2VolumeDetails ::
  AwsEc2VolumeDetails
newAwsEc2VolumeDetails =
  AwsEc2VolumeDetails'
    { status = Prelude.Nothing,
      attachments = Prelude.Nothing,
      size = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      createTime = Prelude.Nothing,
      snapshotId = Prelude.Nothing
    }

-- | The volume state.
awsEc2VolumeDetails_status :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_status = Lens.lens (\AwsEc2VolumeDetails' {status} -> status) (\s@AwsEc2VolumeDetails' {} a -> s {status = a} :: AwsEc2VolumeDetails)

-- | The volume attachments.
awsEc2VolumeDetails_attachments :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe [AwsEc2VolumeAttachment])
awsEc2VolumeDetails_attachments = Lens.lens (\AwsEc2VolumeDetails' {attachments} -> attachments) (\s@AwsEc2VolumeDetails' {} a -> s {attachments = a} :: AwsEc2VolumeDetails) Prelude.. Lens.mapping Lens.coerced

-- | The size of the volume, in GiBs.
awsEc2VolumeDetails_size :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Int)
awsEc2VolumeDetails_size = Lens.lens (\AwsEc2VolumeDetails' {size} -> size) (\s@AwsEc2VolumeDetails' {} a -> s {size = a} :: AwsEc2VolumeDetails)

-- | Whether the volume is encrypted.
awsEc2VolumeDetails_encrypted :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Bool)
awsEc2VolumeDetails_encrypted = Lens.lens (\AwsEc2VolumeDetails' {encrypted} -> encrypted) (\s@AwsEc2VolumeDetails' {} a -> s {encrypted = a} :: AwsEc2VolumeDetails)

-- | The ARN of the KMS key that was used to protect the volume encryption
-- key for the volume.
awsEc2VolumeDetails_kmsKeyId :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_kmsKeyId = Lens.lens (\AwsEc2VolumeDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsEc2VolumeDetails' {} a -> s {kmsKeyId = a} :: AwsEc2VolumeDetails)

-- | Indicates when the volume was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEc2VolumeDetails_createTime :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_createTime = Lens.lens (\AwsEc2VolumeDetails' {createTime} -> createTime) (\s@AwsEc2VolumeDetails' {} a -> s {createTime = a} :: AwsEc2VolumeDetails)

-- | The snapshot from which the volume was created.
awsEc2VolumeDetails_snapshotId :: Lens.Lens' AwsEc2VolumeDetails (Prelude.Maybe Prelude.Text)
awsEc2VolumeDetails_snapshotId = Lens.lens (\AwsEc2VolumeDetails' {snapshotId} -> snapshotId) (\s@AwsEc2VolumeDetails' {} a -> s {snapshotId = a} :: AwsEc2VolumeDetails)

instance Core.FromJSON AwsEc2VolumeDetails where
  parseJSON =
    Core.withObject
      "AwsEc2VolumeDetails"
      ( \x ->
          AwsEc2VolumeDetails'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Attachments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Size")
            Prelude.<*> (x Core..:? "Encrypted")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> (x Core..:? "SnapshotId")
      )

instance Prelude.Hashable AwsEc2VolumeDetails where
  hashWithSalt salt' AwsEc2VolumeDetails' {..} =
    salt' `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsEc2VolumeDetails where
  rnf AwsEc2VolumeDetails' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf attachments

instance Core.ToJSON AwsEc2VolumeDetails where
  toJSON AwsEc2VolumeDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("Attachments" Core..=) Prelude.<$> attachments,
            ("Size" Core..=) Prelude.<$> size,
            ("Encrypted" Core..=) Prelude.<$> encrypted,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("CreateTime" Core..=) Prelude.<$> createTime,
            ("SnapshotId" Core..=) Prelude.<$> snapshotId
          ]
      )
