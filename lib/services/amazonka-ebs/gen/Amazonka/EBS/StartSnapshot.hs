{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EBS.StartSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon EBS snapshot. The new snapshot enters the @pending@
-- state after the request completes.
--
-- After creating the snapshot, use
-- <https://docs.aws.amazon.com/ebs/latest/APIReference/API_PutSnapshotBlock.html PutSnapshotBlock>
-- to write blocks of data to the snapshot.
module Amazonka.EBS.StartSnapshot
  ( -- * Creating a Request
    StartSnapshot (..),
    newStartSnapshot,

    -- * Request Lenses
    startSnapshot_clientToken,
    startSnapshot_description,
    startSnapshot_encrypted,
    startSnapshot_kmsKeyArn,
    startSnapshot_parentSnapshotId,
    startSnapshot_tags,
    startSnapshot_timeout,
    startSnapshot_volumeSize,

    -- * Destructuring the Response
    StartSnapshotResponse (..),
    newStartSnapshotResponse,

    -- * Response Lenses
    startSnapshotResponse_blockSize,
    startSnapshotResponse_description,
    startSnapshotResponse_kmsKeyArn,
    startSnapshotResponse_ownerId,
    startSnapshotResponse_parentSnapshotId,
    startSnapshotResponse_snapshotId,
    startSnapshotResponse_startTime,
    startSnapshotResponse_status,
    startSnapshotResponse_tags,
    startSnapshotResponse_volumeSize,
    startSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EBS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSnapshot' smart constructor.
data StartSnapshot = StartSnapshot'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Idempotency ensures that an API request
    -- completes only once. With an idempotent request, if the original request
    -- completes successfully. The subsequent retries with the same client
    -- token return the result from the original successful request and they
    -- have no additional effect.
    --
    -- If you do not specify a client token, one is automatically generated by
    -- the Amazon Web Services SDK.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-direct-api-idempotency.html Idempotency for StartSnapshot API>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to encrypt the snapshot.
    --
    -- You can\'t specify __Encrypted__ and __ParentSnapshotId__ in the same
    -- request. If you specify both parameters, the request fails with
    -- @ValidationException@.
    --
    -- The encryption status of the snapshot depends on the values that you
    -- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
    -- whether your Amazon Web Services account is enabled for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- To create an encrypted snapshot, you must have permission to use the KMS
    -- key. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
    -- to be used to encrypt the snapshot.
    --
    -- The encryption status of the snapshot depends on the values that you
    -- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
    -- whether your Amazon Web Services account is enabled for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- To create an encrypted snapshot, you must have permission to use the KMS
    -- key. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    kmsKeyArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the parent snapshot. If there is no parent snapshot, or if you
    -- are creating the first snapshot for an on-premises volume, omit this
    -- parameter.
    --
    -- You can\'t specify __ParentSnapshotId__ and __Encrypted__ in the same
    -- request. If you specify both parameters, the request fails with
    -- @ValidationException@.
    --
    -- The encryption status of the snapshot depends on the values that you
    -- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
    -- whether your Amazon Web Services account is enabled for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- If you specify an encrypted parent snapshot, you must have permission to
    -- use the KMS key that was used to encrypt the parent snapshot. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    parentSnapshotId :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The amount of time (in minutes) after which the snapshot is
    -- automatically cancelled if:
    --
    -- -   No blocks are written to the snapshot.
    --
    -- -   The snapshot is not completed after writing the last block of data.
    --
    -- If no value is specified, the timeout defaults to @60@ minutes.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The size of the volume, in GiB. The maximum size is @65536@ GiB (64
    -- TiB).
    volumeSize :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startSnapshot_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Idempotency ensures that an API request
-- completes only once. With an idempotent request, if the original request
-- completes successfully. The subsequent retries with the same client
-- token return the result from the original successful request and they
-- have no additional effect.
--
-- If you do not specify a client token, one is automatically generated by
-- the Amazon Web Services SDK.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-direct-api-idempotency.html Idempotency for StartSnapshot API>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'description', 'startSnapshot_description' - A description for the snapshot.
--
-- 'encrypted', 'startSnapshot_encrypted' - Indicates whether to encrypt the snapshot.
--
-- You can\'t specify __Encrypted__ and __ParentSnapshotId__ in the same
-- request. If you specify both parameters, the request fails with
-- @ValidationException@.
--
-- The encryption status of the snapshot depends on the values that you
-- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
-- whether your Amazon Web Services account is enabled for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- To create an encrypted snapshot, you must have permission to use the KMS
-- key. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'kmsKeyArn', 'startSnapshot_kmsKeyArn' - The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- to be used to encrypt the snapshot.
--
-- The encryption status of the snapshot depends on the values that you
-- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
-- whether your Amazon Web Services account is enabled for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- To create an encrypted snapshot, you must have permission to use the KMS
-- key. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'parentSnapshotId', 'startSnapshot_parentSnapshotId' - The ID of the parent snapshot. If there is no parent snapshot, or if you
-- are creating the first snapshot for an on-premises volume, omit this
-- parameter.
--
-- You can\'t specify __ParentSnapshotId__ and __Encrypted__ in the same
-- request. If you specify both parameters, the request fails with
-- @ValidationException@.
--
-- The encryption status of the snapshot depends on the values that you
-- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
-- whether your Amazon Web Services account is enabled for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you specify an encrypted parent snapshot, you must have permission to
-- use the KMS key that was used to encrypt the parent snapshot. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'tags', 'startSnapshot_tags' - The tags to apply to the snapshot.
--
-- 'timeout', 'startSnapshot_timeout' - The amount of time (in minutes) after which the snapshot is
-- automatically cancelled if:
--
-- -   No blocks are written to the snapshot.
--
-- -   The snapshot is not completed after writing the last block of data.
--
-- If no value is specified, the timeout defaults to @60@ minutes.
--
-- 'volumeSize', 'startSnapshot_volumeSize' - The size of the volume, in GiB. The maximum size is @65536@ GiB (64
-- TiB).
newStartSnapshot ::
  -- | 'volumeSize'
  Prelude.Natural ->
  StartSnapshot
newStartSnapshot pVolumeSize_ =
  StartSnapshot'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      parentSnapshotId = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      volumeSize = pVolumeSize_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Idempotency ensures that an API request
-- completes only once. With an idempotent request, if the original request
-- completes successfully. The subsequent retries with the same client
-- token return the result from the original successful request and they
-- have no additional effect.
--
-- If you do not specify a client token, one is automatically generated by
-- the Amazon Web Services SDK.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-direct-api-idempotency.html Idempotency for StartSnapshot API>
-- in the /Amazon Elastic Compute Cloud User Guide/.
startSnapshot_clientToken :: Lens.Lens' StartSnapshot (Prelude.Maybe Prelude.Text)
startSnapshot_clientToken = Lens.lens (\StartSnapshot' {clientToken} -> clientToken) (\s@StartSnapshot' {} a -> s {clientToken = a} :: StartSnapshot)

-- | A description for the snapshot.
startSnapshot_description :: Lens.Lens' StartSnapshot (Prelude.Maybe Prelude.Text)
startSnapshot_description = Lens.lens (\StartSnapshot' {description} -> description) (\s@StartSnapshot' {} a -> s {description = a} :: StartSnapshot)

-- | Indicates whether to encrypt the snapshot.
--
-- You can\'t specify __Encrypted__ and __ParentSnapshotId__ in the same
-- request. If you specify both parameters, the request fails with
-- @ValidationException@.
--
-- The encryption status of the snapshot depends on the values that you
-- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
-- whether your Amazon Web Services account is enabled for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- To create an encrypted snapshot, you must have permission to use the KMS
-- key. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
-- in the /Amazon Elastic Compute Cloud User Guide/.
startSnapshot_encrypted :: Lens.Lens' StartSnapshot (Prelude.Maybe Prelude.Bool)
startSnapshot_encrypted = Lens.lens (\StartSnapshot' {encrypted} -> encrypted) (\s@StartSnapshot' {} a -> s {encrypted = a} :: StartSnapshot)

-- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- to be used to encrypt the snapshot.
--
-- The encryption status of the snapshot depends on the values that you
-- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
-- whether your Amazon Web Services account is enabled for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- To create an encrypted snapshot, you must have permission to use the KMS
-- key. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
-- in the /Amazon Elastic Compute Cloud User Guide/.
startSnapshot_kmsKeyArn :: Lens.Lens' StartSnapshot (Prelude.Maybe Prelude.Text)
startSnapshot_kmsKeyArn = Lens.lens (\StartSnapshot' {kmsKeyArn} -> kmsKeyArn) (\s@StartSnapshot' {} a -> s {kmsKeyArn = a} :: StartSnapshot) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the parent snapshot. If there is no parent snapshot, or if you
-- are creating the first snapshot for an on-premises volume, omit this
-- parameter.
--
-- You can\'t specify __ParentSnapshotId__ and __Encrypted__ in the same
-- request. If you specify both parameters, the request fails with
-- @ValidationException@.
--
-- The encryption status of the snapshot depends on the values that you
-- specify for __Encrypted__, __KmsKeyArn__, and __ParentSnapshotId__, and
-- whether your Amazon Web Services account is enabled for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default encryption by default>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapis-using-encryption.html Using encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you specify an encrypted parent snapshot, you must have permission to
-- use the KMS key that was used to encrypt the parent snapshot. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebsapi-permissions.html#ebsapi-kms-permissions Permissions to use Key Management Service keys>
-- in the /Amazon Elastic Compute Cloud User Guide/.
startSnapshot_parentSnapshotId :: Lens.Lens' StartSnapshot (Prelude.Maybe Prelude.Text)
startSnapshot_parentSnapshotId = Lens.lens (\StartSnapshot' {parentSnapshotId} -> parentSnapshotId) (\s@StartSnapshot' {} a -> s {parentSnapshotId = a} :: StartSnapshot)

-- | The tags to apply to the snapshot.
startSnapshot_tags :: Lens.Lens' StartSnapshot (Prelude.Maybe [Tag])
startSnapshot_tags = Lens.lens (\StartSnapshot' {tags} -> tags) (\s@StartSnapshot' {} a -> s {tags = a} :: StartSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time (in minutes) after which the snapshot is
-- automatically cancelled if:
--
-- -   No blocks are written to the snapshot.
--
-- -   The snapshot is not completed after writing the last block of data.
--
-- If no value is specified, the timeout defaults to @60@ minutes.
startSnapshot_timeout :: Lens.Lens' StartSnapshot (Prelude.Maybe Prelude.Natural)
startSnapshot_timeout = Lens.lens (\StartSnapshot' {timeout} -> timeout) (\s@StartSnapshot' {} a -> s {timeout = a} :: StartSnapshot)

-- | The size of the volume, in GiB. The maximum size is @65536@ GiB (64
-- TiB).
startSnapshot_volumeSize :: Lens.Lens' StartSnapshot Prelude.Natural
startSnapshot_volumeSize = Lens.lens (\StartSnapshot' {volumeSize} -> volumeSize) (\s@StartSnapshot' {} a -> s {volumeSize = a} :: StartSnapshot)

instance Core.AWSRequest StartSnapshot where
  type
    AWSResponse StartSnapshot =
      StartSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSnapshotResponse'
            Prelude.<$> (x Data..?> "BlockSize")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "KmsKeyArn")
            Prelude.<*> (x Data..?> "OwnerId")
            Prelude.<*> (x Data..?> "ParentSnapshotId")
            Prelude.<*> (x Data..?> "SnapshotId")
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VolumeSize")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSnapshot where
  hashWithSalt _salt StartSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` parentSnapshotId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` volumeSize

instance Prelude.NFData StartSnapshot where
  rnf StartSnapshot' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf encrypted `Prelude.seq`
          Prelude.rnf kmsKeyArn `Prelude.seq`
            Prelude.rnf parentSnapshotId `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf timeout `Prelude.seq`
                  Prelude.rnf volumeSize

instance Data.ToHeaders StartSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSnapshot where
  toJSON StartSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Encrypted" Data..=) Prelude.<$> encrypted,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("ParentSnapshotId" Data..=)
              Prelude.<$> parentSnapshotId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("VolumeSize" Data..= volumeSize)
          ]
      )

instance Data.ToPath StartSnapshot where
  toPath = Prelude.const "/snapshots"

instance Data.ToQuery StartSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSnapshotResponse' smart constructor.
data StartSnapshotResponse = StartSnapshotResponse'
  { -- | The size of the blocks in the snapshot, in bytes.
    blockSize :: Prelude.Maybe Prelude.Int,
    -- | The description of the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
    -- used to encrypt the snapshot.
    kmsKeyArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Web Services account ID of the snapshot owner.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent snapshot.
    parentSnapshotId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the snapshot was created.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the snapshot.
    status :: Prelude.Maybe Status,
    -- | The tags applied to the snapshot. You can specify up to 50 tags per
    -- snapshot. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The size of the volume, in GiB.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockSize', 'startSnapshotResponse_blockSize' - The size of the blocks in the snapshot, in bytes.
--
-- 'description', 'startSnapshotResponse_description' - The description of the snapshot.
--
-- 'kmsKeyArn', 'startSnapshotResponse_kmsKeyArn' - The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- used to encrypt the snapshot.
--
-- 'ownerId', 'startSnapshotResponse_ownerId' - The Amazon Web Services account ID of the snapshot owner.
--
-- 'parentSnapshotId', 'startSnapshotResponse_parentSnapshotId' - The ID of the parent snapshot.
--
-- 'snapshotId', 'startSnapshotResponse_snapshotId' - The ID of the snapshot.
--
-- 'startTime', 'startSnapshotResponse_startTime' - The timestamp when the snapshot was created.
--
-- 'status', 'startSnapshotResponse_status' - The status of the snapshot.
--
-- 'tags', 'startSnapshotResponse_tags' - The tags applied to the snapshot. You can specify up to 50 tags per
-- snapshot. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'volumeSize', 'startSnapshotResponse_volumeSize' - The size of the volume, in GiB.
--
-- 'httpStatus', 'startSnapshotResponse_httpStatus' - The response's http status code.
newStartSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSnapshotResponse
newStartSnapshotResponse pHttpStatus_ =
  StartSnapshotResponse'
    { blockSize = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      parentSnapshotId = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The size of the blocks in the snapshot, in bytes.
startSnapshotResponse_blockSize :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.Int)
startSnapshotResponse_blockSize = Lens.lens (\StartSnapshotResponse' {blockSize} -> blockSize) (\s@StartSnapshotResponse' {} a -> s {blockSize = a} :: StartSnapshotResponse)

-- | The description of the snapshot.
startSnapshotResponse_description :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.Text)
startSnapshotResponse_description = Lens.lens (\StartSnapshotResponse' {description} -> description) (\s@StartSnapshotResponse' {} a -> s {description = a} :: StartSnapshotResponse)

-- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- used to encrypt the snapshot.
startSnapshotResponse_kmsKeyArn :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.Text)
startSnapshotResponse_kmsKeyArn = Lens.lens (\StartSnapshotResponse' {kmsKeyArn} -> kmsKeyArn) (\s@StartSnapshotResponse' {} a -> s {kmsKeyArn = a} :: StartSnapshotResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Web Services account ID of the snapshot owner.
startSnapshotResponse_ownerId :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.Text)
startSnapshotResponse_ownerId = Lens.lens (\StartSnapshotResponse' {ownerId} -> ownerId) (\s@StartSnapshotResponse' {} a -> s {ownerId = a} :: StartSnapshotResponse)

-- | The ID of the parent snapshot.
startSnapshotResponse_parentSnapshotId :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.Text)
startSnapshotResponse_parentSnapshotId = Lens.lens (\StartSnapshotResponse' {parentSnapshotId} -> parentSnapshotId) (\s@StartSnapshotResponse' {} a -> s {parentSnapshotId = a} :: StartSnapshotResponse)

-- | The ID of the snapshot.
startSnapshotResponse_snapshotId :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.Text)
startSnapshotResponse_snapshotId = Lens.lens (\StartSnapshotResponse' {snapshotId} -> snapshotId) (\s@StartSnapshotResponse' {} a -> s {snapshotId = a} :: StartSnapshotResponse)

-- | The timestamp when the snapshot was created.
startSnapshotResponse_startTime :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.UTCTime)
startSnapshotResponse_startTime = Lens.lens (\StartSnapshotResponse' {startTime} -> startTime) (\s@StartSnapshotResponse' {} a -> s {startTime = a} :: StartSnapshotResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the snapshot.
startSnapshotResponse_status :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Status)
startSnapshotResponse_status = Lens.lens (\StartSnapshotResponse' {status} -> status) (\s@StartSnapshotResponse' {} a -> s {status = a} :: StartSnapshotResponse)

-- | The tags applied to the snapshot. You can specify up to 50 tags per
-- snapshot. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
startSnapshotResponse_tags :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe [Tag])
startSnapshotResponse_tags = Lens.lens (\StartSnapshotResponse' {tags} -> tags) (\s@StartSnapshotResponse' {} a -> s {tags = a} :: StartSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The size of the volume, in GiB.
startSnapshotResponse_volumeSize :: Lens.Lens' StartSnapshotResponse (Prelude.Maybe Prelude.Natural)
startSnapshotResponse_volumeSize = Lens.lens (\StartSnapshotResponse' {volumeSize} -> volumeSize) (\s@StartSnapshotResponse' {} a -> s {volumeSize = a} :: StartSnapshotResponse)

-- | The response's http status code.
startSnapshotResponse_httpStatus :: Lens.Lens' StartSnapshotResponse Prelude.Int
startSnapshotResponse_httpStatus = Lens.lens (\StartSnapshotResponse' {httpStatus} -> httpStatus) (\s@StartSnapshotResponse' {} a -> s {httpStatus = a} :: StartSnapshotResponse)

instance Prelude.NFData StartSnapshotResponse where
  rnf StartSnapshotResponse' {..} =
    Prelude.rnf blockSize `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf kmsKeyArn `Prelude.seq`
          Prelude.rnf ownerId `Prelude.seq`
            Prelude.rnf parentSnapshotId `Prelude.seq`
              Prelude.rnf snapshotId `Prelude.seq`
                Prelude.rnf startTime `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf volumeSize `Prelude.seq`
                        Prelude.rnf httpStatus
