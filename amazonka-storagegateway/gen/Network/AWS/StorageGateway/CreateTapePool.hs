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
-- Module      : Network.AWS.StorageGateway.CreateTapePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom tape pool. You can use custom tape pool to enable
-- tape retention lock on tapes that are archived in the custom pool.
module Network.AWS.StorageGateway.CreateTapePool
  ( -- * Creating a Request
    CreateTapePool (..),
    newCreateTapePool,

    -- * Request Lenses
    createTapePool_retentionLockType,
    createTapePool_tags,
    createTapePool_retentionLockTimeInDays,
    createTapePool_poolName,
    createTapePool_storageClass,

    -- * Destructuring the Response
    CreateTapePoolResponse (..),
    newCreateTapePoolResponse,

    -- * Response Lenses
    createTapePoolResponse_poolARN,
    createTapePoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newCreateTapePool' smart constructor.
data CreateTapePool = CreateTapePool'
  { -- | Tape retention lock can be configured in two modes. When configured in
    -- governance mode, AWS accounts with specific IAM permissions are
    -- authorized to remove the tape retention lock from archived virtual
    -- tapes. When configured in compliance mode, the tape retention lock
    -- cannot be removed by any user, including the root AWS account.
    retentionLockType :: Prelude.Maybe RetentionLockType,
    -- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a
    -- key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | Tape retention lock time is set in days. Tape retention lock can be
    -- enabled for up to 100 years (36,500 days).
    retentionLockTimeInDays :: Prelude.Maybe Prelude.Natural,
    -- | The name of the new custom tape pool.
    poolName :: Prelude.Text,
    -- | The storage class that is associated with the new custom pool. When you
    -- use your backup application to eject the tape, the tape is archived
    -- directly into the storage class (S3 Glacier or S3 Glacier Deep Archive)
    -- that corresponds to the pool.
    storageClass :: TapeStorageClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTapePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionLockType', 'createTapePool_retentionLockType' - Tape retention lock can be configured in two modes. When configured in
-- governance mode, AWS accounts with specific IAM permissions are
-- authorized to remove the tape retention lock from archived virtual
-- tapes. When configured in compliance mode, the tape retention lock
-- cannot be removed by any user, including the root AWS account.
--
-- 'tags', 'createTapePool_tags' - A list of up to 50 tags that can be assigned to tape pool. Each tag is a
-- key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'retentionLockTimeInDays', 'createTapePool_retentionLockTimeInDays' - Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
--
-- 'poolName', 'createTapePool_poolName' - The name of the new custom tape pool.
--
-- 'storageClass', 'createTapePool_storageClass' - The storage class that is associated with the new custom pool. When you
-- use your backup application to eject the tape, the tape is archived
-- directly into the storage class (S3 Glacier or S3 Glacier Deep Archive)
-- that corresponds to the pool.
newCreateTapePool ::
  -- | 'poolName'
  Prelude.Text ->
  -- | 'storageClass'
  TapeStorageClass ->
  CreateTapePool
newCreateTapePool pPoolName_ pStorageClass_ =
  CreateTapePool'
    { retentionLockType =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      retentionLockTimeInDays = Prelude.Nothing,
      poolName = pPoolName_,
      storageClass = pStorageClass_
    }

-- | Tape retention lock can be configured in two modes. When configured in
-- governance mode, AWS accounts with specific IAM permissions are
-- authorized to remove the tape retention lock from archived virtual
-- tapes. When configured in compliance mode, the tape retention lock
-- cannot be removed by any user, including the root AWS account.
createTapePool_retentionLockType :: Lens.Lens' CreateTapePool (Prelude.Maybe RetentionLockType)
createTapePool_retentionLockType = Lens.lens (\CreateTapePool' {retentionLockType} -> retentionLockType) (\s@CreateTapePool' {} a -> s {retentionLockType = a} :: CreateTapePool)

-- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a
-- key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createTapePool_tags :: Lens.Lens' CreateTapePool (Prelude.Maybe [Tag])
createTapePool_tags = Lens.lens (\CreateTapePool' {tags} -> tags) (\s@CreateTapePool' {} a -> s {tags = a} :: CreateTapePool) Prelude.. Lens.mapping Lens._Coerce

-- | Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
createTapePool_retentionLockTimeInDays :: Lens.Lens' CreateTapePool (Prelude.Maybe Prelude.Natural)
createTapePool_retentionLockTimeInDays = Lens.lens (\CreateTapePool' {retentionLockTimeInDays} -> retentionLockTimeInDays) (\s@CreateTapePool' {} a -> s {retentionLockTimeInDays = a} :: CreateTapePool)

-- | The name of the new custom tape pool.
createTapePool_poolName :: Lens.Lens' CreateTapePool Prelude.Text
createTapePool_poolName = Lens.lens (\CreateTapePool' {poolName} -> poolName) (\s@CreateTapePool' {} a -> s {poolName = a} :: CreateTapePool)

-- | The storage class that is associated with the new custom pool. When you
-- use your backup application to eject the tape, the tape is archived
-- directly into the storage class (S3 Glacier or S3 Glacier Deep Archive)
-- that corresponds to the pool.
createTapePool_storageClass :: Lens.Lens' CreateTapePool TapeStorageClass
createTapePool_storageClass = Lens.lens (\CreateTapePool' {storageClass} -> storageClass) (\s@CreateTapePool' {} a -> s {storageClass = a} :: CreateTapePool)

instance Core.AWSRequest CreateTapePool where
  type
    AWSResponse CreateTapePool =
      CreateTapePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTapePoolResponse'
            Prelude.<$> (x Core..?> "PoolARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTapePool

instance Prelude.NFData CreateTapePool

instance Core.ToHeaders CreateTapePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateTapePool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTapePool where
  toJSON CreateTapePool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RetentionLockType" Core..=)
              Prelude.<$> retentionLockType,
            ("Tags" Core..=) Prelude.<$> tags,
            ("RetentionLockTimeInDays" Core..=)
              Prelude.<$> retentionLockTimeInDays,
            Prelude.Just ("PoolName" Core..= poolName),
            Prelude.Just ("StorageClass" Core..= storageClass)
          ]
      )

instance Core.ToPath CreateTapePool where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTapePool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTapePoolResponse' smart constructor.
data CreateTapePoolResponse = CreateTapePoolResponse'
  { -- | The unique Amazon Resource Name (ARN) that represents the custom tape
    -- pool. Use the ListTapePools operation to return a list of tape pools for
    -- your account and AWS Region.
    poolARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTapePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolARN', 'createTapePoolResponse_poolARN' - The unique Amazon Resource Name (ARN) that represents the custom tape
-- pool. Use the ListTapePools operation to return a list of tape pools for
-- your account and AWS Region.
--
-- 'httpStatus', 'createTapePoolResponse_httpStatus' - The response's http status code.
newCreateTapePoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTapePoolResponse
newCreateTapePoolResponse pHttpStatus_ =
  CreateTapePoolResponse'
    { poolARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Amazon Resource Name (ARN) that represents the custom tape
-- pool. Use the ListTapePools operation to return a list of tape pools for
-- your account and AWS Region.
createTapePoolResponse_poolARN :: Lens.Lens' CreateTapePoolResponse (Prelude.Maybe Prelude.Text)
createTapePoolResponse_poolARN = Lens.lens (\CreateTapePoolResponse' {poolARN} -> poolARN) (\s@CreateTapePoolResponse' {} a -> s {poolARN = a} :: CreateTapePoolResponse)

-- | The response's http status code.
createTapePoolResponse_httpStatus :: Lens.Lens' CreateTapePoolResponse Prelude.Int
createTapePoolResponse_httpStatus = Lens.lens (\CreateTapePoolResponse' {httpStatus} -> httpStatus) (\s@CreateTapePoolResponse' {} a -> s {httpStatus = a} :: CreateTapePoolResponse)

instance Prelude.NFData CreateTapePoolResponse
