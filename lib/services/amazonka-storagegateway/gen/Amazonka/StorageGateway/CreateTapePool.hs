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
-- Module      : Amazonka.StorageGateway.CreateTapePool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom tape pool. You can use custom tape pool to enable
-- tape retention lock on tapes that are archived in the custom pool.
module Amazonka.StorageGateway.CreateTapePool
  ( -- * Creating a Request
    CreateTapePool (..),
    newCreateTapePool,

    -- * Request Lenses
    createTapePool_retentionLockTimeInDays,
    createTapePool_retentionLockType,
    createTapePool_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newCreateTapePool' smart constructor.
data CreateTapePool = CreateTapePool'
  { -- | Tape retention lock time is set in days. Tape retention lock can be
    -- enabled for up to 100 years (36,500 days).
    retentionLockTimeInDays :: Prelude.Maybe Prelude.Natural,
    -- | Tape retention lock can be configured in two modes. When configured in
    -- governance mode, Amazon Web Services accounts with specific IAM
    -- permissions are authorized to remove the tape retention lock from
    -- archived virtual tapes. When configured in compliance mode, the tape
    -- retention lock cannot be removed by any user, including the root Amazon
    -- Web Services account.
    retentionLockType :: Prelude.Maybe RetentionLockType,
    -- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a
    -- key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
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
-- 'retentionLockTimeInDays', 'createTapePool_retentionLockTimeInDays' - Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
--
-- 'retentionLockType', 'createTapePool_retentionLockType' - Tape retention lock can be configured in two modes. When configured in
-- governance mode, Amazon Web Services accounts with specific IAM
-- permissions are authorized to remove the tape retention lock from
-- archived virtual tapes. When configured in compliance mode, the tape
-- retention lock cannot be removed by any user, including the root Amazon
-- Web Services account.
--
-- 'tags', 'createTapePool_tags' - A list of up to 50 tags that can be assigned to tape pool. Each tag is a
-- key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
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
    { retentionLockTimeInDays =
        Prelude.Nothing,
      retentionLockType = Prelude.Nothing,
      tags = Prelude.Nothing,
      poolName = pPoolName_,
      storageClass = pStorageClass_
    }

-- | Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
createTapePool_retentionLockTimeInDays :: Lens.Lens' CreateTapePool (Prelude.Maybe Prelude.Natural)
createTapePool_retentionLockTimeInDays = Lens.lens (\CreateTapePool' {retentionLockTimeInDays} -> retentionLockTimeInDays) (\s@CreateTapePool' {} a -> s {retentionLockTimeInDays = a} :: CreateTapePool)

-- | Tape retention lock can be configured in two modes. When configured in
-- governance mode, Amazon Web Services accounts with specific IAM
-- permissions are authorized to remove the tape retention lock from
-- archived virtual tapes. When configured in compliance mode, the tape
-- retention lock cannot be removed by any user, including the root Amazon
-- Web Services account.
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
createTapePool_tags = Lens.lens (\CreateTapePool' {tags} -> tags) (\s@CreateTapePool' {} a -> s {tags = a} :: CreateTapePool) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTapePoolResponse'
            Prelude.<$> (x Data..?> "PoolARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTapePool where
  hashWithSalt _salt CreateTapePool' {..} =
    _salt
      `Prelude.hashWithSalt` retentionLockTimeInDays
      `Prelude.hashWithSalt` retentionLockType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` poolName
      `Prelude.hashWithSalt` storageClass

instance Prelude.NFData CreateTapePool where
  rnf CreateTapePool' {..} =
    Prelude.rnf retentionLockTimeInDays
      `Prelude.seq` Prelude.rnf retentionLockType
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf poolName
      `Prelude.seq` Prelude.rnf storageClass

instance Data.ToHeaders CreateTapePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.CreateTapePool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTapePool where
  toJSON CreateTapePool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RetentionLockTimeInDays" Data..=)
              Prelude.<$> retentionLockTimeInDays,
            ("RetentionLockType" Data..=)
              Prelude.<$> retentionLockType,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("PoolName" Data..= poolName),
            Prelude.Just ("StorageClass" Data..= storageClass)
          ]
      )

instance Data.ToPath CreateTapePool where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTapePool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTapePoolResponse' smart constructor.
data CreateTapePoolResponse = CreateTapePoolResponse'
  { -- | The unique Amazon Resource Name (ARN) that represents the custom tape
    -- pool. Use the ListTapePools operation to return a list of tape pools for
    -- your account and Amazon Web Services Region.
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
-- your account and Amazon Web Services Region.
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
-- your account and Amazon Web Services Region.
createTapePoolResponse_poolARN :: Lens.Lens' CreateTapePoolResponse (Prelude.Maybe Prelude.Text)
createTapePoolResponse_poolARN = Lens.lens (\CreateTapePoolResponse' {poolARN} -> poolARN) (\s@CreateTapePoolResponse' {} a -> s {poolARN = a} :: CreateTapePoolResponse)

-- | The response's http status code.
createTapePoolResponse_httpStatus :: Lens.Lens' CreateTapePoolResponse Prelude.Int
createTapePoolResponse_httpStatus = Lens.lens (\CreateTapePoolResponse' {httpStatus} -> httpStatus) (\s@CreateTapePoolResponse' {} a -> s {httpStatus = a} :: CreateTapePoolResponse)

instance Prelude.NFData CreateTapePoolResponse where
  rnf CreateTapePoolResponse' {..} =
    Prelude.rnf poolARN
      `Prelude.seq` Prelude.rnf httpStatus
