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
-- Module      : Amazonka.Lightsail.CreateBucketAccessKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new access key for the specified Amazon Lightsail bucket.
-- Access keys consist of an access key ID and corresponding secret access
-- key.
--
-- Access keys grant full programmatic access to the specified bucket and
-- its objects. You can have a maximum of two access keys per bucket. Use
-- the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBucketAccessKeys.html GetBucketAccessKeys>
-- action to get a list of current access keys for a specific bucket. For
-- more information about access keys, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-creating-bucket-access-keys Creating access keys for a bucket in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- The @secretAccessKey@ value is returned only in response to the
-- @CreateBucketAccessKey@ action. You can get a secret access key only
-- when you first create an access key; you cannot get the secret access
-- key later. If you lose the secret access key, you must create a new
-- access key.
module Amazonka.Lightsail.CreateBucketAccessKey
  ( -- * Creating a Request
    CreateBucketAccessKey (..),
    newCreateBucketAccessKey,

    -- * Request Lenses
    createBucketAccessKey_bucketName,

    -- * Destructuring the Response
    CreateBucketAccessKeyResponse (..),
    newCreateBucketAccessKeyResponse,

    -- * Response Lenses
    createBucketAccessKeyResponse_operations,
    createBucketAccessKeyResponse_accessKey,
    createBucketAccessKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBucketAccessKey' smart constructor.
data CreateBucketAccessKey = CreateBucketAccessKey'
  { -- | The name of the bucket that the new access key will belong to, and grant
    -- access to.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBucketAccessKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'createBucketAccessKey_bucketName' - The name of the bucket that the new access key will belong to, and grant
-- access to.
newCreateBucketAccessKey ::
  -- | 'bucketName'
  Prelude.Text ->
  CreateBucketAccessKey
newCreateBucketAccessKey pBucketName_ =
  CreateBucketAccessKey' {bucketName = pBucketName_}

-- | The name of the bucket that the new access key will belong to, and grant
-- access to.
createBucketAccessKey_bucketName :: Lens.Lens' CreateBucketAccessKey Prelude.Text
createBucketAccessKey_bucketName = Lens.lens (\CreateBucketAccessKey' {bucketName} -> bucketName) (\s@CreateBucketAccessKey' {} a -> s {bucketName = a} :: CreateBucketAccessKey)

instance Core.AWSRequest CreateBucketAccessKey where
  type
    AWSResponse CreateBucketAccessKey =
      CreateBucketAccessKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBucketAccessKeyResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "accessKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBucketAccessKey where
  hashWithSalt _salt CreateBucketAccessKey' {..} =
    _salt `Prelude.hashWithSalt` bucketName

instance Prelude.NFData CreateBucketAccessKey where
  rnf CreateBucketAccessKey' {..} =
    Prelude.rnf bucketName

instance Core.ToHeaders CreateBucketAccessKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateBucketAccessKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBucketAccessKey where
  toJSON CreateBucketAccessKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("bucketName" Core..= bucketName)]
      )

instance Core.ToPath CreateBucketAccessKey where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBucketAccessKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBucketAccessKeyResponse' smart constructor.
data CreateBucketAccessKeyResponse = CreateBucketAccessKeyResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | An object that describes the access key that is created.
    accessKey :: Prelude.Maybe AccessKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBucketAccessKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createBucketAccessKeyResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'accessKey', 'createBucketAccessKeyResponse_accessKey' - An object that describes the access key that is created.
--
-- 'httpStatus', 'createBucketAccessKeyResponse_httpStatus' - The response's http status code.
newCreateBucketAccessKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBucketAccessKeyResponse
newCreateBucketAccessKeyResponse pHttpStatus_ =
  CreateBucketAccessKeyResponse'
    { operations =
        Prelude.Nothing,
      accessKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createBucketAccessKeyResponse_operations :: Lens.Lens' CreateBucketAccessKeyResponse (Prelude.Maybe [Operation])
createBucketAccessKeyResponse_operations = Lens.lens (\CreateBucketAccessKeyResponse' {operations} -> operations) (\s@CreateBucketAccessKeyResponse' {} a -> s {operations = a} :: CreateBucketAccessKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | An object that describes the access key that is created.
createBucketAccessKeyResponse_accessKey :: Lens.Lens' CreateBucketAccessKeyResponse (Prelude.Maybe AccessKey)
createBucketAccessKeyResponse_accessKey = Lens.lens (\CreateBucketAccessKeyResponse' {accessKey} -> accessKey) (\s@CreateBucketAccessKeyResponse' {} a -> s {accessKey = a} :: CreateBucketAccessKeyResponse)

-- | The response's http status code.
createBucketAccessKeyResponse_httpStatus :: Lens.Lens' CreateBucketAccessKeyResponse Prelude.Int
createBucketAccessKeyResponse_httpStatus = Lens.lens (\CreateBucketAccessKeyResponse' {httpStatus} -> httpStatus) (\s@CreateBucketAccessKeyResponse' {} a -> s {httpStatus = a} :: CreateBucketAccessKeyResponse)

instance Prelude.NFData CreateBucketAccessKeyResponse where
  rnf CreateBucketAccessKeyResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf accessKey
      `Prelude.seq` Prelude.rnf httpStatus
