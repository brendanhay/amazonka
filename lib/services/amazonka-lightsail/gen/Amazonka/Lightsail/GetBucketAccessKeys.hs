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
-- Module      : Amazonka.Lightsail.GetBucketAccessKeys
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the existing access key IDs for the specified Amazon Lightsail
-- bucket.
--
-- This action does not return the secret access key value of an access
-- key. You can get a secret access key only when you create it from the
-- response of the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_CreateBucketAccessKey.html CreateBucketAccessKey>
-- action. If you lose the secret access key, you must create a new access
-- key.
module Amazonka.Lightsail.GetBucketAccessKeys
  ( -- * Creating a Request
    GetBucketAccessKeys (..),
    newGetBucketAccessKeys,

    -- * Request Lenses
    getBucketAccessKeys_bucketName,

    -- * Destructuring the Response
    GetBucketAccessKeysResponse (..),
    newGetBucketAccessKeysResponse,

    -- * Response Lenses
    getBucketAccessKeysResponse_accessKeys,
    getBucketAccessKeysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBucketAccessKeys' smart constructor.
data GetBucketAccessKeys = GetBucketAccessKeys'
  { -- | The name of the bucket for which to return access keys.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAccessKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'getBucketAccessKeys_bucketName' - The name of the bucket for which to return access keys.
newGetBucketAccessKeys ::
  -- | 'bucketName'
  Prelude.Text ->
  GetBucketAccessKeys
newGetBucketAccessKeys pBucketName_ =
  GetBucketAccessKeys' {bucketName = pBucketName_}

-- | The name of the bucket for which to return access keys.
getBucketAccessKeys_bucketName :: Lens.Lens' GetBucketAccessKeys Prelude.Text
getBucketAccessKeys_bucketName = Lens.lens (\GetBucketAccessKeys' {bucketName} -> bucketName) (\s@GetBucketAccessKeys' {} a -> s {bucketName = a} :: GetBucketAccessKeys)

instance Core.AWSRequest GetBucketAccessKeys where
  type
    AWSResponse GetBucketAccessKeys =
      GetBucketAccessKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBucketAccessKeysResponse'
            Prelude.<$> (x Core..?> "accessKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketAccessKeys where
  hashWithSalt _salt GetBucketAccessKeys' {..} =
    _salt `Prelude.hashWithSalt` bucketName

instance Prelude.NFData GetBucketAccessKeys where
  rnf GetBucketAccessKeys' {..} = Prelude.rnf bucketName

instance Core.ToHeaders GetBucketAccessKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetBucketAccessKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBucketAccessKeys where
  toJSON GetBucketAccessKeys' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("bucketName" Core..= bucketName)]
      )

instance Core.ToPath GetBucketAccessKeys where
  toPath = Prelude.const "/"

instance Core.ToQuery GetBucketAccessKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBucketAccessKeysResponse' smart constructor.
data GetBucketAccessKeysResponse = GetBucketAccessKeysResponse'
  { -- | An object that describes the access keys for the specified bucket.
    accessKeys :: Prelude.Maybe [AccessKey],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAccessKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeys', 'getBucketAccessKeysResponse_accessKeys' - An object that describes the access keys for the specified bucket.
--
-- 'httpStatus', 'getBucketAccessKeysResponse_httpStatus' - The response's http status code.
newGetBucketAccessKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketAccessKeysResponse
newGetBucketAccessKeysResponse pHttpStatus_ =
  GetBucketAccessKeysResponse'
    { accessKeys =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the access keys for the specified bucket.
getBucketAccessKeysResponse_accessKeys :: Lens.Lens' GetBucketAccessKeysResponse (Prelude.Maybe [AccessKey])
getBucketAccessKeysResponse_accessKeys = Lens.lens (\GetBucketAccessKeysResponse' {accessKeys} -> accessKeys) (\s@GetBucketAccessKeysResponse' {} a -> s {accessKeys = a} :: GetBucketAccessKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getBucketAccessKeysResponse_httpStatus :: Lens.Lens' GetBucketAccessKeysResponse Prelude.Int
getBucketAccessKeysResponse_httpStatus = Lens.lens (\GetBucketAccessKeysResponse' {httpStatus} -> httpStatus) (\s@GetBucketAccessKeysResponse' {} a -> s {httpStatus = a} :: GetBucketAccessKeysResponse)

instance Prelude.NFData GetBucketAccessKeysResponse where
  rnf GetBucketAccessKeysResponse' {..} =
    Prelude.rnf accessKeys
      `Prelude.seq` Prelude.rnf httpStatus
