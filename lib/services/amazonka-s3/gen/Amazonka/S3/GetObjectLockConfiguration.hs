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
-- Module      : Amazonka.S3.GetObjectLockConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Object Lock configuration for a bucket. The rule specified in
-- the Object Lock configuration will be applied by default to every new
-- object placed in the specified bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
--
-- The following action is related to @GetObjectLockConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAttributes.html GetObjectAttributes>
module Amazonka.S3.GetObjectLockConfiguration
  ( -- * Creating a Request
    GetObjectLockConfiguration (..),
    newGetObjectLockConfiguration,

    -- * Request Lenses
    getObjectLockConfiguration_expectedBucketOwner,
    getObjectLockConfiguration_bucket,

    -- * Destructuring the Response
    GetObjectLockConfigurationResponse (..),
    newGetObjectLockConfigurationResponse,

    -- * Response Lenses
    getObjectLockConfigurationResponse_objectLockConfiguration,
    getObjectLockConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObjectLockConfiguration' smart constructor.
data GetObjectLockConfiguration = GetObjectLockConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket whose Object Lock configuration you want to retrieve.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectLockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectLockConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getObjectLockConfiguration_bucket' - The bucket whose Object Lock configuration you want to retrieve.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
newGetObjectLockConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetObjectLockConfiguration
newGetObjectLockConfiguration pBucket_ =
  GetObjectLockConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getObjectLockConfiguration_expectedBucketOwner :: Lens.Lens' GetObjectLockConfiguration (Prelude.Maybe Prelude.Text)
getObjectLockConfiguration_expectedBucketOwner = Lens.lens (\GetObjectLockConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectLockConfiguration' {} a -> s {expectedBucketOwner = a} :: GetObjectLockConfiguration)

-- | The bucket whose Object Lock configuration you want to retrieve.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
getObjectLockConfiguration_bucket :: Lens.Lens' GetObjectLockConfiguration BucketName
getObjectLockConfiguration_bucket = Lens.lens (\GetObjectLockConfiguration' {bucket} -> bucket) (\s@GetObjectLockConfiguration' {} a -> s {bucket = a} :: GetObjectLockConfiguration)

instance Core.AWSRequest GetObjectLockConfiguration where
  type
    AWSResponse GetObjectLockConfiguration =
      GetObjectLockConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectLockConfigurationResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectLockConfiguration where
  hashWithSalt _salt GetObjectLockConfiguration' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetObjectLockConfiguration where
  rnf GetObjectLockConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetObjectLockConfiguration where
  toHeaders GetObjectLockConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetObjectLockConfiguration where
  toPath GetObjectLockConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetObjectLockConfiguration where
  toQuery =
    Prelude.const (Prelude.mconcat ["object-lock"])

-- | /See:/ 'newGetObjectLockConfigurationResponse' smart constructor.
data GetObjectLockConfigurationResponse = GetObjectLockConfigurationResponse'
  { -- | The specified bucket\'s Object Lock configuration.
    objectLockConfiguration :: Prelude.Maybe ObjectLockConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectLockConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectLockConfiguration', 'getObjectLockConfigurationResponse_objectLockConfiguration' - The specified bucket\'s Object Lock configuration.
--
-- 'httpStatus', 'getObjectLockConfigurationResponse_httpStatus' - The response's http status code.
newGetObjectLockConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetObjectLockConfigurationResponse
newGetObjectLockConfigurationResponse pHttpStatus_ =
  GetObjectLockConfigurationResponse'
    { objectLockConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The specified bucket\'s Object Lock configuration.
getObjectLockConfigurationResponse_objectLockConfiguration :: Lens.Lens' GetObjectLockConfigurationResponse (Prelude.Maybe ObjectLockConfiguration)
getObjectLockConfigurationResponse_objectLockConfiguration = Lens.lens (\GetObjectLockConfigurationResponse' {objectLockConfiguration} -> objectLockConfiguration) (\s@GetObjectLockConfigurationResponse' {} a -> s {objectLockConfiguration = a} :: GetObjectLockConfigurationResponse)

-- | The response's http status code.
getObjectLockConfigurationResponse_httpStatus :: Lens.Lens' GetObjectLockConfigurationResponse Prelude.Int
getObjectLockConfigurationResponse_httpStatus = Lens.lens (\GetObjectLockConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetObjectLockConfigurationResponse' {} a -> s {httpStatus = a} :: GetObjectLockConfigurationResponse)

instance
  Prelude.NFData
    GetObjectLockConfigurationResponse
  where
  rnf GetObjectLockConfigurationResponse' {..} =
    Prelude.rnf objectLockConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
