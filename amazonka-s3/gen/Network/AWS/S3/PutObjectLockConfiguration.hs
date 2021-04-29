{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.PutObjectLockConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Lock configuration on the specified bucket. The rule
-- specified in the Object Lock configuration will be applied by default to
-- every new object placed in the specified bucket.
--
-- @DefaultRetention@ requires either Days or Years. You can\'t specify
-- both at the same time.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>
module Network.AWS.S3.PutObjectLockConfiguration
  ( -- * Creating a Request
    PutObjectLockConfiguration (..),
    newPutObjectLockConfiguration,

    -- * Request Lenses
    putObjectLockConfiguration_expectedBucketOwner,
    putObjectLockConfiguration_objectLockConfiguration,
    putObjectLockConfiguration_contentMD5,
    putObjectLockConfiguration_requestPayer,
    putObjectLockConfiguration_token,
    putObjectLockConfiguration_bucket,

    -- * Destructuring the Response
    PutObjectLockConfigurationResponse (..),
    newPutObjectLockConfigurationResponse,

    -- * Response Lenses
    putObjectLockConfigurationResponse_requestCharged,
    putObjectLockConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutObjectLockConfiguration' smart constructor.
data PutObjectLockConfiguration = PutObjectLockConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The Object Lock configuration that you want to apply to the specified
    -- bucket.
    objectLockConfiguration :: Prelude.Maybe ObjectLockConfiguration,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | A token to allow Object Lock to be enabled for an existing bucket.
    token :: Prelude.Maybe Prelude.Text,
    -- | The bucket whose Object Lock configuration you want to create or
    -- replace.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutObjectLockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putObjectLockConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'objectLockConfiguration', 'putObjectLockConfiguration_objectLockConfiguration' - The Object Lock configuration that you want to apply to the specified
-- bucket.
--
-- 'contentMD5', 'putObjectLockConfiguration_contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'requestPayer', 'putObjectLockConfiguration_requestPayer' - Undocumented member.
--
-- 'token', 'putObjectLockConfiguration_token' - A token to allow Object Lock to be enabled for an existing bucket.
--
-- 'bucket', 'putObjectLockConfiguration_bucket' - The bucket whose Object Lock configuration you want to create or
-- replace.
newPutObjectLockConfiguration ::
  -- | 'bucket'
  BucketName ->
  PutObjectLockConfiguration
newPutObjectLockConfiguration pBucket_ =
  PutObjectLockConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      objectLockConfiguration = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      token = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putObjectLockConfiguration_expectedBucketOwner :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe Prelude.Text)
putObjectLockConfiguration_expectedBucketOwner = Lens.lens (\PutObjectLockConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectLockConfiguration' {} a -> s {expectedBucketOwner = a} :: PutObjectLockConfiguration)

-- | The Object Lock configuration that you want to apply to the specified
-- bucket.
putObjectLockConfiguration_objectLockConfiguration :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe ObjectLockConfiguration)
putObjectLockConfiguration_objectLockConfiguration = Lens.lens (\PutObjectLockConfiguration' {objectLockConfiguration} -> objectLockConfiguration) (\s@PutObjectLockConfiguration' {} a -> s {objectLockConfiguration = a} :: PutObjectLockConfiguration)

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putObjectLockConfiguration_contentMD5 :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe Prelude.Text)
putObjectLockConfiguration_contentMD5 = Lens.lens (\PutObjectLockConfiguration' {contentMD5} -> contentMD5) (\s@PutObjectLockConfiguration' {} a -> s {contentMD5 = a} :: PutObjectLockConfiguration)

-- | Undocumented member.
putObjectLockConfiguration_requestPayer :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe RequestPayer)
putObjectLockConfiguration_requestPayer = Lens.lens (\PutObjectLockConfiguration' {requestPayer} -> requestPayer) (\s@PutObjectLockConfiguration' {} a -> s {requestPayer = a} :: PutObjectLockConfiguration)

-- | A token to allow Object Lock to be enabled for an existing bucket.
putObjectLockConfiguration_token :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe Prelude.Text)
putObjectLockConfiguration_token = Lens.lens (\PutObjectLockConfiguration' {token} -> token) (\s@PutObjectLockConfiguration' {} a -> s {token = a} :: PutObjectLockConfiguration)

-- | The bucket whose Object Lock configuration you want to create or
-- replace.
putObjectLockConfiguration_bucket :: Lens.Lens' PutObjectLockConfiguration BucketName
putObjectLockConfiguration_bucket = Lens.lens (\PutObjectLockConfiguration' {bucket} -> bucket) (\s@PutObjectLockConfiguration' {} a -> s {bucket = a} :: PutObjectLockConfiguration)

instance
  Prelude.AWSRequest
    PutObjectLockConfiguration
  where
  type
    Rs PutObjectLockConfiguration =
      PutObjectLockConfigurationResponse
  request = Request.putXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectLockConfigurationResponse'
            Prelude.<$> (h Prelude..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectLockConfiguration

instance Prelude.NFData PutObjectLockConfiguration

instance Prelude.ToElement PutObjectLockConfiguration where
  toElement PutObjectLockConfiguration' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ObjectLockConfiguration"
      objectLockConfiguration

instance Prelude.ToHeaders PutObjectLockConfiguration where
  toHeaders PutObjectLockConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5,
        "x-amz-request-payer" Prelude.=# requestPayer,
        "x-amz-bucket-object-lock-token" Prelude.=# token
      ]

instance Prelude.ToPath PutObjectLockConfiguration where
  toPath PutObjectLockConfiguration' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery PutObjectLockConfiguration where
  toQuery =
    Prelude.const (Prelude.mconcat ["object-lock"])

-- | /See:/ 'newPutObjectLockConfigurationResponse' smart constructor.
data PutObjectLockConfigurationResponse = PutObjectLockConfigurationResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutObjectLockConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'putObjectLockConfigurationResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'putObjectLockConfigurationResponse_httpStatus' - The response's http status code.
newPutObjectLockConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutObjectLockConfigurationResponse
newPutObjectLockConfigurationResponse pHttpStatus_ =
  PutObjectLockConfigurationResponse'
    { requestCharged =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putObjectLockConfigurationResponse_requestCharged :: Lens.Lens' PutObjectLockConfigurationResponse (Prelude.Maybe RequestCharged)
putObjectLockConfigurationResponse_requestCharged = Lens.lens (\PutObjectLockConfigurationResponse' {requestCharged} -> requestCharged) (\s@PutObjectLockConfigurationResponse' {} a -> s {requestCharged = a} :: PutObjectLockConfigurationResponse)

-- | The response's http status code.
putObjectLockConfigurationResponse_httpStatus :: Lens.Lens' PutObjectLockConfigurationResponse Prelude.Int
putObjectLockConfigurationResponse_httpStatus = Lens.lens (\PutObjectLockConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutObjectLockConfigurationResponse' {} a -> s {httpStatus = a} :: PutObjectLockConfigurationResponse)

instance
  Prelude.NFData
    PutObjectLockConfigurationResponse
