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
-- Module      : Amazonka.S3.PutObjectLockConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Lock configuration on the specified bucket. The rule
-- specified in the Object Lock configuration will be applied by default to
-- every new object placed in the specified bucket. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
--
-- -   The @DefaultRetention@ settings require both a mode and a period.
--
-- -   The @DefaultRetention@ period can be either @Days@ or @Years@ but
--     you must select one. You cannot specify @Days@ and @Years@ at the
--     same time.
--
-- -   You can only enable Object Lock for new buckets. If you want to turn
--     on Object Lock for an existing bucket, contact Amazon Web Services
--     Support.
module Amazonka.S3.PutObjectLockConfiguration
  ( -- * Creating a Request
    PutObjectLockConfiguration (..),
    newPutObjectLockConfiguration,

    -- * Request Lenses
    putObjectLockConfiguration_checksumAlgorithm,
    putObjectLockConfiguration_contentMD5,
    putObjectLockConfiguration_expectedBucketOwner,
    putObjectLockConfiguration_objectLockConfiguration,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutObjectLockConfiguration' smart constructor.
data PutObjectLockConfiguration = PutObjectLockConfiguration'
  { -- | Indicates the algorithm used to create the checksum for the object when
    -- using the SDK. This header will not provide any additional functionality
    -- if not using the SDK. When sending this header, there must be a
    -- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
    -- Otherwise, Amazon S3 fails the request with the HTTP status code
    -- @400 Bad Request@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    --
    -- If you provide an individual checksum, Amazon S3 ignores any provided
    -- @ChecksumAlgorithm@ parameter.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The Object Lock configuration that you want to apply to the specified
    -- bucket.
    objectLockConfiguration :: Prelude.Maybe ObjectLockConfiguration,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | A token to allow Object Lock to be enabled for an existing bucket.
    token :: Prelude.Maybe Prelude.Text,
    -- | The bucket whose Object Lock configuration you want to create or
    -- replace.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectLockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putObjectLockConfiguration_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
--
-- 'contentMD5', 'putObjectLockConfiguration_contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putObjectLockConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'objectLockConfiguration', 'putObjectLockConfiguration_objectLockConfiguration' - The Object Lock configuration that you want to apply to the specified
-- bucket.
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
    { checksumAlgorithm =
        Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      objectLockConfiguration = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      token = Prelude.Nothing,
      bucket = pBucket_
    }

-- | Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
putObjectLockConfiguration_checksumAlgorithm :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe ChecksumAlgorithm)
putObjectLockConfiguration_checksumAlgorithm = Lens.lens (\PutObjectLockConfiguration' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutObjectLockConfiguration' {} a -> s {checksumAlgorithm = a} :: PutObjectLockConfiguration)

-- | The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putObjectLockConfiguration_contentMD5 :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe Prelude.Text)
putObjectLockConfiguration_contentMD5 = Lens.lens (\PutObjectLockConfiguration' {contentMD5} -> contentMD5) (\s@PutObjectLockConfiguration' {} a -> s {contentMD5 = a} :: PutObjectLockConfiguration)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putObjectLockConfiguration_expectedBucketOwner :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe Prelude.Text)
putObjectLockConfiguration_expectedBucketOwner = Lens.lens (\PutObjectLockConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectLockConfiguration' {} a -> s {expectedBucketOwner = a} :: PutObjectLockConfiguration)

-- | The Object Lock configuration that you want to apply to the specified
-- bucket.
putObjectLockConfiguration_objectLockConfiguration :: Lens.Lens' PutObjectLockConfiguration (Prelude.Maybe ObjectLockConfiguration)
putObjectLockConfiguration_objectLockConfiguration = Lens.lens (\PutObjectLockConfiguration' {objectLockConfiguration} -> objectLockConfiguration) (\s@PutObjectLockConfiguration' {} a -> s {objectLockConfiguration = a} :: PutObjectLockConfiguration)

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

instance Core.AWSRequest PutObjectLockConfiguration where
  type
    AWSResponse PutObjectLockConfiguration =
      PutObjectLockConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectLockConfigurationResponse'
            Prelude.<$> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectLockConfiguration where
  hashWithSalt _salt PutObjectLockConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` objectLockConfiguration
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData PutObjectLockConfiguration where
  rnf PutObjectLockConfiguration' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf objectLockConfiguration
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToElement PutObjectLockConfiguration where
  toElement PutObjectLockConfiguration' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ObjectLockConfiguration"
      objectLockConfiguration

instance Data.ToHeaders PutObjectLockConfiguration where
  toHeaders PutObjectLockConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer,
        "x-amz-bucket-object-lock-token" Data.=# token
      ]

instance Data.ToPath PutObjectLockConfiguration where
  toPath PutObjectLockConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutObjectLockConfiguration where
  toQuery =
    Prelude.const (Prelude.mconcat ["object-lock"])

-- | /See:/ 'newPutObjectLockConfigurationResponse' smart constructor.
data PutObjectLockConfigurationResponse = PutObjectLockConfigurationResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf PutObjectLockConfigurationResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf httpStatus
