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
-- Module      : Amazonka.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the versioning state of an existing bucket.
--
-- You can set the versioning state with one of the following values:
--
-- __Enabled__—Enables versioning for the objects in the bucket. All
-- objects added to the bucket receive a unique version ID.
--
-- __Suspended__—Disables versioning for the objects in the bucket. All
-- objects added to the bucket receive the version ID null.
--
-- If the versioning state has never been set on a bucket, it has no
-- versioning state; a
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning>
-- request does not return a versioning state value.
--
-- In order to enable MFA Delete, you must be the bucket owner. If you are
-- the bucket owner and want to enable MFA Delete in the bucket versioning
-- configuration, you must include the @x-amz-mfa request@ header and the
-- @Status@ and the @MfaDelete@ request elements in a request to set the
-- versioning state of the bucket.
--
-- If you have an object expiration lifecycle policy in your non-versioned
-- bucket and you want to maintain the same permanent delete behavior when
-- you enable versioning, you must add a noncurrent expiration policy. The
-- noncurrent expiration lifecycle policy will manage the deletes of the
-- noncurrent object versions in the version-enabled bucket. (A
-- version-enabled bucket maintains one current and zero or more noncurrent
-- object versions.) For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-and-other-bucket-config Lifecycle and Versioning>.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning>
module Amazonka.S3.PutBucketVersioning
  ( -- * Creating a Request
    PutBucketVersioning (..),
    newPutBucketVersioning,

    -- * Request Lenses
    putBucketVersioning_checksumAlgorithm,
    putBucketVersioning_contentMD5,
    putBucketVersioning_expectedBucketOwner,
    putBucketVersioning_mfa,
    putBucketVersioning_bucket,
    putBucketVersioning_versioningConfiguration,

    -- * Destructuring the Response
    PutBucketVersioningResponse (..),
    newPutBucketVersioningResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketVersioning' smart constructor.
data PutBucketVersioning = PutBucketVersioning'
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
    -- | >The base64-encoded 128-bit MD5 digest of the data. You must use this
    -- header as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, see
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The concatenation of the authentication device\'s serial number, a
    -- space, and the value that is displayed on your authentication device.
    mfa :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    bucket :: BucketName,
    -- | Container for setting the versioning state.
    versioningConfiguration :: VersioningConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketVersioning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketVersioning_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putBucketVersioning_contentMD5' - >The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketVersioning_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'mfa', 'putBucketVersioning_mfa' - The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
--
-- 'bucket', 'putBucketVersioning_bucket' - The bucket name.
--
-- 'versioningConfiguration', 'putBucketVersioning_versioningConfiguration' - Container for setting the versioning state.
newPutBucketVersioning ::
  -- | 'bucket'
  BucketName ->
  -- | 'versioningConfiguration'
  VersioningConfiguration ->
  PutBucketVersioning
newPutBucketVersioning
  pBucket_
  pVersioningConfiguration_ =
    PutBucketVersioning'
      { checksumAlgorithm =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        mfa = Prelude.Nothing,
        bucket = pBucket_,
        versioningConfiguration = pVersioningConfiguration_
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
putBucketVersioning_checksumAlgorithm :: Lens.Lens' PutBucketVersioning (Prelude.Maybe ChecksumAlgorithm)
putBucketVersioning_checksumAlgorithm = Lens.lens (\PutBucketVersioning' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketVersioning' {} a -> s {checksumAlgorithm = a} :: PutBucketVersioning)

-- | >The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketVersioning_contentMD5 :: Lens.Lens' PutBucketVersioning (Prelude.Maybe Prelude.Text)
putBucketVersioning_contentMD5 = Lens.lens (\PutBucketVersioning' {contentMD5} -> contentMD5) (\s@PutBucketVersioning' {} a -> s {contentMD5 = a} :: PutBucketVersioning)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketVersioning_expectedBucketOwner :: Lens.Lens' PutBucketVersioning (Prelude.Maybe Prelude.Text)
putBucketVersioning_expectedBucketOwner = Lens.lens (\PutBucketVersioning' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketVersioning' {} a -> s {expectedBucketOwner = a} :: PutBucketVersioning)

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
putBucketVersioning_mfa :: Lens.Lens' PutBucketVersioning (Prelude.Maybe Prelude.Text)
putBucketVersioning_mfa = Lens.lens (\PutBucketVersioning' {mfa} -> mfa) (\s@PutBucketVersioning' {} a -> s {mfa = a} :: PutBucketVersioning)

-- | The bucket name.
putBucketVersioning_bucket :: Lens.Lens' PutBucketVersioning BucketName
putBucketVersioning_bucket = Lens.lens (\PutBucketVersioning' {bucket} -> bucket) (\s@PutBucketVersioning' {} a -> s {bucket = a} :: PutBucketVersioning)

-- | Container for setting the versioning state.
putBucketVersioning_versioningConfiguration :: Lens.Lens' PutBucketVersioning VersioningConfiguration
putBucketVersioning_versioningConfiguration = Lens.lens (\PutBucketVersioning' {versioningConfiguration} -> versioningConfiguration) (\s@PutBucketVersioning' {} a -> s {versioningConfiguration = a} :: PutBucketVersioning)

instance Core.AWSRequest PutBucketVersioning where
  type
    AWSResponse PutBucketVersioning =
      PutBucketVersioningResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull PutBucketVersioningResponse'

instance Prelude.Hashable PutBucketVersioning where
  hashWithSalt _salt PutBucketVersioning' {..} =
    _salt
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` mfa
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` versioningConfiguration

instance Prelude.NFData PutBucketVersioning where
  rnf PutBucketVersioning' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf mfa
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf versioningConfiguration

instance Data.ToElement PutBucketVersioning where
  toElement PutBucketVersioning' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}VersioningConfiguration"
      versioningConfiguration

instance Data.ToHeaders PutBucketVersioning where
  toHeaders PutBucketVersioning' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-mfa" Data.=# mfa
      ]

instance Data.ToPath PutBucketVersioning where
  toPath PutBucketVersioning' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketVersioning where
  toQuery =
    Prelude.const (Prelude.mconcat ["versioning"])

-- | /See:/ 'newPutBucketVersioningResponse' smart constructor.
data PutBucketVersioningResponse = PutBucketVersioningResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketVersioningResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketVersioningResponse ::
  PutBucketVersioningResponse
newPutBucketVersioningResponse =
  PutBucketVersioningResponse'

instance Prelude.NFData PutBucketVersioningResponse where
  rnf _ = ()
