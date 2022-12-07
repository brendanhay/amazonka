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
-- Module      : Amazonka.S3.PutBucketLogging
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the logging parameters for a bucket and to specify permissions for
-- who can view and modify the logging parameters. All logs are saved to
-- buckets in the same Amazon Web Services Region as the source bucket. To
-- set the logging status of a bucket, you must be the bucket owner.
--
-- The bucket owner is automatically granted FULL_CONTROL to all logs. You
-- use the @Grantee@ request element to grant access to other people. The
-- @Permissions@ request element specifies the kind of access the grantee
-- has to the logs.
--
-- If the target bucket for log delivery uses the bucket owner enforced
-- setting for S3 Object Ownership, you can\'t use the @Grantee@ request
-- element to grant access to others. Permissions can only be granted using
-- policies. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/enable-server-access-logging.html#grant-log-delivery-permissions-general Permissions for server access log delivery>
-- in the /Amazon S3 User Guide/.
--
-- __Grantee Values__
--
-- You can specify the person (grantee) to whom you\'re assigning access
-- rights (using request elements) in the following ways:
--
-- -   By the person\'s ID:
--
--     @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"CanonicalUser\">\<ID>\<>ID\<>\<\/ID>\<DisplayName>\<>GranteesEmail\<>\<\/DisplayName> \<\/Grantee>@
--
--     DisplayName is optional and ignored in the request.
--
-- -   By Email address:
--
--     @ \<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"AmazonCustomerByEmail\">\<EmailAddress>\<>Grantees\@email.com\<>\<\/EmailAddress>\<\/Grantee>@
--
--     The grantee is resolved to the CanonicalUser and, in a response to a
--     GET Object acl request, appears as the CanonicalUser.
--
-- -   By URI:
--
--     @\<Grantee xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xsi:type=\"Group\">\<URI>\<>http:\/\/acs.amazonaws.com\/groups\/global\/AuthenticatedUsers\<>\<\/URI>\<\/Grantee>@
--
-- To enable logging, you use LoggingEnabled and its children request
-- elements. To disable logging, you use an empty BucketLoggingStatus
-- request element:
--
-- @\<BucketLoggingStatus xmlns=\"http:\/\/doc.s3.amazonaws.com\/2006-03-01\" \/>@
--
-- For more information about server access logging, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/ServerLogs.html Server Access Logging>
-- in the /Amazon S3 User Guide/.
--
-- For more information about creating a bucket, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>.
-- For more information about returning the logging status of a bucket, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLogging.html GetBucketLogging>.
--
-- The following operations are related to @PutBucketLogging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLogging.html GetBucketLogging>
module Amazonka.S3.PutBucketLogging
  ( -- * Creating a Request
    PutBucketLogging (..),
    newPutBucketLogging,

    -- * Request Lenses
    putBucketLogging_checksumAlgorithm,
    putBucketLogging_contentMD5,
    putBucketLogging_expectedBucketOwner,
    putBucketLogging_bucket,
    putBucketLogging_bucketLoggingStatus,

    -- * Destructuring the Response
    PutBucketLoggingResponse (..),
    newPutBucketLoggingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketLogging' smart constructor.
data PutBucketLogging = PutBucketLogging'
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
    -- | The MD5 hash of the @PutBucketLogging@ request body.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which to set the logging parameters.
    bucket :: BucketName,
    -- | Container for logging status information.
    bucketLoggingStatus :: BucketLoggingStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketLogging_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putBucketLogging_contentMD5' - The MD5 hash of the @PutBucketLogging@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketLogging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'putBucketLogging_bucket' - The name of the bucket for which to set the logging parameters.
--
-- 'bucketLoggingStatus', 'putBucketLogging_bucketLoggingStatus' - Container for logging status information.
newPutBucketLogging ::
  -- | 'bucket'
  BucketName ->
  -- | 'bucketLoggingStatus'
  BucketLoggingStatus ->
  PutBucketLogging
newPutBucketLogging pBucket_ pBucketLoggingStatus_ =
  PutBucketLogging'
    { checksumAlgorithm =
        Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      bucketLoggingStatus = pBucketLoggingStatus_
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
putBucketLogging_checksumAlgorithm :: Lens.Lens' PutBucketLogging (Prelude.Maybe ChecksumAlgorithm)
putBucketLogging_checksumAlgorithm = Lens.lens (\PutBucketLogging' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketLogging' {} a -> s {checksumAlgorithm = a} :: PutBucketLogging)

-- | The MD5 hash of the @PutBucketLogging@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketLogging_contentMD5 :: Lens.Lens' PutBucketLogging (Prelude.Maybe Prelude.Text)
putBucketLogging_contentMD5 = Lens.lens (\PutBucketLogging' {contentMD5} -> contentMD5) (\s@PutBucketLogging' {} a -> s {contentMD5 = a} :: PutBucketLogging)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketLogging_expectedBucketOwner :: Lens.Lens' PutBucketLogging (Prelude.Maybe Prelude.Text)
putBucketLogging_expectedBucketOwner = Lens.lens (\PutBucketLogging' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketLogging' {} a -> s {expectedBucketOwner = a} :: PutBucketLogging)

-- | The name of the bucket for which to set the logging parameters.
putBucketLogging_bucket :: Lens.Lens' PutBucketLogging BucketName
putBucketLogging_bucket = Lens.lens (\PutBucketLogging' {bucket} -> bucket) (\s@PutBucketLogging' {} a -> s {bucket = a} :: PutBucketLogging)

-- | Container for logging status information.
putBucketLogging_bucketLoggingStatus :: Lens.Lens' PutBucketLogging BucketLoggingStatus
putBucketLogging_bucketLoggingStatus = Lens.lens (\PutBucketLogging' {bucketLoggingStatus} -> bucketLoggingStatus) (\s@PutBucketLogging' {} a -> s {bucketLoggingStatus = a} :: PutBucketLogging)

instance Core.AWSRequest PutBucketLogging where
  type
    AWSResponse PutBucketLogging =
      PutBucketLoggingResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull PutBucketLoggingResponse'

instance Prelude.Hashable PutBucketLogging where
  hashWithSalt _salt PutBucketLogging' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` bucketLoggingStatus

instance Prelude.NFData PutBucketLogging where
  rnf PutBucketLogging' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf bucketLoggingStatus

instance Data.ToElement PutBucketLogging where
  toElement PutBucketLogging' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}BucketLoggingStatus"
      bucketLoggingStatus

instance Data.ToHeaders PutBucketLogging where
  toHeaders PutBucketLogging' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketLogging where
  toPath PutBucketLogging' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketLogging where
  toQuery = Prelude.const (Prelude.mconcat ["logging"])

-- | /See:/ 'newPutBucketLoggingResponse' smart constructor.
data PutBucketLoggingResponse = PutBucketLoggingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketLoggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketLoggingResponse ::
  PutBucketLoggingResponse
newPutBucketLoggingResponse =
  PutBucketLoggingResponse'

instance Prelude.NFData PutBucketLoggingResponse where
  rnf _ = ()
