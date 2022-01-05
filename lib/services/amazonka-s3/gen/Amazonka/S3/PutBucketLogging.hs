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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerLogs.html Server Access Logging>.
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketLogging' smart constructor.
data PutBucketLogging = PutBucketLogging'
  { -- | The MD5 hash of the @PutBucketLogging@ request body.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
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
-- 'contentMD5', 'putBucketLogging_contentMD5' - The MD5 hash of the @PutBucketLogging@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketLogging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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
    { contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      bucketLoggingStatus = pBucketLoggingStatus_
    }

-- | The MD5 hash of the @PutBucketLogging@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketLogging_contentMD5 :: Lens.Lens' PutBucketLogging (Prelude.Maybe Prelude.Text)
putBucketLogging_contentMD5 = Lens.lens (\PutBucketLogging' {contentMD5} -> contentMD5) (\s@PutBucketLogging' {} a -> s {contentMD5 = a} :: PutBucketLogging)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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
  request =
    Request.s3vhost
      Prelude.. Request.putXML defaultService
  response =
    Response.receiveNull PutBucketLoggingResponse'

instance Prelude.Hashable PutBucketLogging where
  hashWithSalt _salt PutBucketLogging' {..} =
    _salt `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` bucketLoggingStatus

instance Prelude.NFData PutBucketLogging where
  rnf PutBucketLogging' {..} =
    Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf bucketLoggingStatus

instance Core.ToElement PutBucketLogging where
  toElement PutBucketLogging' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}BucketLoggingStatus"
      bucketLoggingStatus

instance Core.ToHeaders PutBucketLogging where
  toHeaders PutBucketLogging' {..} =
    Prelude.mconcat
      [ "Content-MD5" Core.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath PutBucketLogging where
  toPath PutBucketLogging' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery PutBucketLogging where
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
