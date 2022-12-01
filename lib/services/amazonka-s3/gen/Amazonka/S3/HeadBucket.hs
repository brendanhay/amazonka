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
-- Module      : Amazonka.S3.HeadBucket
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is useful to determine if a bucket exists and you have
-- permission to access it. The action returns a @200 OK@ if the bucket
-- exists and you have permission to access it.
--
-- If the bucket does not exist or you do not have permission to access it,
-- the @HEAD@ request returns a generic @404 Not Found@ or @403 Forbidden@
-- code. A message body is not included, so you cannot determine the
-- exception beyond these error codes.
--
-- To use this operation, you must have permissions to perform the
-- @s3:ListBucket@ action. The bucket owner has this permission by default
-- and can grant this permission to others. For more information about
-- permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- To use this API against an access point, you must provide the alias of
-- the access point in place of the bucket name or specify the access point
-- ARN. When using the access point ARN, you must direct requests to the
-- access point hostname. The access point hostname takes the form
-- AccessPointName-AccountId.s3-accesspoint.Region.amazonaws.com. When
-- using the Amazon Web Services SDKs, you provide the ARN in place of the
-- bucket name. For more information see,
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>.
module Amazonka.S3.HeadBucket
  ( -- * Creating a Request
    HeadBucket (..),
    newHeadBucket,

    -- * Request Lenses
    headBucket_expectedBucketOwner,
    headBucket_bucket,

    -- * Destructuring the Response
    HeadBucketResponse (..),
    newHeadBucketResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newHeadBucket' smart constructor.
data HeadBucket = HeadBucket'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- When using this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeadBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'headBucket_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'headBucket_bucket' - The bucket name.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
newHeadBucket ::
  -- | 'bucket'
  BucketName ->
  HeadBucket
newHeadBucket pBucket_ =
  HeadBucket'
    { expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
headBucket_expectedBucketOwner :: Lens.Lens' HeadBucket (Prelude.Maybe Prelude.Text)
headBucket_expectedBucketOwner = Lens.lens (\HeadBucket' {expectedBucketOwner} -> expectedBucketOwner) (\s@HeadBucket' {} a -> s {expectedBucketOwner = a} :: HeadBucket)

-- | The bucket name.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
headBucket_bucket :: Lens.Lens' HeadBucket BucketName
headBucket_bucket = Lens.lens (\HeadBucket' {bucket} -> bucket) (\s@HeadBucket' {} a -> s {bucket = a} :: HeadBucket)

instance Core.AWSRequest HeadBucket where
  type AWSResponse HeadBucket = HeadBucketResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.head' (overrides defaultService)
  response = Response.receiveNull HeadBucketResponse'

instance Prelude.Hashable HeadBucket where
  hashWithSalt _salt HeadBucket' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData HeadBucket where
  rnf HeadBucket' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Core.ToHeaders HeadBucket where
  toHeaders HeadBucket' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath HeadBucket where
  toPath HeadBucket' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery HeadBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newHeadBucketResponse' smart constructor.
data HeadBucketResponse = HeadBucketResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeadBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newHeadBucketResponse ::
  HeadBucketResponse
newHeadBucketResponse = HeadBucketResponse'

instance Prelude.NFData HeadBucketResponse where
  rnf _ = ()
