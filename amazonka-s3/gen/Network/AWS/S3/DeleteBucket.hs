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
-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 bucket. All objects (including all object versions and
-- delete markers) in the bucket must be deleted before the bucket itself
-- can be deleted.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.DeleteBucket
  ( -- * Creating a Request
    DeleteBucket (..),
    newDeleteBucket,

    -- * Request Lenses
    deleteBucket_expectedBucketOwner,
    deleteBucket_bucket,

    -- * Destructuring the Response
    DeleteBucketResponse (..),
    newDeleteBucketResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucket' smart constructor.
data DeleteBucket = DeleteBucket'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | Specifies the bucket being deleted.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucket_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucket_bucket' - Specifies the bucket being deleted.
newDeleteBucket ::
  -- | 'bucket'
  BucketName ->
  DeleteBucket
newDeleteBucket pBucket_ =
  DeleteBucket'
    { expectedBucketOwner = Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucket_expectedBucketOwner :: Lens.Lens' DeleteBucket (Core.Maybe Core.Text)
deleteBucket_expectedBucketOwner = Lens.lens (\DeleteBucket' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucket' {} a -> s {expectedBucketOwner = a} :: DeleteBucket)

-- | Specifies the bucket being deleted.
deleteBucket_bucket :: Lens.Lens' DeleteBucket BucketName
deleteBucket_bucket = Lens.lens (\DeleteBucket' {bucket} -> bucket) (\s@DeleteBucket' {} a -> s {bucket = a} :: DeleteBucket)

instance Core.AWSRequest DeleteBucket where
  type AWSResponse DeleteBucket = DeleteBucketResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteBucketResponse'

instance Core.Hashable DeleteBucket

instance Core.NFData DeleteBucket

instance Core.ToHeaders DeleteBucket where
  toHeaders DeleteBucket' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath DeleteBucket where
  toPath DeleteBucket' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery DeleteBucket where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteBucketResponse' smart constructor.
data DeleteBucketResponse = DeleteBucketResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketResponse ::
  DeleteBucketResponse
newDeleteBucketResponse = DeleteBucketResponse'

instance Core.NFData DeleteBucketResponse
