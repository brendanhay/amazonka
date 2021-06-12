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
-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag set associated with the bucket.
--
-- To use this operation, you must have permission to perform the
-- @s3:GetBucketTagging@ action. By default, the bucket owner has this
-- permission and can grant this permission to others.
--
-- @GetBucketTagging@ has the following special error:
--
-- -   Error code: @NoSuchTagSetError@
--
--     -   Description: There is no tag set associated with the bucket.
--
-- The following operations are related to @GetBucketTagging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging>
module Network.AWS.S3.GetBucketTagging
  ( -- * Creating a Request
    GetBucketTagging (..),
    newGetBucketTagging,

    -- * Request Lenses
    getBucketTagging_expectedBucketOwner,
    getBucketTagging_bucket,

    -- * Destructuring the Response
    GetBucketTaggingResponse (..),
    newGetBucketTaggingResponse,

    -- * Response Lenses
    getBucketTaggingResponse_httpStatus,
    getBucketTaggingResponse_tagSet,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketTagging' smart constructor.
data GetBucketTagging = GetBucketTagging'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The name of the bucket for which to get the tagging information.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketTagging_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketTagging_bucket' - The name of the bucket for which to get the tagging information.
newGetBucketTagging ::
  -- | 'bucket'
  BucketName ->
  GetBucketTagging
newGetBucketTagging pBucket_ =
  GetBucketTagging'
    { expectedBucketOwner =
        Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketTagging_expectedBucketOwner :: Lens.Lens' GetBucketTagging (Core.Maybe Core.Text)
getBucketTagging_expectedBucketOwner = Lens.lens (\GetBucketTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketTagging' {} a -> s {expectedBucketOwner = a} :: GetBucketTagging)

-- | The name of the bucket for which to get the tagging information.
getBucketTagging_bucket :: Lens.Lens' GetBucketTagging BucketName
getBucketTagging_bucket = Lens.lens (\GetBucketTagging' {bucket} -> bucket) (\s@GetBucketTagging' {} a -> s {bucket = a} :: GetBucketTagging)

instance Core.AWSRequest GetBucketTagging where
  type
    AWSResponse GetBucketTagging =
      GetBucketTaggingResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketTaggingResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "TagSet" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "Tag"
                     )
      )

instance Core.Hashable GetBucketTagging

instance Core.NFData GetBucketTagging

instance Core.ToHeaders GetBucketTagging where
  toHeaders GetBucketTagging' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketTagging where
  toPath GetBucketTagging' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketTagging where
  toQuery = Core.const (Core.mconcat ["tagging"])

-- | /See:/ 'newGetBucketTaggingResponse' smart constructor.
data GetBucketTaggingResponse = GetBucketTaggingResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Contains the tag set.
    tagSet :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBucketTaggingResponse_httpStatus' - The response's http status code.
--
-- 'tagSet', 'getBucketTaggingResponse_tagSet' - Contains the tag set.
newGetBucketTaggingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBucketTaggingResponse
newGetBucketTaggingResponse pHttpStatus_ =
  GetBucketTaggingResponse'
    { httpStatus =
        pHttpStatus_,
      tagSet = Core.mempty
    }

-- | The response's http status code.
getBucketTaggingResponse_httpStatus :: Lens.Lens' GetBucketTaggingResponse Core.Int
getBucketTaggingResponse_httpStatus = Lens.lens (\GetBucketTaggingResponse' {httpStatus} -> httpStatus) (\s@GetBucketTaggingResponse' {} a -> s {httpStatus = a} :: GetBucketTaggingResponse)

-- | Contains the tag set.
getBucketTaggingResponse_tagSet :: Lens.Lens' GetBucketTaggingResponse [Tag]
getBucketTaggingResponse_tagSet = Lens.lens (\GetBucketTaggingResponse' {tagSet} -> tagSet) (\s@GetBucketTaggingResponse' {} a -> s {tagSet = a} :: GetBucketTaggingResponse) Core.. Lens._Coerce

instance Core.NFData GetBucketTaggingResponse
