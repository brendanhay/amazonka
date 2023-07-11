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
-- Module      : Amazonka.S3.GetBucketTagging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- -   Error code: @NoSuchTagSet@
--
--     -   Description: There is no tag set associated with the bucket.
--
-- The following operations are related to @GetBucketTagging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging>
module Amazonka.S3.GetBucketTagging
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketTagging' smart constructor.
data GetBucketTagging = GetBucketTagging'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which to get the tagging information.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketTagging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketTagging_bucket' - The name of the bucket for which to get the tagging information.
newGetBucketTagging ::
  -- | 'bucket'
  BucketName ->
  GetBucketTagging
newGetBucketTagging pBucket_ =
  GetBucketTagging'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketTagging_expectedBucketOwner :: Lens.Lens' GetBucketTagging (Prelude.Maybe Prelude.Text)
getBucketTagging_expectedBucketOwner = Lens.lens (\GetBucketTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketTagging' {} a -> s {expectedBucketOwner = a} :: GetBucketTagging)

-- | The name of the bucket for which to get the tagging information.
getBucketTagging_bucket :: Lens.Lens' GetBucketTagging BucketName
getBucketTagging_bucket = Lens.lens (\GetBucketTagging' {bucket} -> bucket) (\s@GetBucketTagging' {} a -> s {bucket = a} :: GetBucketTagging)

instance Core.AWSRequest GetBucketTagging where
  type
    AWSResponse GetBucketTagging =
      GetBucketTaggingResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketTaggingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "TagSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "Tag"
                        )
      )

instance Prelude.Hashable GetBucketTagging where
  hashWithSalt _salt GetBucketTagging' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketTagging where
  rnf GetBucketTagging' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetBucketTagging where
  toHeaders GetBucketTagging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketTagging where
  toPath GetBucketTagging' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketTagging where
  toQuery = Prelude.const (Prelude.mconcat ["tagging"])

-- | /See:/ 'newGetBucketTaggingResponse' smart constructor.
data GetBucketTaggingResponse = GetBucketTaggingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains the tag set.
    tagSet :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetBucketTaggingResponse
newGetBucketTaggingResponse pHttpStatus_ =
  GetBucketTaggingResponse'
    { httpStatus =
        pHttpStatus_,
      tagSet = Prelude.mempty
    }

-- | The response's http status code.
getBucketTaggingResponse_httpStatus :: Lens.Lens' GetBucketTaggingResponse Prelude.Int
getBucketTaggingResponse_httpStatus = Lens.lens (\GetBucketTaggingResponse' {httpStatus} -> httpStatus) (\s@GetBucketTaggingResponse' {} a -> s {httpStatus = a} :: GetBucketTaggingResponse)

-- | Contains the tag set.
getBucketTaggingResponse_tagSet :: Lens.Lens' GetBucketTaggingResponse [Tag]
getBucketTaggingResponse_tagSet = Lens.lens (\GetBucketTaggingResponse' {tagSet} -> tagSet) (\s@GetBucketTaggingResponse' {} a -> s {tagSet = a} :: GetBucketTaggingResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetBucketTaggingResponse where
  rnf GetBucketTaggingResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tagSet
