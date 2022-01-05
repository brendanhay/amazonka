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
-- Module      : Amazonka.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the tags from the bucket.
--
-- To use this operation, you must have permission to perform the
-- @s3:PutBucketTagging@ action. By default, the bucket owner has this
-- permission and can grant this permission to others.
--
-- The following operations are related to @DeleteBucketTagging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
module Amazonka.S3.DeleteBucketTagging
  ( -- * Creating a Request
    DeleteBucketTagging (..),
    newDeleteBucketTagging,

    -- * Request Lenses
    deleteBucketTagging_expectedBucketOwner,
    deleteBucketTagging_bucket,

    -- * Destructuring the Response
    DeleteBucketTaggingResponse (..),
    newDeleteBucketTaggingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucketTagging' smart constructor.
data DeleteBucketTagging = DeleteBucketTagging'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket that has the tag set to be removed.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketTagging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketTagging_bucket' - The bucket that has the tag set to be removed.
newDeleteBucketTagging ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketTagging
newDeleteBucketTagging pBucket_ =
  DeleteBucketTagging'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketTagging_expectedBucketOwner :: Lens.Lens' DeleteBucketTagging (Prelude.Maybe Prelude.Text)
deleteBucketTagging_expectedBucketOwner = Lens.lens (\DeleteBucketTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketTagging' {} a -> s {expectedBucketOwner = a} :: DeleteBucketTagging)

-- | The bucket that has the tag set to be removed.
deleteBucketTagging_bucket :: Lens.Lens' DeleteBucketTagging BucketName
deleteBucketTagging_bucket = Lens.lens (\DeleteBucketTagging' {bucket} -> bucket) (\s@DeleteBucketTagging' {} a -> s {bucket = a} :: DeleteBucketTagging)

instance Core.AWSRequest DeleteBucketTagging where
  type
    AWSResponse DeleteBucketTagging =
      DeleteBucketTaggingResponse
  request =
    Request.s3vhost
      Prelude.. Request.delete defaultService
  response =
    Response.receiveNull DeleteBucketTaggingResponse'

instance Prelude.Hashable DeleteBucketTagging where
  hashWithSalt _salt DeleteBucketTagging' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData DeleteBucketTagging where
  rnf DeleteBucketTagging' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Core.ToHeaders DeleteBucketTagging where
  toHeaders DeleteBucketTagging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath DeleteBucketTagging where
  toPath DeleteBucketTagging' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery DeleteBucketTagging where
  toQuery = Prelude.const (Prelude.mconcat ["tagging"])

-- | /See:/ 'newDeleteBucketTaggingResponse' smart constructor.
data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketTaggingResponse ::
  DeleteBucketTaggingResponse
newDeleteBucketTaggingResponse =
  DeleteBucketTaggingResponse'

instance Prelude.NFData DeleteBucketTaggingResponse where
  rnf _ = ()
