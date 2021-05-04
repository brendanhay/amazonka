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
-- Module      : Network.AWS.S3.DeleteBucketTagging
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
module Network.AWS.S3.DeleteBucketTagging
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketTagging' smart constructor.
data DeleteBucketTagging = DeleteBucketTagging'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket that has the tag set to be removed.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketTagging_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
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

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketTagging_expectedBucketOwner :: Lens.Lens' DeleteBucketTagging (Prelude.Maybe Prelude.Text)
deleteBucketTagging_expectedBucketOwner = Lens.lens (\DeleteBucketTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketTagging' {} a -> s {expectedBucketOwner = a} :: DeleteBucketTagging)

-- | The bucket that has the tag set to be removed.
deleteBucketTagging_bucket :: Lens.Lens' DeleteBucketTagging BucketName
deleteBucketTagging_bucket = Lens.lens (\DeleteBucketTagging' {bucket} -> bucket) (\s@DeleteBucketTagging' {} a -> s {bucket = a} :: DeleteBucketTagging)

instance Prelude.AWSRequest DeleteBucketTagging where
  type
    Rs DeleteBucketTagging =
      DeleteBucketTaggingResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteBucketTaggingResponse'

instance Prelude.Hashable DeleteBucketTagging

instance Prelude.NFData DeleteBucketTagging

instance Prelude.ToHeaders DeleteBucketTagging where
  toHeaders DeleteBucketTagging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath DeleteBucketTagging where
  toPath DeleteBucketTagging' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery DeleteBucketTagging where
  toQuery = Prelude.const (Prelude.mconcat ["tagging"])

-- | /See:/ 'newDeleteBucketTaggingResponse' smart constructor.
data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketTaggingResponse ::
  DeleteBucketTaggingResponse
newDeleteBucketTaggingResponse =
  DeleteBucketTaggingResponse'

instance Prelude.NFData DeleteBucketTaggingResponse
