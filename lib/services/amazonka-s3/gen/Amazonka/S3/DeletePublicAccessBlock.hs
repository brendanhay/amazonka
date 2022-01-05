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
-- Module      : Amazonka.S3.DeletePublicAccessBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the @PublicAccessBlock@ configuration for an Amazon S3 bucket.
-- To use this operation, you must have the @s3:PutBucketPublicAccessBlock@
-- permission. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- The following operations are related to @DeletePublicAccessBlock@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus>
module Amazonka.S3.DeletePublicAccessBlock
  ( -- * Creating a Request
    DeletePublicAccessBlock (..),
    newDeletePublicAccessBlock,

    -- * Request Lenses
    deletePublicAccessBlock_expectedBucketOwner,
    deletePublicAccessBlock_bucket,

    -- * Destructuring the Response
    DeletePublicAccessBlockResponse (..),
    newDeletePublicAccessBlockResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeletePublicAccessBlock' smart constructor.
data DeletePublicAccessBlock = DeletePublicAccessBlock'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to
    -- delete.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublicAccessBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deletePublicAccessBlock_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deletePublicAccessBlock_bucket' - The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to
-- delete.
newDeletePublicAccessBlock ::
  -- | 'bucket'
  BucketName ->
  DeletePublicAccessBlock
newDeletePublicAccessBlock pBucket_ =
  DeletePublicAccessBlock'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deletePublicAccessBlock_expectedBucketOwner :: Lens.Lens' DeletePublicAccessBlock (Prelude.Maybe Prelude.Text)
deletePublicAccessBlock_expectedBucketOwner = Lens.lens (\DeletePublicAccessBlock' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeletePublicAccessBlock' {} a -> s {expectedBucketOwner = a} :: DeletePublicAccessBlock)

-- | The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to
-- delete.
deletePublicAccessBlock_bucket :: Lens.Lens' DeletePublicAccessBlock BucketName
deletePublicAccessBlock_bucket = Lens.lens (\DeletePublicAccessBlock' {bucket} -> bucket) (\s@DeletePublicAccessBlock' {} a -> s {bucket = a} :: DeletePublicAccessBlock)

instance Core.AWSRequest DeletePublicAccessBlock where
  type
    AWSResponse DeletePublicAccessBlock =
      DeletePublicAccessBlockResponse
  request =
    Request.s3vhost
      Prelude.. Request.delete defaultService
  response =
    Response.receiveNull
      DeletePublicAccessBlockResponse'

instance Prelude.Hashable DeletePublicAccessBlock where
  hashWithSalt _salt DeletePublicAccessBlock' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData DeletePublicAccessBlock where
  rnf DeletePublicAccessBlock' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Core.ToHeaders DeletePublicAccessBlock where
  toHeaders DeletePublicAccessBlock' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath DeletePublicAccessBlock where
  toPath DeletePublicAccessBlock' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery DeletePublicAccessBlock where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["publicAccessBlock"])

-- | /See:/ 'newDeletePublicAccessBlockResponse' smart constructor.
data DeletePublicAccessBlockResponse = DeletePublicAccessBlockResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublicAccessBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePublicAccessBlockResponse ::
  DeletePublicAccessBlockResponse
newDeletePublicAccessBlockResponse =
  DeletePublicAccessBlockResponse'

instance
  Prelude.NFData
    DeletePublicAccessBlockResponse
  where
  rnf _ = ()
