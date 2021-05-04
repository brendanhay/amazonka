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
-- Module      : Network.AWS.S3.DeleteBucketOwnershipControls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes @OwnershipControls@ for an Amazon S3 bucket. To use this
-- operation, you must have the @s3:PutBucketOwnershipControls@ permission.
-- For more information about Amazon S3 permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
--
-- For information about Amazon S3 Object Ownership, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership>.
--
-- The following operations are related to @DeleteBucketOwnershipControls@:
--
-- -   GetBucketOwnershipControls
--
-- -   PutBucketOwnershipControls
module Network.AWS.S3.DeleteBucketOwnershipControls
  ( -- * Creating a Request
    DeleteBucketOwnershipControls (..),
    newDeleteBucketOwnershipControls,

    -- * Request Lenses
    deleteBucketOwnershipControls_expectedBucketOwner,
    deleteBucketOwnershipControls_bucket,

    -- * Destructuring the Response
    DeleteBucketOwnershipControlsResponse (..),
    newDeleteBucketOwnershipControlsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketOwnershipControls' smart constructor.
data DeleteBucketOwnershipControls = DeleteBucketOwnershipControls'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketOwnershipControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketOwnershipControls_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketOwnershipControls_bucket' - The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
newDeleteBucketOwnershipControls ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketOwnershipControls
newDeleteBucketOwnershipControls pBucket_ =
  DeleteBucketOwnershipControls'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketOwnershipControls_expectedBucketOwner :: Lens.Lens' DeleteBucketOwnershipControls (Prelude.Maybe Prelude.Text)
deleteBucketOwnershipControls_expectedBucketOwner = Lens.lens (\DeleteBucketOwnershipControls' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketOwnershipControls' {} a -> s {expectedBucketOwner = a} :: DeleteBucketOwnershipControls)

-- | The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
deleteBucketOwnershipControls_bucket :: Lens.Lens' DeleteBucketOwnershipControls BucketName
deleteBucketOwnershipControls_bucket = Lens.lens (\DeleteBucketOwnershipControls' {bucket} -> bucket) (\s@DeleteBucketOwnershipControls' {} a -> s {bucket = a} :: DeleteBucketOwnershipControls)

instance
  Prelude.AWSRequest
    DeleteBucketOwnershipControls
  where
  type
    Rs DeleteBucketOwnershipControls =
      DeleteBucketOwnershipControlsResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBucketOwnershipControlsResponse'

instance
  Prelude.Hashable
    DeleteBucketOwnershipControls

instance Prelude.NFData DeleteBucketOwnershipControls

instance
  Prelude.ToHeaders
    DeleteBucketOwnershipControls
  where
  toHeaders DeleteBucketOwnershipControls' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath DeleteBucketOwnershipControls where
  toPath DeleteBucketOwnershipControls' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance
  Prelude.ToQuery
    DeleteBucketOwnershipControls
  where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["ownershipControls"])

-- | /See:/ 'newDeleteBucketOwnershipControlsResponse' smart constructor.
data DeleteBucketOwnershipControlsResponse = DeleteBucketOwnershipControlsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketOwnershipControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketOwnershipControlsResponse ::
  DeleteBucketOwnershipControlsResponse
newDeleteBucketOwnershipControlsResponse =
  DeleteBucketOwnershipControlsResponse'

instance
  Prelude.NFData
    DeleteBucketOwnershipControlsResponse
