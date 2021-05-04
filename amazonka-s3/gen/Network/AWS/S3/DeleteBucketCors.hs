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
-- Module      : Network.AWS.S3.DeleteBucketCors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @cors@ configuration information set for the bucket.
--
-- To use this operation, you must have permission to perform the
-- @s3:PutBucketCORS@ action. The bucket owner has this permission by
-- default and can grant this permission to others.
--
-- For information about @cors@, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- __Related Resources:__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html RESTOPTIONSobject>
module Network.AWS.S3.DeleteBucketCors
  ( -- * Creating a Request
    DeleteBucketCors (..),
    newDeleteBucketCors,

    -- * Request Lenses
    deleteBucketCors_expectedBucketOwner,
    deleteBucketCors_bucket,

    -- * Destructuring the Response
    DeleteBucketCorsResponse (..),
    newDeleteBucketCorsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketCors' smart constructor.
data DeleteBucketCors = DeleteBucketCors'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the bucket whose @cors@ configuration is being deleted.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketCors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketCors_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketCors_bucket' - Specifies the bucket whose @cors@ configuration is being deleted.
newDeleteBucketCors ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketCors
newDeleteBucketCors pBucket_ =
  DeleteBucketCors'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketCors_expectedBucketOwner :: Lens.Lens' DeleteBucketCors (Prelude.Maybe Prelude.Text)
deleteBucketCors_expectedBucketOwner = Lens.lens (\DeleteBucketCors' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketCors' {} a -> s {expectedBucketOwner = a} :: DeleteBucketCors)

-- | Specifies the bucket whose @cors@ configuration is being deleted.
deleteBucketCors_bucket :: Lens.Lens' DeleteBucketCors BucketName
deleteBucketCors_bucket = Lens.lens (\DeleteBucketCors' {bucket} -> bucket) (\s@DeleteBucketCors' {} a -> s {bucket = a} :: DeleteBucketCors)

instance Prelude.AWSRequest DeleteBucketCors where
  type Rs DeleteBucketCors = DeleteBucketCorsResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteBucketCorsResponse'

instance Prelude.Hashable DeleteBucketCors

instance Prelude.NFData DeleteBucketCors

instance Prelude.ToHeaders DeleteBucketCors where
  toHeaders DeleteBucketCors' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath DeleteBucketCors where
  toPath DeleteBucketCors' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery DeleteBucketCors where
  toQuery = Prelude.const (Prelude.mconcat ["cors"])

-- | /See:/ 'newDeleteBucketCorsResponse' smart constructor.
data DeleteBucketCorsResponse = DeleteBucketCorsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketCorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketCorsResponse ::
  DeleteBucketCorsResponse
newDeleteBucketCorsResponse =
  DeleteBucketCorsResponse'

instance Prelude.NFData DeleteBucketCorsResponse
