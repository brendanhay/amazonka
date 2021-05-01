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
-- Module      : Network.AWS.S3.DeleteBucketEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the DELETE operation removes default encryption
-- from the bucket. For information about the Amazon S3 default encryption
-- feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutEncryptionConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
module Network.AWS.S3.DeleteBucketEncryption
  ( -- * Creating a Request
    DeleteBucketEncryption (..),
    newDeleteBucketEncryption,

    -- * Request Lenses
    deleteBucketEncryption_expectedBucketOwner,
    deleteBucketEncryption_bucket,

    -- * Destructuring the Response
    DeleteBucketEncryptionResponse (..),
    newDeleteBucketEncryptionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketEncryption' smart constructor.
data DeleteBucketEncryption = DeleteBucketEncryption'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the server-side encryption
    -- configuration to delete.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketEncryption_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketEncryption_bucket' - The name of the bucket containing the server-side encryption
-- configuration to delete.
newDeleteBucketEncryption ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketEncryption
newDeleteBucketEncryption pBucket_ =
  DeleteBucketEncryption'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketEncryption_expectedBucketOwner :: Lens.Lens' DeleteBucketEncryption (Prelude.Maybe Prelude.Text)
deleteBucketEncryption_expectedBucketOwner = Lens.lens (\DeleteBucketEncryption' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketEncryption' {} a -> s {expectedBucketOwner = a} :: DeleteBucketEncryption)

-- | The name of the bucket containing the server-side encryption
-- configuration to delete.
deleteBucketEncryption_bucket :: Lens.Lens' DeleteBucketEncryption BucketName
deleteBucketEncryption_bucket = Lens.lens (\DeleteBucketEncryption' {bucket} -> bucket) (\s@DeleteBucketEncryption' {} a -> s {bucket = a} :: DeleteBucketEncryption)

instance Prelude.AWSRequest DeleteBucketEncryption where
  type
    Rs DeleteBucketEncryption =
      DeleteBucketEncryptionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBucketEncryptionResponse'

instance Prelude.Hashable DeleteBucketEncryption

instance Prelude.NFData DeleteBucketEncryption

instance Prelude.ToHeaders DeleteBucketEncryption where
  toHeaders DeleteBucketEncryption' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath DeleteBucketEncryption where
  toPath DeleteBucketEncryption' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery DeleteBucketEncryption where
  toQuery =
    Prelude.const (Prelude.mconcat ["encryption"])

-- | /See:/ 'newDeleteBucketEncryptionResponse' smart constructor.
data DeleteBucketEncryptionResponse = DeleteBucketEncryptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketEncryptionResponse ::
  DeleteBucketEncryptionResponse
newDeleteBucketEncryptionResponse =
  DeleteBucketEncryptionResponse'

instance
  Prelude.NFData
    DeleteBucketEncryptionResponse
