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
-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the website configuration for a bucket. Amazon S3
-- returns a @200 OK@ response upon successfully deleting a website
-- configuration on the specified bucket. You will get a @200 OK@ response
-- if the website configuration you are trying to delete does not exist on
-- the bucket. Amazon S3 returns a @404@ response if the bucket specified
-- in the request does not exist.
--
-- This DELETE operation requires the @S3:DeleteBucketWebsite@ permission.
-- By default, only the bucket owner can delete the website configuration
-- attached to a bucket. However, bucket owners can grant other users
-- permission to delete the website configuration by writing a bucket
-- policy granting them the @S3:DeleteBucketWebsite@ permission.
--
-- For more information about hosting websites, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3>.
--
-- The following operations are related to @DeleteBucketWebsite@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketWebsite.html GetBucketWebsite>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketWebsite.html PutBucketWebsite>
module Network.AWS.S3.DeleteBucketWebsite
  ( -- * Creating a Request
    DeleteBucketWebsite (..),
    newDeleteBucketWebsite,

    -- * Request Lenses
    deleteBucketWebsite_expectedBucketOwner,
    deleteBucketWebsite_bucket,

    -- * Destructuring the Response
    DeleteBucketWebsiteResponse (..),
    newDeleteBucketWebsiteResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketWebsite' smart constructor.
data DeleteBucketWebsite = DeleteBucketWebsite'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name for which you want to remove the website configuration.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketWebsite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketWebsite_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketWebsite_bucket' - The bucket name for which you want to remove the website configuration.
newDeleteBucketWebsite ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketWebsite
newDeleteBucketWebsite pBucket_ =
  DeleteBucketWebsite'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketWebsite_expectedBucketOwner :: Lens.Lens' DeleteBucketWebsite (Prelude.Maybe Prelude.Text)
deleteBucketWebsite_expectedBucketOwner = Lens.lens (\DeleteBucketWebsite' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketWebsite' {} a -> s {expectedBucketOwner = a} :: DeleteBucketWebsite)

-- | The bucket name for which you want to remove the website configuration.
deleteBucketWebsite_bucket :: Lens.Lens' DeleteBucketWebsite BucketName
deleteBucketWebsite_bucket = Lens.lens (\DeleteBucketWebsite' {bucket} -> bucket) (\s@DeleteBucketWebsite' {} a -> s {bucket = a} :: DeleteBucketWebsite)

instance Prelude.AWSRequest DeleteBucketWebsite where
  type
    Rs DeleteBucketWebsite =
      DeleteBucketWebsiteResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteBucketWebsiteResponse'

instance Prelude.Hashable DeleteBucketWebsite

instance Prelude.NFData DeleteBucketWebsite

instance Prelude.ToHeaders DeleteBucketWebsite where
  toHeaders DeleteBucketWebsite' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath DeleteBucketWebsite where
  toPath DeleteBucketWebsite' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery DeleteBucketWebsite where
  toQuery = Prelude.const (Prelude.mconcat ["website"])

-- | /See:/ 'newDeleteBucketWebsiteResponse' smart constructor.
data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketWebsiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketWebsiteResponse ::
  DeleteBucketWebsiteResponse
newDeleteBucketWebsiteResponse =
  DeleteBucketWebsiteResponse'

instance Prelude.NFData DeleteBucketWebsiteResponse
