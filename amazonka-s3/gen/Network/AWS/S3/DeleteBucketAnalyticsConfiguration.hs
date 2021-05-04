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
-- Module      : Network.AWS.S3.DeleteBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analytics configuration for the bucket (specified by the
-- analytics configuration ID).
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutAnalyticsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about the Amazon S3 analytics feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis>.
--
-- The following operations are related to
-- @DeleteBucketAnalyticsConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.DeleteBucketAnalyticsConfiguration
  ( -- * Creating a Request
    DeleteBucketAnalyticsConfiguration (..),
    newDeleteBucketAnalyticsConfiguration,

    -- * Request Lenses
    deleteBucketAnalyticsConfiguration_expectedBucketOwner,
    deleteBucketAnalyticsConfiguration_bucket,
    deleteBucketAnalyticsConfiguration_id,

    -- * Destructuring the Response
    DeleteBucketAnalyticsConfigurationResponse (..),
    newDeleteBucketAnalyticsConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketAnalyticsConfiguration' smart constructor.
data DeleteBucketAnalyticsConfiguration = DeleteBucketAnalyticsConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket from which an analytics configuration is deleted.
    bucket :: BucketName,
    -- | The ID that identifies the analytics configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketAnalyticsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketAnalyticsConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketAnalyticsConfiguration_bucket' - The name of the bucket from which an analytics configuration is deleted.
--
-- 'id', 'deleteBucketAnalyticsConfiguration_id' - The ID that identifies the analytics configuration.
newDeleteBucketAnalyticsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  DeleteBucketAnalyticsConfiguration
newDeleteBucketAnalyticsConfiguration pBucket_ pId_ =
  DeleteBucketAnalyticsConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketAnalyticsConfiguration_expectedBucketOwner :: Lens.Lens' DeleteBucketAnalyticsConfiguration (Prelude.Maybe Prelude.Text)
deleteBucketAnalyticsConfiguration_expectedBucketOwner = Lens.lens (\DeleteBucketAnalyticsConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketAnalyticsConfiguration' {} a -> s {expectedBucketOwner = a} :: DeleteBucketAnalyticsConfiguration)

-- | The name of the bucket from which an analytics configuration is deleted.
deleteBucketAnalyticsConfiguration_bucket :: Lens.Lens' DeleteBucketAnalyticsConfiguration BucketName
deleteBucketAnalyticsConfiguration_bucket = Lens.lens (\DeleteBucketAnalyticsConfiguration' {bucket} -> bucket) (\s@DeleteBucketAnalyticsConfiguration' {} a -> s {bucket = a} :: DeleteBucketAnalyticsConfiguration)

-- | The ID that identifies the analytics configuration.
deleteBucketAnalyticsConfiguration_id :: Lens.Lens' DeleteBucketAnalyticsConfiguration Prelude.Text
deleteBucketAnalyticsConfiguration_id = Lens.lens (\DeleteBucketAnalyticsConfiguration' {id} -> id) (\s@DeleteBucketAnalyticsConfiguration' {} a -> s {id = a} :: DeleteBucketAnalyticsConfiguration)

instance
  Prelude.AWSRequest
    DeleteBucketAnalyticsConfiguration
  where
  type
    Rs DeleteBucketAnalyticsConfiguration =
      DeleteBucketAnalyticsConfigurationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBucketAnalyticsConfigurationResponse'

instance
  Prelude.Hashable
    DeleteBucketAnalyticsConfiguration

instance
  Prelude.NFData
    DeleteBucketAnalyticsConfiguration

instance
  Prelude.ToHeaders
    DeleteBucketAnalyticsConfiguration
  where
  toHeaders DeleteBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance
  Prelude.ToPath
    DeleteBucketAnalyticsConfiguration
  where
  toPath DeleteBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance
  Prelude.ToQuery
    DeleteBucketAnalyticsConfiguration
  where
  toQuery DeleteBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["id" Prelude.=: id, "analytics"]

-- | /See:/ 'newDeleteBucketAnalyticsConfigurationResponse' smart constructor.
data DeleteBucketAnalyticsConfigurationResponse = DeleteBucketAnalyticsConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketAnalyticsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketAnalyticsConfigurationResponse ::
  DeleteBucketAnalyticsConfigurationResponse
newDeleteBucketAnalyticsConfigurationResponse =
  DeleteBucketAnalyticsConfigurationResponse'

instance
  Prelude.NFData
    DeleteBucketAnalyticsConfigurationResponse
