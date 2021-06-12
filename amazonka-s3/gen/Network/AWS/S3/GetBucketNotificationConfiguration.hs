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
-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the notification configuration of a bucket.
--
-- If notifications are not enabled on the bucket, the operation returns an
-- empty @NotificationConfiguration@ element.
--
-- By default, you must be the bucket owner to read the notification
-- configuration of a bucket. However, the bucket owner can use a bucket
-- policy to grant permission to other users to read this configuration
-- with the @s3:GetBucketNotification@ permission.
--
-- For more information about setting and reading the notification
-- configuration on a bucket, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Setting Up Notification of Bucket Events>.
-- For more information about bucket policies, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies>.
--
-- The following operation is related to @GetBucketNotification@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketNotification.html PutBucketNotification>
module Network.AWS.S3.GetBucketNotificationConfiguration
  ( -- * Creating a Request
    GetBucketNotificationConfiguration (..),
    newGetBucketNotificationConfiguration,

    -- * Request Lenses
    getBucketNotificationConfiguration_expectedBucketOwner,
    getBucketNotificationConfiguration_bucket,

    -- * Destructuring the Response
    NotificationConfiguration (..),
    newNotificationConfiguration,

    -- * Response Lenses
    notificationConfiguration_lambdaFunctionConfigurations,
    notificationConfiguration_queueConfigurations,
    notificationConfiguration_topicConfigurations,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketNotificationConfiguration' smart constructor.
data GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The name of the bucket for which to get the notification configuration.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketNotificationConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketNotificationConfiguration_bucket' - The name of the bucket for which to get the notification configuration.
newGetBucketNotificationConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetBucketNotificationConfiguration
newGetBucketNotificationConfiguration pBucket_ =
  GetBucketNotificationConfiguration'
    { expectedBucketOwner =
        Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketNotificationConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketNotificationConfiguration (Core.Maybe Core.Text)
getBucketNotificationConfiguration_expectedBucketOwner = Lens.lens (\GetBucketNotificationConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketNotificationConfiguration' {} a -> s {expectedBucketOwner = a} :: GetBucketNotificationConfiguration)

-- | The name of the bucket for which to get the notification configuration.
getBucketNotificationConfiguration_bucket :: Lens.Lens' GetBucketNotificationConfiguration BucketName
getBucketNotificationConfiguration_bucket = Lens.lens (\GetBucketNotificationConfiguration' {bucket} -> bucket) (\s@GetBucketNotificationConfiguration' {} a -> s {bucket = a} :: GetBucketNotificationConfiguration)

instance
  Core.AWSRequest
    GetBucketNotificationConfiguration
  where
  type
    AWSResponse GetBucketNotificationConfiguration =
      NotificationConfiguration
  request = Request.get defaultService
  response =
    Response.receiveXML (\s h x -> Core.parseXML x)

instance
  Core.Hashable
    GetBucketNotificationConfiguration

instance
  Core.NFData
    GetBucketNotificationConfiguration

instance
  Core.ToHeaders
    GetBucketNotificationConfiguration
  where
  toHeaders GetBucketNotificationConfiguration' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance
  Core.ToPath
    GetBucketNotificationConfiguration
  where
  toPath GetBucketNotificationConfiguration' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance
  Core.ToQuery
    GetBucketNotificationConfiguration
  where
  toQuery = Core.const (Core.mconcat ["notification"])
