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
-- Module      : Amazonka.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the notification configuration of a bucket.
--
-- If notifications are not enabled on the bucket, the action returns an
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
-- The following action is related to @GetBucketNotification@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketNotification.html PutBucketNotification>
module Amazonka.S3.GetBucketNotificationConfiguration
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
    notificationConfiguration_eventBridgeConfiguration,
    notificationConfiguration_queueConfigurations,
    notificationConfiguration_topicConfigurations,
    notificationConfiguration_lambdaFunctionConfigurations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketNotificationConfiguration' smart constructor.
data GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which to get the notification configuration.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketNotificationConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketNotificationConfiguration_bucket' - The name of the bucket for which to get the notification configuration.
newGetBucketNotificationConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetBucketNotificationConfiguration
newGetBucketNotificationConfiguration pBucket_ =
  GetBucketNotificationConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketNotificationConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketNotificationConfiguration (Prelude.Maybe Prelude.Text)
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
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML (\s h x -> Data.parseXML x)

instance
  Prelude.Hashable
    GetBucketNotificationConfiguration
  where
  hashWithSalt
    _salt
    GetBucketNotificationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    GetBucketNotificationConfiguration
  where
  rnf GetBucketNotificationConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToHeaders
    GetBucketNotificationConfiguration
  where
  toHeaders GetBucketNotificationConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance
  Data.ToPath
    GetBucketNotificationConfiguration
  where
  toPath GetBucketNotificationConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance
  Data.ToQuery
    GetBucketNotificationConfiguration
  where
  toQuery =
    Prelude.const (Prelude.mconcat ["notification"])
