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
-- Module      : Network.AWS.S3.PutBucketNotificationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables notifications of specified events for a bucket. For more
-- information about event notifications, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>.
--
-- Using this API, you can replace an existing notification configuration.
-- The configuration is an XML file that defines the event types that you
-- want Amazon S3 to publish and the destination where you want Amazon S3
-- to publish an event notification when it detects an event of the
-- specified type.
--
-- By default, your bucket has no event notifications configured. That is,
-- the notification configuration will be an empty
-- @NotificationConfiguration@.
--
-- @\<NotificationConfiguration>@
--
-- @\<\/NotificationConfiguration>@
--
-- This operation replaces the existing notification configuration with the
-- configuration you include in the request body.
--
-- After Amazon S3 receives this request, it first verifies that any Amazon
-- Simple Notification Service (Amazon SNS) or Amazon Simple Queue Service
-- (Amazon SQS) destination exists, and that the bucket owner has
-- permission to publish to it by sending a test notification. In the case
-- of AWS Lambda destinations, Amazon S3 verifies that the Lambda function
-- permissions grant Amazon S3 permission to invoke the function from the
-- Amazon S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Notifications for Amazon S3 Events>.
--
-- You can disable notifications by adding the empty
-- NotificationConfiguration element.
--
-- By default, only the bucket owner can configure notifications on a
-- bucket. However, bucket owners can use a bucket policy to grant
-- permission to other users to set this configuration with
-- @s3:PutBucketNotification@ permission.
--
-- The PUT notification is an atomic operation. For example, suppose your
-- notification configuration includes SNS topic, SQS queue, and Lambda
-- function configurations. When you send a PUT request with this
-- configuration, Amazon S3 sends test messages to your SNS topic. If the
-- message fails, the entire PUT operation will fail, and Amazon S3 will
-- not add the configuration to your bucket.
--
-- __Responses__
--
-- If the configuration in the request body includes only one
-- @TopicConfiguration@ specifying only the
-- @s3:ReducedRedundancyLostObject@ event type, the response will also
-- include the @x-amz-sns-test-message-id@ header containing the message ID
-- of the test notification sent to the topic.
--
-- The following operation is related to
-- @PutBucketNotificationConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketNotificationConfiguration.html GetBucketNotificationConfiguration>
module Network.AWS.S3.PutBucketNotificationConfiguration
  ( -- * Creating a Request
    PutBucketNotificationConfiguration (..),
    newPutBucketNotificationConfiguration,

    -- * Request Lenses
    putBucketNotificationConfiguration_expectedBucketOwner,
    putBucketNotificationConfiguration_bucket,
    putBucketNotificationConfiguration_notificationConfiguration,

    -- * Destructuring the Response
    PutBucketNotificationConfigurationResponse (..),
    newPutBucketNotificationConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketNotificationConfiguration' smart constructor.
data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket.
    bucket :: BucketName,
    notificationConfiguration :: NotificationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketNotificationConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'putBucketNotificationConfiguration_bucket' - The name of the bucket.
--
-- 'notificationConfiguration', 'putBucketNotificationConfiguration_notificationConfiguration' - Undocumented member.
newPutBucketNotificationConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'notificationConfiguration'
  NotificationConfiguration ->
  PutBucketNotificationConfiguration
newPutBucketNotificationConfiguration
  pBucket_
  pNotificationConfiguration_ =
    PutBucketNotificationConfiguration'
      { expectedBucketOwner =
          Prelude.Nothing,
        bucket = pBucket_,
        notificationConfiguration =
          pNotificationConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketNotificationConfiguration_expectedBucketOwner :: Lens.Lens' PutBucketNotificationConfiguration (Prelude.Maybe Prelude.Text)
putBucketNotificationConfiguration_expectedBucketOwner = Lens.lens (\PutBucketNotificationConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketNotificationConfiguration' {} a -> s {expectedBucketOwner = a} :: PutBucketNotificationConfiguration)

-- | The name of the bucket.
putBucketNotificationConfiguration_bucket :: Lens.Lens' PutBucketNotificationConfiguration BucketName
putBucketNotificationConfiguration_bucket = Lens.lens (\PutBucketNotificationConfiguration' {bucket} -> bucket) (\s@PutBucketNotificationConfiguration' {} a -> s {bucket = a} :: PutBucketNotificationConfiguration)

-- | Undocumented member.
putBucketNotificationConfiguration_notificationConfiguration :: Lens.Lens' PutBucketNotificationConfiguration NotificationConfiguration
putBucketNotificationConfiguration_notificationConfiguration = Lens.lens (\PutBucketNotificationConfiguration' {notificationConfiguration} -> notificationConfiguration) (\s@PutBucketNotificationConfiguration' {} a -> s {notificationConfiguration = a} :: PutBucketNotificationConfiguration)

instance
  Prelude.AWSRequest
    PutBucketNotificationConfiguration
  where
  type
    Rs PutBucketNotificationConfiguration =
      PutBucketNotificationConfigurationResponse
  request = Request.putXML defaultService
  response =
    Response.receiveNull
      PutBucketNotificationConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketNotificationConfiguration

instance
  Prelude.NFData
    PutBucketNotificationConfiguration

instance
  Prelude.ToElement
    PutBucketNotificationConfiguration
  where
  toElement PutBucketNotificationConfiguration' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}NotificationConfiguration"
      notificationConfiguration

instance
  Prelude.ToHeaders
    PutBucketNotificationConfiguration
  where
  toHeaders PutBucketNotificationConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance
  Prelude.ToPath
    PutBucketNotificationConfiguration
  where
  toPath PutBucketNotificationConfiguration' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance
  Prelude.ToQuery
    PutBucketNotificationConfiguration
  where
  toQuery =
    Prelude.const (Prelude.mconcat ["notification"])

-- | /See:/ 'newPutBucketNotificationConfigurationResponse' smart constructor.
data PutBucketNotificationConfigurationResponse = PutBucketNotificationConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketNotificationConfigurationResponse ::
  PutBucketNotificationConfigurationResponse
newPutBucketNotificationConfigurationResponse =
  PutBucketNotificationConfigurationResponse'

instance
  Prelude.NFData
    PutBucketNotificationConfigurationResponse
