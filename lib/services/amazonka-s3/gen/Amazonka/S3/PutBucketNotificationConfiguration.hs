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
-- Module      : Amazonka.S3.PutBucketNotificationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- This action replaces the existing notification configuration with the
-- configuration you include in the request body.
--
-- After Amazon S3 receives this request, it first verifies that any Amazon
-- Simple Notification Service (Amazon SNS) or Amazon Simple Queue Service
-- (Amazon SQS) destination exists, and that the bucket owner has
-- permission to publish to it by sending a test notification. In the case
-- of Lambda destinations, Amazon S3 verifies that the Lambda function
-- permissions grant Amazon S3 permission to invoke the function from the
-- Amazon S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Notifications for Amazon S3 Events>.
--
-- You can disable notifications by adding the empty
-- NotificationConfiguration element.
--
-- For more information about the number of event notification
-- configurations that you can create per bucket, see
-- <https://docs.aws.amazon.com/general/latest/gr/s3.html#limits_s3 Amazon S3 service quotas>
-- in /Amazon Web Services General Reference/.
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
-- message fails, the entire PUT action will fail, and Amazon S3 will not
-- add the configuration to your bucket.
--
-- __Responses__
--
-- If the configuration in the request body includes only one
-- @TopicConfiguration@ specifying only the
-- @s3:ReducedRedundancyLostObject@ event type, the response will also
-- include the @x-amz-sns-test-message-id@ header containing the message ID
-- of the test notification sent to the topic.
--
-- The following action is related to @PutBucketNotificationConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketNotificationConfiguration.html GetBucketNotificationConfiguration>
module Amazonka.S3.PutBucketNotificationConfiguration
  ( -- * Creating a Request
    PutBucketNotificationConfiguration (..),
    newPutBucketNotificationConfiguration,

    -- * Request Lenses
    putBucketNotificationConfiguration_expectedBucketOwner,
    putBucketNotificationConfiguration_skipDestinationValidation,
    putBucketNotificationConfiguration_bucket,
    putBucketNotificationConfiguration_notificationConfiguration,

    -- * Destructuring the Response
    PutBucketNotificationConfigurationResponse (..),
    newPutBucketNotificationConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketNotificationConfiguration' smart constructor.
data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Skips validation of Amazon SQS, Amazon SNS, and Lambda destinations.
    -- True or false value.
    skipDestinationValidation :: Prelude.Maybe Prelude.Bool,
    -- | The name of the bucket.
    bucket :: BucketName,
    notificationConfiguration :: NotificationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketNotificationConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'skipDestinationValidation', 'putBucketNotificationConfiguration_skipDestinationValidation' - Skips validation of Amazon SQS, Amazon SNS, and Lambda destinations.
-- True or false value.
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
        skipDestinationValidation =
          Prelude.Nothing,
        bucket = pBucket_,
        notificationConfiguration =
          pNotificationConfiguration_
      }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketNotificationConfiguration_expectedBucketOwner :: Lens.Lens' PutBucketNotificationConfiguration (Prelude.Maybe Prelude.Text)
putBucketNotificationConfiguration_expectedBucketOwner = Lens.lens (\PutBucketNotificationConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketNotificationConfiguration' {} a -> s {expectedBucketOwner = a} :: PutBucketNotificationConfiguration)

-- | Skips validation of Amazon SQS, Amazon SNS, and Lambda destinations.
-- True or false value.
putBucketNotificationConfiguration_skipDestinationValidation :: Lens.Lens' PutBucketNotificationConfiguration (Prelude.Maybe Prelude.Bool)
putBucketNotificationConfiguration_skipDestinationValidation = Lens.lens (\PutBucketNotificationConfiguration' {skipDestinationValidation} -> skipDestinationValidation) (\s@PutBucketNotificationConfiguration' {} a -> s {skipDestinationValidation = a} :: PutBucketNotificationConfiguration)

-- | The name of the bucket.
putBucketNotificationConfiguration_bucket :: Lens.Lens' PutBucketNotificationConfiguration BucketName
putBucketNotificationConfiguration_bucket = Lens.lens (\PutBucketNotificationConfiguration' {bucket} -> bucket) (\s@PutBucketNotificationConfiguration' {} a -> s {bucket = a} :: PutBucketNotificationConfiguration)

-- | Undocumented member.
putBucketNotificationConfiguration_notificationConfiguration :: Lens.Lens' PutBucketNotificationConfiguration NotificationConfiguration
putBucketNotificationConfiguration_notificationConfiguration = Lens.lens (\PutBucketNotificationConfiguration' {notificationConfiguration} -> notificationConfiguration) (\s@PutBucketNotificationConfiguration' {} a -> s {notificationConfiguration = a} :: PutBucketNotificationConfiguration)

instance
  Core.AWSRequest
    PutBucketNotificationConfiguration
  where
  type
    AWSResponse PutBucketNotificationConfiguration =
      PutBucketNotificationConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull
      PutBucketNotificationConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketNotificationConfiguration
  where
  hashWithSalt
    _salt
    PutBucketNotificationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` skipDestinationValidation
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` notificationConfiguration

instance
  Prelude.NFData
    PutBucketNotificationConfiguration
  where
  rnf PutBucketNotificationConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf skipDestinationValidation
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf notificationConfiguration

instance
  Data.ToElement
    PutBucketNotificationConfiguration
  where
  toElement PutBucketNotificationConfiguration' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}NotificationConfiguration"
      notificationConfiguration

instance
  Data.ToHeaders
    PutBucketNotificationConfiguration
  where
  toHeaders PutBucketNotificationConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-skip-destination-validation"
          Data.=# skipDestinationValidation
      ]

instance
  Data.ToPath
    PutBucketNotificationConfiguration
  where
  toPath PutBucketNotificationConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance
  Data.ToQuery
    PutBucketNotificationConfiguration
  where
  toQuery =
    Prelude.const (Prelude.mconcat ["notification"])

-- | /See:/ 'newPutBucketNotificationConfigurationResponse' smart constructor.
data PutBucketNotificationConfigurationResponse = PutBucketNotificationConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
