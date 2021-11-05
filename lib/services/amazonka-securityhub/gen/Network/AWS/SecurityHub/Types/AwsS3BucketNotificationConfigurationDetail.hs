{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecurityHub.Types.AwsS3BucketNotificationConfigurationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsS3BucketNotificationConfigurationDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsS3BucketNotificationConfigurationFilter

-- | Details for an S3 bucket notification configuration.
--
-- /See:/ 'newAwsS3BucketNotificationConfigurationDetail' smart constructor.
data AwsS3BucketNotificationConfigurationDetail = AwsS3BucketNotificationConfigurationDetail'
  { -- | The ARN of the Lambda function, Amazon SQS queue, or Amazon SNS topic
    -- that generates the notification.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The list of events that trigger a notification.
    events :: Prelude.Maybe [Prelude.Text],
    -- | Indicates the type of notification. Notifications can be generated using
    -- Lambda functions, Amazon SQS queues or Amazon SNS topics.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The filters that determine which S3 buckets generate notifications.
    filter' :: Prelude.Maybe AwsS3BucketNotificationConfigurationFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketNotificationConfigurationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'awsS3BucketNotificationConfigurationDetail_destination' - The ARN of the Lambda function, Amazon SQS queue, or Amazon SNS topic
-- that generates the notification.
--
-- 'events', 'awsS3BucketNotificationConfigurationDetail_events' - The list of events that trigger a notification.
--
-- 'type'', 'awsS3BucketNotificationConfigurationDetail_type' - Indicates the type of notification. Notifications can be generated using
-- Lambda functions, Amazon SQS queues or Amazon SNS topics.
--
-- 'filter'', 'awsS3BucketNotificationConfigurationDetail_filter' - The filters that determine which S3 buckets generate notifications.
newAwsS3BucketNotificationConfigurationDetail ::
  AwsS3BucketNotificationConfigurationDetail
newAwsS3BucketNotificationConfigurationDetail =
  AwsS3BucketNotificationConfigurationDetail'
    { destination =
        Prelude.Nothing,
      events = Prelude.Nothing,
      type' = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | The ARN of the Lambda function, Amazon SQS queue, or Amazon SNS topic
-- that generates the notification.
awsS3BucketNotificationConfigurationDetail_destination :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe Prelude.Text)
awsS3BucketNotificationConfigurationDetail_destination = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {destination} -> destination) (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {destination = a} :: AwsS3BucketNotificationConfigurationDetail)

-- | The list of events that trigger a notification.
awsS3BucketNotificationConfigurationDetail_events :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe [Prelude.Text])
awsS3BucketNotificationConfigurationDetail_events = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {events} -> events) (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {events = a} :: AwsS3BucketNotificationConfigurationDetail) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the type of notification. Notifications can be generated using
-- Lambda functions, Amazon SQS queues or Amazon SNS topics.
awsS3BucketNotificationConfigurationDetail_type :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe Prelude.Text)
awsS3BucketNotificationConfigurationDetail_type = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {type'} -> type') (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {type' = a} :: AwsS3BucketNotificationConfigurationDetail)

-- | The filters that determine which S3 buckets generate notifications.
awsS3BucketNotificationConfigurationDetail_filter :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe AwsS3BucketNotificationConfigurationFilter)
awsS3BucketNotificationConfigurationDetail_filter = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {filter'} -> filter') (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {filter' = a} :: AwsS3BucketNotificationConfigurationDetail)

instance
  Core.FromJSON
    AwsS3BucketNotificationConfigurationDetail
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketNotificationConfigurationDetail"
      ( \x ->
          AwsS3BucketNotificationConfigurationDetail'
            Prelude.<$> (x Core..:? "Destination")
              Prelude.<*> (x Core..:? "Events" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Type")
              Prelude.<*> (x Core..:? "Filter")
      )

instance
  Prelude.Hashable
    AwsS3BucketNotificationConfigurationDetail

instance
  Prelude.NFData
    AwsS3BucketNotificationConfigurationDetail

instance
  Core.ToJSON
    AwsS3BucketNotificationConfigurationDetail
  where
  toJSON
    AwsS3BucketNotificationConfigurationDetail' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Destination" Core..=) Prelude.<$> destination,
              ("Events" Core..=) Prelude.<$> events,
              ("Type" Core..=) Prelude.<$> type',
              ("Filter" Core..=) Prelude.<$> filter'
            ]
        )
