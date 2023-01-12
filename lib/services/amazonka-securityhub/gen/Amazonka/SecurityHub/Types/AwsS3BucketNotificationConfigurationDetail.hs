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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationFilter

-- | Details for an S3 bucket notification configuration.
--
-- /See:/ 'newAwsS3BucketNotificationConfigurationDetail' smart constructor.
data AwsS3BucketNotificationConfigurationDetail = AwsS3BucketNotificationConfigurationDetail'
  { -- | The ARN of the Lambda function, Amazon SQS queue, or Amazon SNS topic
    -- that generates the notification.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The list of events that trigger a notification.
    events :: Prelude.Maybe [Prelude.Text],
    -- | The filters that determine which S3 buckets generate notifications.
    filter' :: Prelude.Maybe AwsS3BucketNotificationConfigurationFilter,
    -- | Indicates the type of notification. Notifications can be generated using
    -- Lambda functions, Amazon SQS queues, or Amazon SNS topics, with
    -- corresponding valid values as follows:
    --
    -- -   @LambdaConfiguration@
    --
    -- -   @QueueConfiguration@
    --
    -- -   @TopicConfiguration@
    type' :: Prelude.Maybe Prelude.Text
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
-- 'filter'', 'awsS3BucketNotificationConfigurationDetail_filter' - The filters that determine which S3 buckets generate notifications.
--
-- 'type'', 'awsS3BucketNotificationConfigurationDetail_type' - Indicates the type of notification. Notifications can be generated using
-- Lambda functions, Amazon SQS queues, or Amazon SNS topics, with
-- corresponding valid values as follows:
--
-- -   @LambdaConfiguration@
--
-- -   @QueueConfiguration@
--
-- -   @TopicConfiguration@
newAwsS3BucketNotificationConfigurationDetail ::
  AwsS3BucketNotificationConfigurationDetail
newAwsS3BucketNotificationConfigurationDetail =
  AwsS3BucketNotificationConfigurationDetail'
    { destination =
        Prelude.Nothing,
      events = Prelude.Nothing,
      filter' = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the Lambda function, Amazon SQS queue, or Amazon SNS topic
-- that generates the notification.
awsS3BucketNotificationConfigurationDetail_destination :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe Prelude.Text)
awsS3BucketNotificationConfigurationDetail_destination = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {destination} -> destination) (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {destination = a} :: AwsS3BucketNotificationConfigurationDetail)

-- | The list of events that trigger a notification.
awsS3BucketNotificationConfigurationDetail_events :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe [Prelude.Text])
awsS3BucketNotificationConfigurationDetail_events = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {events} -> events) (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {events = a} :: AwsS3BucketNotificationConfigurationDetail) Prelude.. Lens.mapping Lens.coerced

-- | The filters that determine which S3 buckets generate notifications.
awsS3BucketNotificationConfigurationDetail_filter :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe AwsS3BucketNotificationConfigurationFilter)
awsS3BucketNotificationConfigurationDetail_filter = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {filter'} -> filter') (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {filter' = a} :: AwsS3BucketNotificationConfigurationDetail)

-- | Indicates the type of notification. Notifications can be generated using
-- Lambda functions, Amazon SQS queues, or Amazon SNS topics, with
-- corresponding valid values as follows:
--
-- -   @LambdaConfiguration@
--
-- -   @QueueConfiguration@
--
-- -   @TopicConfiguration@
awsS3BucketNotificationConfigurationDetail_type :: Lens.Lens' AwsS3BucketNotificationConfigurationDetail (Prelude.Maybe Prelude.Text)
awsS3BucketNotificationConfigurationDetail_type = Lens.lens (\AwsS3BucketNotificationConfigurationDetail' {type'} -> type') (\s@AwsS3BucketNotificationConfigurationDetail' {} a -> s {type' = a} :: AwsS3BucketNotificationConfigurationDetail)

instance
  Data.FromJSON
    AwsS3BucketNotificationConfigurationDetail
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketNotificationConfigurationDetail"
      ( \x ->
          AwsS3BucketNotificationConfigurationDetail'
            Prelude.<$> (x Data..:? "Destination")
              Prelude.<*> (x Data..:? "Events" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Filter")
              Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsS3BucketNotificationConfigurationDetail
  where
  hashWithSalt
    _salt
    AwsS3BucketNotificationConfigurationDetail' {..} =
      _salt `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` events
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsS3BucketNotificationConfigurationDetail
  where
  rnf AwsS3BucketNotificationConfigurationDetail' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsS3BucketNotificationConfigurationDetail
  where
  toJSON
    AwsS3BucketNotificationConfigurationDetail' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Destination" Data..=) Prelude.<$> destination,
              ("Events" Data..=) Prelude.<$> events,
              ("Filter" Data..=) Prelude.<$> filter',
              ("Type" Data..=) Prelude.<$> type'
            ]
        )
