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
-- Module      : Network.AWS.SESv2.Types.EventDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.EventDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.CloudWatchDestination
import Network.AWS.SESv2.Types.EventType
import Network.AWS.SESv2.Types.KinesisFirehoseDestination
import Network.AWS.SESv2.Types.PinpointDestination
import Network.AWS.SESv2.Types.SnsDestination

-- | In the Amazon SES API v2, /events/ include message sends, deliveries,
-- opens, clicks, bounces, complaints and delivery delays. /Event
-- destinations/ are places that you can send information about these
-- events to. For example, you can send event data to Amazon SNS to receive
-- notifications when you receive bounces or complaints, or you can use
-- Amazon Kinesis Data Firehose to stream data to Amazon S3 for long-term
-- storage.
--
-- /See:/ 'newEventDestination' smart constructor.
data EventDestination = EventDestination'
  { -- | An object that defines an Amazon CloudWatch destination for email
    -- events. You can use Amazon CloudWatch to monitor and gain insights on
    -- your email sending metrics.
    cloudWatchDestination :: Prelude.Maybe CloudWatchDestination,
    -- | If @true@, the event destination is enabled. When the event destination
    -- is enabled, the specified event types are sent to the destinations in
    -- this @EventDestinationDefinition@.
    --
    -- If @false@, the event destination is disabled. When the event
    -- destination is disabled, events aren\'t sent to the specified
    -- destinations.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that defines an Amazon Pinpoint project destination for email
    -- events. You can send email event data to a Amazon Pinpoint project to
    -- view metrics using the Transactional Messaging dashboards that are built
    -- in to Amazon Pinpoint. For more information, see
    -- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
    -- in the /Amazon Pinpoint User Guide/.
    pinpointDestination :: Prelude.Maybe PinpointDestination,
    -- | An object that defines an Amazon Kinesis Data Firehose destination for
    -- email events. You can use Amazon Kinesis Data Firehose to stream data to
    -- other services, such as Amazon S3 and Amazon Redshift.
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    -- | An object that defines an Amazon SNS destination for email events. You
    -- can use Amazon SNS to send notification when certain email events occur.
    snsDestination :: Prelude.Maybe SnsDestination,
    -- | A name that identifies the event destination.
    name :: Prelude.Text,
    -- | The types of events that Amazon SES sends to the specified event
    -- destinations.
    matchingEventTypes :: [EventType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchDestination', 'eventDestination_cloudWatchDestination' - An object that defines an Amazon CloudWatch destination for email
-- events. You can use Amazon CloudWatch to monitor and gain insights on
-- your email sending metrics.
--
-- 'enabled', 'eventDestination_enabled' - If @true@, the event destination is enabled. When the event destination
-- is enabled, the specified event types are sent to the destinations in
-- this @EventDestinationDefinition@.
--
-- If @false@, the event destination is disabled. When the event
-- destination is disabled, events aren\'t sent to the specified
-- destinations.
--
-- 'pinpointDestination', 'eventDestination_pinpointDestination' - An object that defines an Amazon Pinpoint project destination for email
-- events. You can send email event data to a Amazon Pinpoint project to
-- view metrics using the Transactional Messaging dashboards that are built
-- in to Amazon Pinpoint. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
-- in the /Amazon Pinpoint User Guide/.
--
-- 'kinesisFirehoseDestination', 'eventDestination_kinesisFirehoseDestination' - An object that defines an Amazon Kinesis Data Firehose destination for
-- email events. You can use Amazon Kinesis Data Firehose to stream data to
-- other services, such as Amazon S3 and Amazon Redshift.
--
-- 'snsDestination', 'eventDestination_snsDestination' - An object that defines an Amazon SNS destination for email events. You
-- can use Amazon SNS to send notification when certain email events occur.
--
-- 'name', 'eventDestination_name' - A name that identifies the event destination.
--
-- 'matchingEventTypes', 'eventDestination_matchingEventTypes' - The types of events that Amazon SES sends to the specified event
-- destinations.
newEventDestination ::
  -- | 'name'
  Prelude.Text ->
  EventDestination
newEventDestination pName_ =
  EventDestination'
    { cloudWatchDestination =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      pinpointDestination = Prelude.Nothing,
      kinesisFirehoseDestination = Prelude.Nothing,
      snsDestination = Prelude.Nothing,
      name = pName_,
      matchingEventTypes = Prelude.mempty
    }

-- | An object that defines an Amazon CloudWatch destination for email
-- events. You can use Amazon CloudWatch to monitor and gain insights on
-- your email sending metrics.
eventDestination_cloudWatchDestination :: Lens.Lens' EventDestination (Prelude.Maybe CloudWatchDestination)
eventDestination_cloudWatchDestination = Lens.lens (\EventDestination' {cloudWatchDestination} -> cloudWatchDestination) (\s@EventDestination' {} a -> s {cloudWatchDestination = a} :: EventDestination)

-- | If @true@, the event destination is enabled. When the event destination
-- is enabled, the specified event types are sent to the destinations in
-- this @EventDestinationDefinition@.
--
-- If @false@, the event destination is disabled. When the event
-- destination is disabled, events aren\'t sent to the specified
-- destinations.
eventDestination_enabled :: Lens.Lens' EventDestination (Prelude.Maybe Prelude.Bool)
eventDestination_enabled = Lens.lens (\EventDestination' {enabled} -> enabled) (\s@EventDestination' {} a -> s {enabled = a} :: EventDestination)

-- | An object that defines an Amazon Pinpoint project destination for email
-- events. You can send email event data to a Amazon Pinpoint project to
-- view metrics using the Transactional Messaging dashboards that are built
-- in to Amazon Pinpoint. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
-- in the /Amazon Pinpoint User Guide/.
eventDestination_pinpointDestination :: Lens.Lens' EventDestination (Prelude.Maybe PinpointDestination)
eventDestination_pinpointDestination = Lens.lens (\EventDestination' {pinpointDestination} -> pinpointDestination) (\s@EventDestination' {} a -> s {pinpointDestination = a} :: EventDestination)

-- | An object that defines an Amazon Kinesis Data Firehose destination for
-- email events. You can use Amazon Kinesis Data Firehose to stream data to
-- other services, such as Amazon S3 and Amazon Redshift.
eventDestination_kinesisFirehoseDestination :: Lens.Lens' EventDestination (Prelude.Maybe KinesisFirehoseDestination)
eventDestination_kinesisFirehoseDestination = Lens.lens (\EventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestination' {} a -> s {kinesisFirehoseDestination = a} :: EventDestination)

-- | An object that defines an Amazon SNS destination for email events. You
-- can use Amazon SNS to send notification when certain email events occur.
eventDestination_snsDestination :: Lens.Lens' EventDestination (Prelude.Maybe SnsDestination)
eventDestination_snsDestination = Lens.lens (\EventDestination' {snsDestination} -> snsDestination) (\s@EventDestination' {} a -> s {snsDestination = a} :: EventDestination)

-- | A name that identifies the event destination.
eventDestination_name :: Lens.Lens' EventDestination Prelude.Text
eventDestination_name = Lens.lens (\EventDestination' {name} -> name) (\s@EventDestination' {} a -> s {name = a} :: EventDestination)

-- | The types of events that Amazon SES sends to the specified event
-- destinations.
eventDestination_matchingEventTypes :: Lens.Lens' EventDestination [EventType]
eventDestination_matchingEventTypes = Lens.lens (\EventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestination' {} a -> s {matchingEventTypes = a} :: EventDestination) Prelude.. Lens._Coerce

instance Core.FromJSON EventDestination where
  parseJSON =
    Core.withObject
      "EventDestination"
      ( \x ->
          EventDestination'
            Prelude.<$> (x Core..:? "CloudWatchDestination")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "PinpointDestination")
            Prelude.<*> (x Core..:? "KinesisFirehoseDestination")
            Prelude.<*> (x Core..:? "SnsDestination")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> ( x Core..:? "MatchingEventTypes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EventDestination

instance Prelude.NFData EventDestination
