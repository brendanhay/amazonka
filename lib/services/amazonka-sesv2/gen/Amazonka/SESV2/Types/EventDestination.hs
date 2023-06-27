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
-- Module      : Amazonka.SESV2.Types.EventDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.EventDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.CloudWatchDestination
import Amazonka.SESV2.Types.EventType
import Amazonka.SESV2.Types.KinesisFirehoseDestination
import Amazonka.SESV2.Types.PinpointDestination
import Amazonka.SESV2.Types.SnsDestination

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
    -- | An object that defines an Amazon Kinesis Data Firehose destination for
    -- email events. You can use Amazon Kinesis Data Firehose to stream data to
    -- other services, such as Amazon S3 and Amazon Redshift.
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    -- | An object that defines an Amazon Pinpoint project destination for email
    -- events. You can send email event data to a Amazon Pinpoint project to
    -- view metrics using the Transactional Messaging dashboards that are built
    -- in to Amazon Pinpoint. For more information, see
    -- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
    -- in the /Amazon Pinpoint User Guide/.
    pinpointDestination :: Prelude.Maybe PinpointDestination,
    -- | An object that defines an Amazon SNS destination for email events. You
    -- can use Amazon SNS to send notification when certain email events occur.
    snsDestination :: Prelude.Maybe SnsDestination,
    -- | A name that identifies the event destination.
    name :: Prelude.Text,
    -- | The types of events that Amazon SES sends to the specified event
    -- destinations.
    --
    -- -   @SEND@ - The send request was successful and SES will attempt to
    --     deliver the message to the recipient’s mail server. (If
    --     account-level or global suppression is being used, SES will still
    --     count it as a send, but delivery is suppressed.)
    --
    -- -   @REJECT@ - SES accepted the email, but determined that it contained
    --     a virus and didn’t attempt to deliver it to the recipient’s mail
    --     server.
    --
    -- -   @BOUNCE@ - (/Hard bounce/) The recipient\'s mail server permanently
    --     rejected the email. (/Soft bounces/ are only included when SES fails
    --     to deliver the email after retrying for a period of time.)
    --
    -- -   @COMPLAINT@ - The email was successfully delivered to the
    --     recipient’s mail server, but the recipient marked it as spam.
    --
    -- -   @DELIVERY@ - SES successfully delivered the email to the
    --     recipient\'s mail server.
    --
    -- -   @OPEN@ - The recipient received the message and opened it in their
    --     email client.
    --
    -- -   @CLICK@ - The recipient clicked one or more links in the email.
    --
    -- -   @RENDERING_FAILURE@ - The email wasn\'t sent because of a template
    --     rendering issue. This event type can occur when template data is
    --     missing, or when there is a mismatch between template parameters and
    --     data. (This event type only occurs when you send email using the
    --     <https://docs.aws.amazon.com/ses/latest/APIReference/API_SendTemplatedEmail.html SendTemplatedEmail>
    --     or
    --     <https://docs.aws.amazon.com/ses/latest/APIReference/API_SendBulkTemplatedEmail.html SendBulkTemplatedEmail>
    --     API operations.)
    --
    -- -   @DELIVERY_DELAY@ - The email couldn\'t be delivered to the
    --     recipient’s mail server because a temporary issue occurred. Delivery
    --     delays can occur, for example, when the recipient\'s inbox is full,
    --     or when the receiving email server experiences a transient issue.
    --
    -- -   @SUBSCRIPTION@ - The email was successfully delivered, but the
    --     recipient updated their subscription preferences by clicking on an
    --     /unsubscribe/ link as part of your
    --     <https://docs.aws.amazon.com/ses/latest/dg/sending-email-subscription-management.html subscription management>.
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
-- 'kinesisFirehoseDestination', 'eventDestination_kinesisFirehoseDestination' - An object that defines an Amazon Kinesis Data Firehose destination for
-- email events. You can use Amazon Kinesis Data Firehose to stream data to
-- other services, such as Amazon S3 and Amazon Redshift.
--
-- 'pinpointDestination', 'eventDestination_pinpointDestination' - An object that defines an Amazon Pinpoint project destination for email
-- events. You can send email event data to a Amazon Pinpoint project to
-- view metrics using the Transactional Messaging dashboards that are built
-- in to Amazon Pinpoint. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
-- in the /Amazon Pinpoint User Guide/.
--
-- 'snsDestination', 'eventDestination_snsDestination' - An object that defines an Amazon SNS destination for email events. You
-- can use Amazon SNS to send notification when certain email events occur.
--
-- 'name', 'eventDestination_name' - A name that identifies the event destination.
--
-- 'matchingEventTypes', 'eventDestination_matchingEventTypes' - The types of events that Amazon SES sends to the specified event
-- destinations.
--
-- -   @SEND@ - The send request was successful and SES will attempt to
--     deliver the message to the recipient’s mail server. (If
--     account-level or global suppression is being used, SES will still
--     count it as a send, but delivery is suppressed.)
--
-- -   @REJECT@ - SES accepted the email, but determined that it contained
--     a virus and didn’t attempt to deliver it to the recipient’s mail
--     server.
--
-- -   @BOUNCE@ - (/Hard bounce/) The recipient\'s mail server permanently
--     rejected the email. (/Soft bounces/ are only included when SES fails
--     to deliver the email after retrying for a period of time.)
--
-- -   @COMPLAINT@ - The email was successfully delivered to the
--     recipient’s mail server, but the recipient marked it as spam.
--
-- -   @DELIVERY@ - SES successfully delivered the email to the
--     recipient\'s mail server.
--
-- -   @OPEN@ - The recipient received the message and opened it in their
--     email client.
--
-- -   @CLICK@ - The recipient clicked one or more links in the email.
--
-- -   @RENDERING_FAILURE@ - The email wasn\'t sent because of a template
--     rendering issue. This event type can occur when template data is
--     missing, or when there is a mismatch between template parameters and
--     data. (This event type only occurs when you send email using the
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_SendTemplatedEmail.html SendTemplatedEmail>
--     or
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_SendBulkTemplatedEmail.html SendBulkTemplatedEmail>
--     API operations.)
--
-- -   @DELIVERY_DELAY@ - The email couldn\'t be delivered to the
--     recipient’s mail server because a temporary issue occurred. Delivery
--     delays can occur, for example, when the recipient\'s inbox is full,
--     or when the receiving email server experiences a transient issue.
--
-- -   @SUBSCRIPTION@ - The email was successfully delivered, but the
--     recipient updated their subscription preferences by clicking on an
--     /unsubscribe/ link as part of your
--     <https://docs.aws.amazon.com/ses/latest/dg/sending-email-subscription-management.html subscription management>.
newEventDestination ::
  -- | 'name'
  Prelude.Text ->
  EventDestination
newEventDestination pName_ =
  EventDestination'
    { cloudWatchDestination =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      kinesisFirehoseDestination = Prelude.Nothing,
      pinpointDestination = Prelude.Nothing,
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

-- | An object that defines an Amazon Kinesis Data Firehose destination for
-- email events. You can use Amazon Kinesis Data Firehose to stream data to
-- other services, such as Amazon S3 and Amazon Redshift.
eventDestination_kinesisFirehoseDestination :: Lens.Lens' EventDestination (Prelude.Maybe KinesisFirehoseDestination)
eventDestination_kinesisFirehoseDestination = Lens.lens (\EventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestination' {} a -> s {kinesisFirehoseDestination = a} :: EventDestination)

-- | An object that defines an Amazon Pinpoint project destination for email
-- events. You can send email event data to a Amazon Pinpoint project to
-- view metrics using the Transactional Messaging dashboards that are built
-- in to Amazon Pinpoint. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
-- in the /Amazon Pinpoint User Guide/.
eventDestination_pinpointDestination :: Lens.Lens' EventDestination (Prelude.Maybe PinpointDestination)
eventDestination_pinpointDestination = Lens.lens (\EventDestination' {pinpointDestination} -> pinpointDestination) (\s@EventDestination' {} a -> s {pinpointDestination = a} :: EventDestination)

-- | An object that defines an Amazon SNS destination for email events. You
-- can use Amazon SNS to send notification when certain email events occur.
eventDestination_snsDestination :: Lens.Lens' EventDestination (Prelude.Maybe SnsDestination)
eventDestination_snsDestination = Lens.lens (\EventDestination' {snsDestination} -> snsDestination) (\s@EventDestination' {} a -> s {snsDestination = a} :: EventDestination)

-- | A name that identifies the event destination.
eventDestination_name :: Lens.Lens' EventDestination Prelude.Text
eventDestination_name = Lens.lens (\EventDestination' {name} -> name) (\s@EventDestination' {} a -> s {name = a} :: EventDestination)

-- | The types of events that Amazon SES sends to the specified event
-- destinations.
--
-- -   @SEND@ - The send request was successful and SES will attempt to
--     deliver the message to the recipient’s mail server. (If
--     account-level or global suppression is being used, SES will still
--     count it as a send, but delivery is suppressed.)
--
-- -   @REJECT@ - SES accepted the email, but determined that it contained
--     a virus and didn’t attempt to deliver it to the recipient’s mail
--     server.
--
-- -   @BOUNCE@ - (/Hard bounce/) The recipient\'s mail server permanently
--     rejected the email. (/Soft bounces/ are only included when SES fails
--     to deliver the email after retrying for a period of time.)
--
-- -   @COMPLAINT@ - The email was successfully delivered to the
--     recipient’s mail server, but the recipient marked it as spam.
--
-- -   @DELIVERY@ - SES successfully delivered the email to the
--     recipient\'s mail server.
--
-- -   @OPEN@ - The recipient received the message and opened it in their
--     email client.
--
-- -   @CLICK@ - The recipient clicked one or more links in the email.
--
-- -   @RENDERING_FAILURE@ - The email wasn\'t sent because of a template
--     rendering issue. This event type can occur when template data is
--     missing, or when there is a mismatch between template parameters and
--     data. (This event type only occurs when you send email using the
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_SendTemplatedEmail.html SendTemplatedEmail>
--     or
--     <https://docs.aws.amazon.com/ses/latest/APIReference/API_SendBulkTemplatedEmail.html SendBulkTemplatedEmail>
--     API operations.)
--
-- -   @DELIVERY_DELAY@ - The email couldn\'t be delivered to the
--     recipient’s mail server because a temporary issue occurred. Delivery
--     delays can occur, for example, when the recipient\'s inbox is full,
--     or when the receiving email server experiences a transient issue.
--
-- -   @SUBSCRIPTION@ - The email was successfully delivered, but the
--     recipient updated their subscription preferences by clicking on an
--     /unsubscribe/ link as part of your
--     <https://docs.aws.amazon.com/ses/latest/dg/sending-email-subscription-management.html subscription management>.
eventDestination_matchingEventTypes :: Lens.Lens' EventDestination [EventType]
eventDestination_matchingEventTypes = Lens.lens (\EventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestination' {} a -> s {matchingEventTypes = a} :: EventDestination) Prelude.. Lens.coerced

instance Data.FromJSON EventDestination where
  parseJSON =
    Data.withObject
      "EventDestination"
      ( \x ->
          EventDestination'
            Prelude.<$> (x Data..:? "CloudWatchDestination")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "KinesisFirehoseDestination")
            Prelude.<*> (x Data..:? "PinpointDestination")
            Prelude.<*> (x Data..:? "SnsDestination")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> ( x
                            Data..:? "MatchingEventTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EventDestination where
  hashWithSalt _salt EventDestination' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchDestination
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` kinesisFirehoseDestination
      `Prelude.hashWithSalt` pinpointDestination
      `Prelude.hashWithSalt` snsDestination
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` matchingEventTypes

instance Prelude.NFData EventDestination where
  rnf EventDestination' {..} =
    Prelude.rnf cloudWatchDestination
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf kinesisFirehoseDestination
      `Prelude.seq` Prelude.rnf pinpointDestination
      `Prelude.seq` Prelude.rnf snsDestination
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf matchingEventTypes
