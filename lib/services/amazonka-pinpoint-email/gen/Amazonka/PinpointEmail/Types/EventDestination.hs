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
-- Module      : Amazonka.PinpointEmail.Types.EventDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.EventDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointEmail.Types.CloudWatchDestination
import Amazonka.PinpointEmail.Types.EventType
import Amazonka.PinpointEmail.Types.KinesisFirehoseDestination
import Amazonka.PinpointEmail.Types.PinpointDestination
import Amazonka.PinpointEmail.Types.SnsDestination
import qualified Amazonka.Prelude as Prelude

-- | In Amazon Pinpoint, /events/ include message sends, deliveries, opens,
-- clicks, bounces, and complaints. /Event destinations/ are places that
-- you can send information about these events to. For example, you can
-- send event data to Amazon SNS to receive notifications when you receive
-- bounces or complaints, or you can use Amazon Kinesis Data Firehose to
-- stream data to Amazon S3 for long-term storage.
--
-- /See:/ 'newEventDestination' smart constructor.
data EventDestination = EventDestination'
  { -- | An object that defines a Amazon Pinpoint destination for email events.
    -- You can use Amazon Pinpoint events to create attributes in Amazon
    -- Pinpoint projects. You can use these attributes to create segments for
    -- your campaigns.
    pinpointDestination :: Prelude.Maybe PinpointDestination,
    -- | An object that defines an Amazon SNS destination for email events. You
    -- can use Amazon SNS to send notification when certain email events occur.
    snsDestination :: Prelude.Maybe SnsDestination,
    -- | If @true@, the event destination is enabled. When the event destination
    -- is enabled, the specified event types are sent to the destinations in
    -- this @EventDestinationDefinition@.
    --
    -- If @false@, the event destination is disabled. When the event
    -- destination is disabled, events aren\'t sent to the specified
    -- destinations.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that defines an Amazon CloudWatch destination for email
    -- events. You can use Amazon CloudWatch to monitor and gain insights on
    -- your email sending metrics.
    cloudWatchDestination :: Prelude.Maybe CloudWatchDestination,
    -- | An object that defines an Amazon Kinesis Data Firehose destination for
    -- email events. You can use Amazon Kinesis Data Firehose to stream data to
    -- other services, such as Amazon S3 and Amazon Redshift.
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    -- | A name that identifies the event destination.
    name :: Prelude.Text,
    -- | The types of events that Amazon Pinpoint sends to the specified event
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
-- 'pinpointDestination', 'eventDestination_pinpointDestination' - An object that defines a Amazon Pinpoint destination for email events.
-- You can use Amazon Pinpoint events to create attributes in Amazon
-- Pinpoint projects. You can use these attributes to create segments for
-- your campaigns.
--
-- 'snsDestination', 'eventDestination_snsDestination' - An object that defines an Amazon SNS destination for email events. You
-- can use Amazon SNS to send notification when certain email events occur.
--
-- 'enabled', 'eventDestination_enabled' - If @true@, the event destination is enabled. When the event destination
-- is enabled, the specified event types are sent to the destinations in
-- this @EventDestinationDefinition@.
--
-- If @false@, the event destination is disabled. When the event
-- destination is disabled, events aren\'t sent to the specified
-- destinations.
--
-- 'cloudWatchDestination', 'eventDestination_cloudWatchDestination' - An object that defines an Amazon CloudWatch destination for email
-- events. You can use Amazon CloudWatch to monitor and gain insights on
-- your email sending metrics.
--
-- 'kinesisFirehoseDestination', 'eventDestination_kinesisFirehoseDestination' - An object that defines an Amazon Kinesis Data Firehose destination for
-- email events. You can use Amazon Kinesis Data Firehose to stream data to
-- other services, such as Amazon S3 and Amazon Redshift.
--
-- 'name', 'eventDestination_name' - A name that identifies the event destination.
--
-- 'matchingEventTypes', 'eventDestination_matchingEventTypes' - The types of events that Amazon Pinpoint sends to the specified event
-- destinations.
newEventDestination ::
  -- | 'name'
  Prelude.Text ->
  EventDestination
newEventDestination pName_ =
  EventDestination'
    { pinpointDestination =
        Prelude.Nothing,
      snsDestination = Prelude.Nothing,
      enabled = Prelude.Nothing,
      cloudWatchDestination = Prelude.Nothing,
      kinesisFirehoseDestination = Prelude.Nothing,
      name = pName_,
      matchingEventTypes = Prelude.mempty
    }

-- | An object that defines a Amazon Pinpoint destination for email events.
-- You can use Amazon Pinpoint events to create attributes in Amazon
-- Pinpoint projects. You can use these attributes to create segments for
-- your campaigns.
eventDestination_pinpointDestination :: Lens.Lens' EventDestination (Prelude.Maybe PinpointDestination)
eventDestination_pinpointDestination = Lens.lens (\EventDestination' {pinpointDestination} -> pinpointDestination) (\s@EventDestination' {} a -> s {pinpointDestination = a} :: EventDestination)

-- | An object that defines an Amazon SNS destination for email events. You
-- can use Amazon SNS to send notification when certain email events occur.
eventDestination_snsDestination :: Lens.Lens' EventDestination (Prelude.Maybe SnsDestination)
eventDestination_snsDestination = Lens.lens (\EventDestination' {snsDestination} -> snsDestination) (\s@EventDestination' {} a -> s {snsDestination = a} :: EventDestination)

-- | If @true@, the event destination is enabled. When the event destination
-- is enabled, the specified event types are sent to the destinations in
-- this @EventDestinationDefinition@.
--
-- If @false@, the event destination is disabled. When the event
-- destination is disabled, events aren\'t sent to the specified
-- destinations.
eventDestination_enabled :: Lens.Lens' EventDestination (Prelude.Maybe Prelude.Bool)
eventDestination_enabled = Lens.lens (\EventDestination' {enabled} -> enabled) (\s@EventDestination' {} a -> s {enabled = a} :: EventDestination)

-- | An object that defines an Amazon CloudWatch destination for email
-- events. You can use Amazon CloudWatch to monitor and gain insights on
-- your email sending metrics.
eventDestination_cloudWatchDestination :: Lens.Lens' EventDestination (Prelude.Maybe CloudWatchDestination)
eventDestination_cloudWatchDestination = Lens.lens (\EventDestination' {cloudWatchDestination} -> cloudWatchDestination) (\s@EventDestination' {} a -> s {cloudWatchDestination = a} :: EventDestination)

-- | An object that defines an Amazon Kinesis Data Firehose destination for
-- email events. You can use Amazon Kinesis Data Firehose to stream data to
-- other services, such as Amazon S3 and Amazon Redshift.
eventDestination_kinesisFirehoseDestination :: Lens.Lens' EventDestination (Prelude.Maybe KinesisFirehoseDestination)
eventDestination_kinesisFirehoseDestination = Lens.lens (\EventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestination' {} a -> s {kinesisFirehoseDestination = a} :: EventDestination)

-- | A name that identifies the event destination.
eventDestination_name :: Lens.Lens' EventDestination Prelude.Text
eventDestination_name = Lens.lens (\EventDestination' {name} -> name) (\s@EventDestination' {} a -> s {name = a} :: EventDestination)

-- | The types of events that Amazon Pinpoint sends to the specified event
-- destinations.
eventDestination_matchingEventTypes :: Lens.Lens' EventDestination [EventType]
eventDestination_matchingEventTypes = Lens.lens (\EventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestination' {} a -> s {matchingEventTypes = a} :: EventDestination) Prelude.. Lens.coerced

instance Core.FromJSON EventDestination where
  parseJSON =
    Core.withObject
      "EventDestination"
      ( \x ->
          EventDestination'
            Prelude.<$> (x Core..:? "PinpointDestination")
            Prelude.<*> (x Core..:? "SnsDestination")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "CloudWatchDestination")
            Prelude.<*> (x Core..:? "KinesisFirehoseDestination")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> ( x Core..:? "MatchingEventTypes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EventDestination where
  hashWithSalt _salt EventDestination' {..} =
    _salt `Prelude.hashWithSalt` pinpointDestination
      `Prelude.hashWithSalt` snsDestination
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` cloudWatchDestination
      `Prelude.hashWithSalt` kinesisFirehoseDestination
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` matchingEventTypes

instance Prelude.NFData EventDestination where
  rnf EventDestination' {..} =
    Prelude.rnf pinpointDestination
      `Prelude.seq` Prelude.rnf snsDestination
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf cloudWatchDestination
      `Prelude.seq` Prelude.rnf kinesisFirehoseDestination
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf matchingEventTypes
