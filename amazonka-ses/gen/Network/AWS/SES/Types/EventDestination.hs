{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.Types.EventDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.EventDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.CloudWatchDestination
import Network.AWS.SES.Types.EventType
import Network.AWS.SES.Types.KinesisFirehoseDestination
import Network.AWS.SES.Types.SNSDestination

-- | Contains information about the event destination that the specified
-- email sending events will be published to.
--
-- When you create or update an event destination, you must provide one,
-- and only one, destination. The destination can be Amazon CloudWatch,
-- Amazon Kinesis Firehose or Amazon Simple Notification Service (Amazon
-- SNS).
--
-- Event destinations are associated with configuration sets, which enable
-- you to publish email sending events to Amazon CloudWatch, Amazon Kinesis
-- Firehose, or Amazon Simple Notification Service (Amazon SNS). For
-- information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newEventDestination' smart constructor.
data EventDestination = EventDestination'
  { -- | An object that contains the names, default values, and sources of the
    -- dimensions associated with an Amazon CloudWatch event destination.
    cloudWatchDestination :: Prelude.Maybe CloudWatchDestination,
    -- | Sets whether Amazon SES publishes events to this destination when you
    -- send an email with the associated configuration set. Set to @true@ to
    -- enable publishing to this destination; set to @false@ to prevent
    -- publishing to this destination. The default value is @false@.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that contains the delivery stream ARN and the IAM role ARN
    -- associated with an Amazon Kinesis Firehose event destination.
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    -- | An object that contains the topic ARN associated with an Amazon Simple
    -- Notification Service (Amazon SNS) event destination.
    sNSDestination :: Prelude.Maybe SNSDestination,
    -- | The name of the event destination. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Contain less than 64 characters.
    name :: Prelude.Text,
    -- | The type of email sending events to publish to the event destination.
    matchingEventTypes :: [EventType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchDestination', 'eventDestination_cloudWatchDestination' - An object that contains the names, default values, and sources of the
-- dimensions associated with an Amazon CloudWatch event destination.
--
-- 'enabled', 'eventDestination_enabled' - Sets whether Amazon SES publishes events to this destination when you
-- send an email with the associated configuration set. Set to @true@ to
-- enable publishing to this destination; set to @false@ to prevent
-- publishing to this destination. The default value is @false@.
--
-- 'kinesisFirehoseDestination', 'eventDestination_kinesisFirehoseDestination' - An object that contains the delivery stream ARN and the IAM role ARN
-- associated with an Amazon Kinesis Firehose event destination.
--
-- 'sNSDestination', 'eventDestination_sNSDestination' - An object that contains the topic ARN associated with an Amazon Simple
-- Notification Service (Amazon SNS) event destination.
--
-- 'name', 'eventDestination_name' - The name of the event destination. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 64 characters.
--
-- 'matchingEventTypes', 'eventDestination_matchingEventTypes' - The type of email sending events to publish to the event destination.
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
      sNSDestination = Prelude.Nothing,
      name = pName_,
      matchingEventTypes = Prelude.mempty
    }

-- | An object that contains the names, default values, and sources of the
-- dimensions associated with an Amazon CloudWatch event destination.
eventDestination_cloudWatchDestination :: Lens.Lens' EventDestination (Prelude.Maybe CloudWatchDestination)
eventDestination_cloudWatchDestination = Lens.lens (\EventDestination' {cloudWatchDestination} -> cloudWatchDestination) (\s@EventDestination' {} a -> s {cloudWatchDestination = a} :: EventDestination)

-- | Sets whether Amazon SES publishes events to this destination when you
-- send an email with the associated configuration set. Set to @true@ to
-- enable publishing to this destination; set to @false@ to prevent
-- publishing to this destination. The default value is @false@.
eventDestination_enabled :: Lens.Lens' EventDestination (Prelude.Maybe Prelude.Bool)
eventDestination_enabled = Lens.lens (\EventDestination' {enabled} -> enabled) (\s@EventDestination' {} a -> s {enabled = a} :: EventDestination)

-- | An object that contains the delivery stream ARN and the IAM role ARN
-- associated with an Amazon Kinesis Firehose event destination.
eventDestination_kinesisFirehoseDestination :: Lens.Lens' EventDestination (Prelude.Maybe KinesisFirehoseDestination)
eventDestination_kinesisFirehoseDestination = Lens.lens (\EventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestination' {} a -> s {kinesisFirehoseDestination = a} :: EventDestination)

-- | An object that contains the topic ARN associated with an Amazon Simple
-- Notification Service (Amazon SNS) event destination.
eventDestination_sNSDestination :: Lens.Lens' EventDestination (Prelude.Maybe SNSDestination)
eventDestination_sNSDestination = Lens.lens (\EventDestination' {sNSDestination} -> sNSDestination) (\s@EventDestination' {} a -> s {sNSDestination = a} :: EventDestination)

-- | The name of the event destination. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Contain less than 64 characters.
eventDestination_name :: Lens.Lens' EventDestination Prelude.Text
eventDestination_name = Lens.lens (\EventDestination' {name} -> name) (\s@EventDestination' {} a -> s {name = a} :: EventDestination)

-- | The type of email sending events to publish to the event destination.
eventDestination_matchingEventTypes :: Lens.Lens' EventDestination [EventType]
eventDestination_matchingEventTypes = Lens.lens (\EventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestination' {} a -> s {matchingEventTypes = a} :: EventDestination) Prelude.. Prelude._Coerce

instance Prelude.FromXML EventDestination where
  parseXML x =
    EventDestination'
      Prelude.<$> (x Prelude..@? "CloudWatchDestination")
      Prelude.<*> (x Prelude..@? "Enabled")
      Prelude.<*> (x Prelude..@? "KinesisFirehoseDestination")
      Prelude.<*> (x Prelude..@? "SNSDestination")
      Prelude.<*> (x Prelude..@ "Name")
      Prelude.<*> ( x Prelude..@? "MatchingEventTypes"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "member"
                  )

instance Prelude.Hashable EventDestination

instance Prelude.NFData EventDestination

instance Prelude.ToQuery EventDestination where
  toQuery EventDestination' {..} =
    Prelude.mconcat
      [ "CloudWatchDestination"
          Prelude.=: cloudWatchDestination,
        "Enabled" Prelude.=: enabled,
        "KinesisFirehoseDestination"
          Prelude.=: kinesisFirehoseDestination,
        "SNSDestination" Prelude.=: sNSDestination,
        "Name" Prelude.=: name,
        "MatchingEventTypes"
          Prelude.=: Prelude.toQueryList "member" matchingEventTypes
      ]
