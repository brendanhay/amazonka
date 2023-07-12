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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.EventDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.EventDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.CloudWatchLogsDestination
import Amazonka.PinpointSmsVoiceV2.Types.EventType
import Amazonka.PinpointSmsVoiceV2.Types.KinesisFirehoseDestination
import Amazonka.PinpointSmsVoiceV2.Types.SnsDestination
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an event destination.
--
-- Event destinations are associated with configuration sets, which enable
-- you to publish message sending events to Amazon CloudWatch, Amazon
-- Kinesis Data Firehose, or Amazon SNS.
--
-- /See:/ 'newEventDestination' smart constructor.
data EventDestination = EventDestination'
  { -- | An object that contains information about an event destination that
    -- sends logging events to Amazon CloudWatch logs.
    cloudWatchLogsDestination :: Prelude.Maybe CloudWatchLogsDestination,
    -- | An object that contains information about an event destination for
    -- logging to Amazon Kinesis Data Firehose.
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    -- | An object that contains information about an event destination that
    -- sends logging events to Amazon SNS.
    snsDestination :: Prelude.Maybe SnsDestination,
    -- | The name of the EventDestination.
    eventDestinationName :: Prelude.Text,
    -- | When set to true events will be logged.
    enabled :: Prelude.Bool,
    -- | An array of event types that determine which events to log.
    matchingEventTypes :: Prelude.NonEmpty EventType
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
-- 'cloudWatchLogsDestination', 'eventDestination_cloudWatchLogsDestination' - An object that contains information about an event destination that
-- sends logging events to Amazon CloudWatch logs.
--
-- 'kinesisFirehoseDestination', 'eventDestination_kinesisFirehoseDestination' - An object that contains information about an event destination for
-- logging to Amazon Kinesis Data Firehose.
--
-- 'snsDestination', 'eventDestination_snsDestination' - An object that contains information about an event destination that
-- sends logging events to Amazon SNS.
--
-- 'eventDestinationName', 'eventDestination_eventDestinationName' - The name of the EventDestination.
--
-- 'enabled', 'eventDestination_enabled' - When set to true events will be logged.
--
-- 'matchingEventTypes', 'eventDestination_matchingEventTypes' - An array of event types that determine which events to log.
newEventDestination ::
  -- | 'eventDestinationName'
  Prelude.Text ->
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'matchingEventTypes'
  Prelude.NonEmpty EventType ->
  EventDestination
newEventDestination
  pEventDestinationName_
  pEnabled_
  pMatchingEventTypes_ =
    EventDestination'
      { cloudWatchLogsDestination =
          Prelude.Nothing,
        kinesisFirehoseDestination = Prelude.Nothing,
        snsDestination = Prelude.Nothing,
        eventDestinationName = pEventDestinationName_,
        enabled = pEnabled_,
        matchingEventTypes =
          Lens.coerced Lens.# pMatchingEventTypes_
      }

-- | An object that contains information about an event destination that
-- sends logging events to Amazon CloudWatch logs.
eventDestination_cloudWatchLogsDestination :: Lens.Lens' EventDestination (Prelude.Maybe CloudWatchLogsDestination)
eventDestination_cloudWatchLogsDestination = Lens.lens (\EventDestination' {cloudWatchLogsDestination} -> cloudWatchLogsDestination) (\s@EventDestination' {} a -> s {cloudWatchLogsDestination = a} :: EventDestination)

-- | An object that contains information about an event destination for
-- logging to Amazon Kinesis Data Firehose.
eventDestination_kinesisFirehoseDestination :: Lens.Lens' EventDestination (Prelude.Maybe KinesisFirehoseDestination)
eventDestination_kinesisFirehoseDestination = Lens.lens (\EventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestination' {} a -> s {kinesisFirehoseDestination = a} :: EventDestination)

-- | An object that contains information about an event destination that
-- sends logging events to Amazon SNS.
eventDestination_snsDestination :: Lens.Lens' EventDestination (Prelude.Maybe SnsDestination)
eventDestination_snsDestination = Lens.lens (\EventDestination' {snsDestination} -> snsDestination) (\s@EventDestination' {} a -> s {snsDestination = a} :: EventDestination)

-- | The name of the EventDestination.
eventDestination_eventDestinationName :: Lens.Lens' EventDestination Prelude.Text
eventDestination_eventDestinationName = Lens.lens (\EventDestination' {eventDestinationName} -> eventDestinationName) (\s@EventDestination' {} a -> s {eventDestinationName = a} :: EventDestination)

-- | When set to true events will be logged.
eventDestination_enabled :: Lens.Lens' EventDestination Prelude.Bool
eventDestination_enabled = Lens.lens (\EventDestination' {enabled} -> enabled) (\s@EventDestination' {} a -> s {enabled = a} :: EventDestination)

-- | An array of event types that determine which events to log.
eventDestination_matchingEventTypes :: Lens.Lens' EventDestination (Prelude.NonEmpty EventType)
eventDestination_matchingEventTypes = Lens.lens (\EventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestination' {} a -> s {matchingEventTypes = a} :: EventDestination) Prelude.. Lens.coerced

instance Data.FromJSON EventDestination where
  parseJSON =
    Data.withObject
      "EventDestination"
      ( \x ->
          EventDestination'
            Prelude.<$> (x Data..:? "CloudWatchLogsDestination")
            Prelude.<*> (x Data..:? "KinesisFirehoseDestination")
            Prelude.<*> (x Data..:? "SnsDestination")
            Prelude.<*> (x Data..: "EventDestinationName")
            Prelude.<*> (x Data..: "Enabled")
            Prelude.<*> (x Data..: "MatchingEventTypes")
      )

instance Prelude.Hashable EventDestination where
  hashWithSalt _salt EventDestination' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsDestination
      `Prelude.hashWithSalt` kinesisFirehoseDestination
      `Prelude.hashWithSalt` snsDestination
      `Prelude.hashWithSalt` eventDestinationName
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` matchingEventTypes

instance Prelude.NFData EventDestination where
  rnf EventDestination' {..} =
    Prelude.rnf cloudWatchLogsDestination
      `Prelude.seq` Prelude.rnf kinesisFirehoseDestination
      `Prelude.seq` Prelude.rnf snsDestination
      `Prelude.seq` Prelude.rnf eventDestinationName
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf matchingEventTypes
