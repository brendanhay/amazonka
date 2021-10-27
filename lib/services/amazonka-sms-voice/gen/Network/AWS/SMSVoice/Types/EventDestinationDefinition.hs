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
-- Module      : Network.AWS.SMSVoice.Types.EventDestinationDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMSVoice.Types.EventDestinationDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMSVoice.Types.CloudWatchLogsDestination
import Network.AWS.SMSVoice.Types.EventType
import Network.AWS.SMSVoice.Types.KinesisFirehoseDestination
import Network.AWS.SMSVoice.Types.SnsDestination

-- | An object that defines a single event destination.
--
-- /See:/ 'newEventDestinationDefinition' smart constructor.
data EventDestinationDefinition = EventDestinationDefinition'
  { matchingEventTypes :: Prelude.Maybe [EventType],
    -- | Indicates whether or not the event destination is enabled. If the event
    -- destination is enabled, then Amazon Pinpoint sends response data to the
    -- specified event destination.
    enabled :: Prelude.Maybe Prelude.Bool,
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    snsDestination :: Prelude.Maybe SnsDestination,
    cloudWatchLogsDestination :: Prelude.Maybe CloudWatchLogsDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventDestinationDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchingEventTypes', 'eventDestinationDefinition_matchingEventTypes' - Undocumented member.
--
-- 'enabled', 'eventDestinationDefinition_enabled' - Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
--
-- 'kinesisFirehoseDestination', 'eventDestinationDefinition_kinesisFirehoseDestination' - Undocumented member.
--
-- 'snsDestination', 'eventDestinationDefinition_snsDestination' - Undocumented member.
--
-- 'cloudWatchLogsDestination', 'eventDestinationDefinition_cloudWatchLogsDestination' - Undocumented member.
newEventDestinationDefinition ::
  EventDestinationDefinition
newEventDestinationDefinition =
  EventDestinationDefinition'
    { matchingEventTypes =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      kinesisFirehoseDestination = Prelude.Nothing,
      snsDestination = Prelude.Nothing,
      cloudWatchLogsDestination = Prelude.Nothing
    }

-- | Undocumented member.
eventDestinationDefinition_matchingEventTypes :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe [EventType])
eventDestinationDefinition_matchingEventTypes = Lens.lens (\EventDestinationDefinition' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestinationDefinition' {} a -> s {matchingEventTypes = a} :: EventDestinationDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
eventDestinationDefinition_enabled :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe Prelude.Bool)
eventDestinationDefinition_enabled = Lens.lens (\EventDestinationDefinition' {enabled} -> enabled) (\s@EventDestinationDefinition' {} a -> s {enabled = a} :: EventDestinationDefinition)

-- | Undocumented member.
eventDestinationDefinition_kinesisFirehoseDestination :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe KinesisFirehoseDestination)
eventDestinationDefinition_kinesisFirehoseDestination = Lens.lens (\EventDestinationDefinition' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestinationDefinition' {} a -> s {kinesisFirehoseDestination = a} :: EventDestinationDefinition)

-- | Undocumented member.
eventDestinationDefinition_snsDestination :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe SnsDestination)
eventDestinationDefinition_snsDestination = Lens.lens (\EventDestinationDefinition' {snsDestination} -> snsDestination) (\s@EventDestinationDefinition' {} a -> s {snsDestination = a} :: EventDestinationDefinition)

-- | Undocumented member.
eventDestinationDefinition_cloudWatchLogsDestination :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe CloudWatchLogsDestination)
eventDestinationDefinition_cloudWatchLogsDestination = Lens.lens (\EventDestinationDefinition' {cloudWatchLogsDestination} -> cloudWatchLogsDestination) (\s@EventDestinationDefinition' {} a -> s {cloudWatchLogsDestination = a} :: EventDestinationDefinition)

instance Prelude.Hashable EventDestinationDefinition

instance Prelude.NFData EventDestinationDefinition

instance Core.ToJSON EventDestinationDefinition where
  toJSON EventDestinationDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MatchingEventTypes" Core..=)
              Prelude.<$> matchingEventTypes,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("KinesisFirehoseDestination" Core..=)
              Prelude.<$> kinesisFirehoseDestination,
            ("SnsDestination" Core..=)
              Prelude.<$> snsDestination,
            ("CloudWatchLogsDestination" Core..=)
              Prelude.<$> cloudWatchLogsDestination
          ]
      )
