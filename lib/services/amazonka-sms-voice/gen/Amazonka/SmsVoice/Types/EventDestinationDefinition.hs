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
-- Module      : Amazonka.SmsVoice.Types.EventDestinationDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SmsVoice.Types.EventDestinationDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SmsVoice.Types.CloudWatchLogsDestination
import Amazonka.SmsVoice.Types.EventType
import Amazonka.SmsVoice.Types.KinesisFirehoseDestination
import Amazonka.SmsVoice.Types.SnsDestination

-- | An object that defines a single event destination.
--
-- /See:/ 'newEventDestinationDefinition' smart constructor.
data EventDestinationDefinition = EventDestinationDefinition'
  { cloudWatchLogsDestination :: Prelude.Maybe CloudWatchLogsDestination,
    -- | Indicates whether or not the event destination is enabled. If the event
    -- destination is enabled, then Amazon Pinpoint sends response data to the
    -- specified event destination.
    enabled :: Prelude.Maybe Prelude.Bool,
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    matchingEventTypes :: Prelude.Maybe [EventType],
    snsDestination :: Prelude.Maybe SnsDestination
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
-- 'cloudWatchLogsDestination', 'eventDestinationDefinition_cloudWatchLogsDestination' - Undocumented member.
--
-- 'enabled', 'eventDestinationDefinition_enabled' - Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
--
-- 'kinesisFirehoseDestination', 'eventDestinationDefinition_kinesisFirehoseDestination' - Undocumented member.
--
-- 'matchingEventTypes', 'eventDestinationDefinition_matchingEventTypes' - Undocumented member.
--
-- 'snsDestination', 'eventDestinationDefinition_snsDestination' - Undocumented member.
newEventDestinationDefinition ::
  EventDestinationDefinition
newEventDestinationDefinition =
  EventDestinationDefinition'
    { cloudWatchLogsDestination =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      kinesisFirehoseDestination = Prelude.Nothing,
      matchingEventTypes = Prelude.Nothing,
      snsDestination = Prelude.Nothing
    }

-- | Undocumented member.
eventDestinationDefinition_cloudWatchLogsDestination :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe CloudWatchLogsDestination)
eventDestinationDefinition_cloudWatchLogsDestination = Lens.lens (\EventDestinationDefinition' {cloudWatchLogsDestination} -> cloudWatchLogsDestination) (\s@EventDestinationDefinition' {} a -> s {cloudWatchLogsDestination = a} :: EventDestinationDefinition)

-- | Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
eventDestinationDefinition_enabled :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe Prelude.Bool)
eventDestinationDefinition_enabled = Lens.lens (\EventDestinationDefinition' {enabled} -> enabled) (\s@EventDestinationDefinition' {} a -> s {enabled = a} :: EventDestinationDefinition)

-- | Undocumented member.
eventDestinationDefinition_kinesisFirehoseDestination :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe KinesisFirehoseDestination)
eventDestinationDefinition_kinesisFirehoseDestination = Lens.lens (\EventDestinationDefinition' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestinationDefinition' {} a -> s {kinesisFirehoseDestination = a} :: EventDestinationDefinition)

-- | Undocumented member.
eventDestinationDefinition_matchingEventTypes :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe [EventType])
eventDestinationDefinition_matchingEventTypes = Lens.lens (\EventDestinationDefinition' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestinationDefinition' {} a -> s {matchingEventTypes = a} :: EventDestinationDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
eventDestinationDefinition_snsDestination :: Lens.Lens' EventDestinationDefinition (Prelude.Maybe SnsDestination)
eventDestinationDefinition_snsDestination = Lens.lens (\EventDestinationDefinition' {snsDestination} -> snsDestination) (\s@EventDestinationDefinition' {} a -> s {snsDestination = a} :: EventDestinationDefinition)

instance Prelude.Hashable EventDestinationDefinition where
  hashWithSalt _salt EventDestinationDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsDestination
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` kinesisFirehoseDestination
      `Prelude.hashWithSalt` matchingEventTypes
      `Prelude.hashWithSalt` snsDestination

instance Prelude.NFData EventDestinationDefinition where
  rnf EventDestinationDefinition' {..} =
    Prelude.rnf cloudWatchLogsDestination
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf kinesisFirehoseDestination
      `Prelude.seq` Prelude.rnf matchingEventTypes
      `Prelude.seq` Prelude.rnf snsDestination

instance Data.ToJSON EventDestinationDefinition where
  toJSON EventDestinationDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogsDestination" Data..=)
              Prelude.<$> cloudWatchLogsDestination,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("KinesisFirehoseDestination" Data..=)
              Prelude.<$> kinesisFirehoseDestination,
            ("MatchingEventTypes" Data..=)
              Prelude.<$> matchingEventTypes,
            ("SnsDestination" Data..=)
              Prelude.<$> snsDestination
          ]
      )
