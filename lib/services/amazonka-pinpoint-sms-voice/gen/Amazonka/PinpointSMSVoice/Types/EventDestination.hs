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
-- Module      : Amazonka.PinpointSMSVoice.Types.EventDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSMSVoice.Types.EventDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.PinpointSMSVoice.Types.CloudWatchLogsDestination
import Amazonka.PinpointSMSVoice.Types.EventType
import Amazonka.PinpointSMSVoice.Types.KinesisFirehoseDestination
import Amazonka.PinpointSMSVoice.Types.SnsDestination
import qualified Amazonka.Prelude as Prelude

-- | An object that defines an event destination.
--
-- /See:/ 'newEventDestination' smart constructor.
data EventDestination = EventDestination'
  { matchingEventTypes :: Prelude.Maybe [EventType],
    -- | Indicates whether or not the event destination is enabled. If the event
    -- destination is enabled, then Amazon Pinpoint sends response data to the
    -- specified event destination.
    enabled :: Prelude.Maybe Prelude.Bool,
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    -- | A name that identifies the event destination configuration.
    name :: Prelude.Maybe Prelude.Text,
    snsDestination :: Prelude.Maybe SnsDestination,
    cloudWatchLogsDestination :: Prelude.Maybe CloudWatchLogsDestination
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
-- 'matchingEventTypes', 'eventDestination_matchingEventTypes' - Undocumented member.
--
-- 'enabled', 'eventDestination_enabled' - Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
--
-- 'kinesisFirehoseDestination', 'eventDestination_kinesisFirehoseDestination' - Undocumented member.
--
-- 'name', 'eventDestination_name' - A name that identifies the event destination configuration.
--
-- 'snsDestination', 'eventDestination_snsDestination' - Undocumented member.
--
-- 'cloudWatchLogsDestination', 'eventDestination_cloudWatchLogsDestination' - Undocumented member.
newEventDestination ::
  EventDestination
newEventDestination =
  EventDestination'
    { matchingEventTypes =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      kinesisFirehoseDestination = Prelude.Nothing,
      name = Prelude.Nothing,
      snsDestination = Prelude.Nothing,
      cloudWatchLogsDestination = Prelude.Nothing
    }

-- | Undocumented member.
eventDestination_matchingEventTypes :: Lens.Lens' EventDestination (Prelude.Maybe [EventType])
eventDestination_matchingEventTypes = Lens.lens (\EventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestination' {} a -> s {matchingEventTypes = a} :: EventDestination) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
eventDestination_enabled :: Lens.Lens' EventDestination (Prelude.Maybe Prelude.Bool)
eventDestination_enabled = Lens.lens (\EventDestination' {enabled} -> enabled) (\s@EventDestination' {} a -> s {enabled = a} :: EventDestination)

-- | Undocumented member.
eventDestination_kinesisFirehoseDestination :: Lens.Lens' EventDestination (Prelude.Maybe KinesisFirehoseDestination)
eventDestination_kinesisFirehoseDestination = Lens.lens (\EventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestination' {} a -> s {kinesisFirehoseDestination = a} :: EventDestination)

-- | A name that identifies the event destination configuration.
eventDestination_name :: Lens.Lens' EventDestination (Prelude.Maybe Prelude.Text)
eventDestination_name = Lens.lens (\EventDestination' {name} -> name) (\s@EventDestination' {} a -> s {name = a} :: EventDestination)

-- | Undocumented member.
eventDestination_snsDestination :: Lens.Lens' EventDestination (Prelude.Maybe SnsDestination)
eventDestination_snsDestination = Lens.lens (\EventDestination' {snsDestination} -> snsDestination) (\s@EventDestination' {} a -> s {snsDestination = a} :: EventDestination)

-- | Undocumented member.
eventDestination_cloudWatchLogsDestination :: Lens.Lens' EventDestination (Prelude.Maybe CloudWatchLogsDestination)
eventDestination_cloudWatchLogsDestination = Lens.lens (\EventDestination' {cloudWatchLogsDestination} -> cloudWatchLogsDestination) (\s@EventDestination' {} a -> s {cloudWatchLogsDestination = a} :: EventDestination)

instance Core.FromJSON EventDestination where
  parseJSON =
    Core.withObject
      "EventDestination"
      ( \x ->
          EventDestination'
            Prelude.<$> ( x Core..:? "MatchingEventTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "KinesisFirehoseDestination")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "SnsDestination")
            Prelude.<*> (x Core..:? "CloudWatchLogsDestination")
      )

instance Prelude.Hashable EventDestination where
  hashWithSalt salt' EventDestination' {..} =
    salt'
      `Prelude.hashWithSalt` cloudWatchLogsDestination
      `Prelude.hashWithSalt` snsDestination
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` kinesisFirehoseDestination
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` matchingEventTypes

instance Prelude.NFData EventDestination where
  rnf EventDestination' {..} =
    Prelude.rnf matchingEventTypes
      `Prelude.seq` Prelude.rnf cloudWatchLogsDestination
      `Prelude.seq` Prelude.rnf snsDestination
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf kinesisFirehoseDestination
      `Prelude.seq` Prelude.rnf enabled
