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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSMSVoice.Types.EventDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSMSVoice.Types.CloudWatchLogsDestination
import Amazonka.PinpointSMSVoice.Types.EventType
import Amazonka.PinpointSMSVoice.Types.KinesisFirehoseDestination
import Amazonka.PinpointSMSVoice.Types.SnsDestination
import qualified Amazonka.Prelude as Prelude

-- | An object that defines an event destination.
--
-- /See:/ 'newEventDestination' smart constructor.
data EventDestination = EventDestination'
  { cloudWatchLogsDestination :: Prelude.Maybe CloudWatchLogsDestination,
    -- | Indicates whether or not the event destination is enabled. If the event
    -- destination is enabled, then Amazon Pinpoint sends response data to the
    -- specified event destination.
    enabled :: Prelude.Maybe Prelude.Bool,
    kinesisFirehoseDestination :: Prelude.Maybe KinesisFirehoseDestination,
    matchingEventTypes :: Prelude.Maybe [EventType],
    -- | A name that identifies the event destination configuration.
    name :: Prelude.Maybe Prelude.Text,
    snsDestination :: Prelude.Maybe SnsDestination
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
-- 'cloudWatchLogsDestination', 'eventDestination_cloudWatchLogsDestination' - Undocumented member.
--
-- 'enabled', 'eventDestination_enabled' - Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
--
-- 'kinesisFirehoseDestination', 'eventDestination_kinesisFirehoseDestination' - Undocumented member.
--
-- 'matchingEventTypes', 'eventDestination_matchingEventTypes' - Undocumented member.
--
-- 'name', 'eventDestination_name' - A name that identifies the event destination configuration.
--
-- 'snsDestination', 'eventDestination_snsDestination' - Undocumented member.
newEventDestination ::
  EventDestination
newEventDestination =
  EventDestination'
    { cloudWatchLogsDestination =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      kinesisFirehoseDestination = Prelude.Nothing,
      matchingEventTypes = Prelude.Nothing,
      name = Prelude.Nothing,
      snsDestination = Prelude.Nothing
    }

-- | Undocumented member.
eventDestination_cloudWatchLogsDestination :: Lens.Lens' EventDestination (Prelude.Maybe CloudWatchLogsDestination)
eventDestination_cloudWatchLogsDestination = Lens.lens (\EventDestination' {cloudWatchLogsDestination} -> cloudWatchLogsDestination) (\s@EventDestination' {} a -> s {cloudWatchLogsDestination = a} :: EventDestination)

-- | Indicates whether or not the event destination is enabled. If the event
-- destination is enabled, then Amazon Pinpoint sends response data to the
-- specified event destination.
eventDestination_enabled :: Lens.Lens' EventDestination (Prelude.Maybe Prelude.Bool)
eventDestination_enabled = Lens.lens (\EventDestination' {enabled} -> enabled) (\s@EventDestination' {} a -> s {enabled = a} :: EventDestination)

-- | Undocumented member.
eventDestination_kinesisFirehoseDestination :: Lens.Lens' EventDestination (Prelude.Maybe KinesisFirehoseDestination)
eventDestination_kinesisFirehoseDestination = Lens.lens (\EventDestination' {kinesisFirehoseDestination} -> kinesisFirehoseDestination) (\s@EventDestination' {} a -> s {kinesisFirehoseDestination = a} :: EventDestination)

-- | Undocumented member.
eventDestination_matchingEventTypes :: Lens.Lens' EventDestination (Prelude.Maybe [EventType])
eventDestination_matchingEventTypes = Lens.lens (\EventDestination' {matchingEventTypes} -> matchingEventTypes) (\s@EventDestination' {} a -> s {matchingEventTypes = a} :: EventDestination) Prelude.. Lens.mapping Lens.coerced

-- | A name that identifies the event destination configuration.
eventDestination_name :: Lens.Lens' EventDestination (Prelude.Maybe Prelude.Text)
eventDestination_name = Lens.lens (\EventDestination' {name} -> name) (\s@EventDestination' {} a -> s {name = a} :: EventDestination)

-- | Undocumented member.
eventDestination_snsDestination :: Lens.Lens' EventDestination (Prelude.Maybe SnsDestination)
eventDestination_snsDestination = Lens.lens (\EventDestination' {snsDestination} -> snsDestination) (\s@EventDestination' {} a -> s {snsDestination = a} :: EventDestination)

instance Data.FromJSON EventDestination where
  parseJSON =
    Data.withObject
      "EventDestination"
      ( \x ->
          EventDestination'
            Prelude.<$> (x Data..:? "CloudWatchLogsDestination")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "KinesisFirehoseDestination")
            Prelude.<*> ( x
                            Data..:? "MatchingEventTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SnsDestination")
      )

instance Prelude.Hashable EventDestination where
  hashWithSalt _salt EventDestination' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsDestination
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` kinesisFirehoseDestination
      `Prelude.hashWithSalt` matchingEventTypes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` snsDestination

instance Prelude.NFData EventDestination where
  rnf EventDestination' {..} =
    Prelude.rnf cloudWatchLogsDestination `Prelude.seq`
      Prelude.rnf enabled `Prelude.seq`
        Prelude.rnf kinesisFirehoseDestination `Prelude.seq`
          Prelude.rnf matchingEventTypes `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf snsDestination
