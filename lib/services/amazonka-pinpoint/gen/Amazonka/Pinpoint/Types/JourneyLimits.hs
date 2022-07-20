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
-- Module      : Amazonka.Pinpoint.Types.JourneyLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies limits on the messages that a journey can send and the number
-- of times participants can enter a journey.
--
-- /See:/ 'newJourneyLimits' smart constructor.
data JourneyLimits = JourneyLimits'
  { -- | Minimum time that must pass before an endpoint can re-enter a given
    -- journey. The duration should use an ISO 8601 format, such as PT1H.
    endpointReentryInterval :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of messages that the journey can send each second.
    messagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of messages that the journey can send to a single
    -- participant during a 24-hour period. The maximum value is 100.
    dailyCap :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of times that a participant can enter the journey.
    -- The maximum value is 100. To allow participants to enter the journey an
    -- unlimited number of times, set this value to 0.
    endpointReentryCap :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointReentryInterval', 'journeyLimits_endpointReentryInterval' - Minimum time that must pass before an endpoint can re-enter a given
-- journey. The duration should use an ISO 8601 format, such as PT1H.
--
-- 'messagesPerSecond', 'journeyLimits_messagesPerSecond' - The maximum number of messages that the journey can send each second.
--
-- 'dailyCap', 'journeyLimits_dailyCap' - The maximum number of messages that the journey can send to a single
-- participant during a 24-hour period. The maximum value is 100.
--
-- 'endpointReentryCap', 'journeyLimits_endpointReentryCap' - The maximum number of times that a participant can enter the journey.
-- The maximum value is 100. To allow participants to enter the journey an
-- unlimited number of times, set this value to 0.
newJourneyLimits ::
  JourneyLimits
newJourneyLimits =
  JourneyLimits'
    { endpointReentryInterval =
        Prelude.Nothing,
      messagesPerSecond = Prelude.Nothing,
      dailyCap = Prelude.Nothing,
      endpointReentryCap = Prelude.Nothing
    }

-- | Minimum time that must pass before an endpoint can re-enter a given
-- journey. The duration should use an ISO 8601 format, such as PT1H.
journeyLimits_endpointReentryInterval :: Lens.Lens' JourneyLimits (Prelude.Maybe Prelude.Text)
journeyLimits_endpointReentryInterval = Lens.lens (\JourneyLimits' {endpointReentryInterval} -> endpointReentryInterval) (\s@JourneyLimits' {} a -> s {endpointReentryInterval = a} :: JourneyLimits)

-- | The maximum number of messages that the journey can send each second.
journeyLimits_messagesPerSecond :: Lens.Lens' JourneyLimits (Prelude.Maybe Prelude.Int)
journeyLimits_messagesPerSecond = Lens.lens (\JourneyLimits' {messagesPerSecond} -> messagesPerSecond) (\s@JourneyLimits' {} a -> s {messagesPerSecond = a} :: JourneyLimits)

-- | The maximum number of messages that the journey can send to a single
-- participant during a 24-hour period. The maximum value is 100.
journeyLimits_dailyCap :: Lens.Lens' JourneyLimits (Prelude.Maybe Prelude.Int)
journeyLimits_dailyCap = Lens.lens (\JourneyLimits' {dailyCap} -> dailyCap) (\s@JourneyLimits' {} a -> s {dailyCap = a} :: JourneyLimits)

-- | The maximum number of times that a participant can enter the journey.
-- The maximum value is 100. To allow participants to enter the journey an
-- unlimited number of times, set this value to 0.
journeyLimits_endpointReentryCap :: Lens.Lens' JourneyLimits (Prelude.Maybe Prelude.Int)
journeyLimits_endpointReentryCap = Lens.lens (\JourneyLimits' {endpointReentryCap} -> endpointReentryCap) (\s@JourneyLimits' {} a -> s {endpointReentryCap = a} :: JourneyLimits)

instance Core.FromJSON JourneyLimits where
  parseJSON =
    Core.withObject
      "JourneyLimits"
      ( \x ->
          JourneyLimits'
            Prelude.<$> (x Core..:? "EndpointReentryInterval")
            Prelude.<*> (x Core..:? "MessagesPerSecond")
            Prelude.<*> (x Core..:? "DailyCap")
            Prelude.<*> (x Core..:? "EndpointReentryCap")
      )

instance Prelude.Hashable JourneyLimits where
  hashWithSalt _salt JourneyLimits' {..} =
    _salt
      `Prelude.hashWithSalt` endpointReentryInterval
      `Prelude.hashWithSalt` messagesPerSecond
      `Prelude.hashWithSalt` dailyCap
      `Prelude.hashWithSalt` endpointReentryCap

instance Prelude.NFData JourneyLimits where
  rnf JourneyLimits' {..} =
    Prelude.rnf endpointReentryInterval
      `Prelude.seq` Prelude.rnf messagesPerSecond
      `Prelude.seq` Prelude.rnf dailyCap
      `Prelude.seq` Prelude.rnf endpointReentryCap

instance Core.ToJSON JourneyLimits where
  toJSON JourneyLimits' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EndpointReentryInterval" Core..=)
              Prelude.<$> endpointReentryInterval,
            ("MessagesPerSecond" Core..=)
              Prelude.<$> messagesPerSecond,
            ("DailyCap" Core..=) Prelude.<$> dailyCap,
            ("EndpointReentryCap" Core..=)
              Prelude.<$> endpointReentryCap
          ]
      )
