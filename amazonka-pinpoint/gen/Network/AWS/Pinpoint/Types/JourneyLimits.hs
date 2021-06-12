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
-- Module      : Network.AWS.Pinpoint.Types.JourneyLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies limits on the messages that a journey can send and the number
-- of times participants can enter a journey.
--
-- /See:/ 'newJourneyLimits' smart constructor.
data JourneyLimits = JourneyLimits'
  { -- | The maximum number of times that a participant can enter the journey.
    -- The maximum value is 100. To allow participants to enter the journey an
    -- unlimited number of times, set this value to 0.
    endpointReentryCap :: Core.Maybe Core.Int,
    -- | The maximum number of messages that the journey can send each second.
    messagesPerSecond :: Core.Maybe Core.Int,
    -- | The maximum number of messages that the journey can send to a single
    -- participant during a 24-hour period. The maximum value is 100.
    dailyCap :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JourneyLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointReentryCap', 'journeyLimits_endpointReentryCap' - The maximum number of times that a participant can enter the journey.
-- The maximum value is 100. To allow participants to enter the journey an
-- unlimited number of times, set this value to 0.
--
-- 'messagesPerSecond', 'journeyLimits_messagesPerSecond' - The maximum number of messages that the journey can send each second.
--
-- 'dailyCap', 'journeyLimits_dailyCap' - The maximum number of messages that the journey can send to a single
-- participant during a 24-hour period. The maximum value is 100.
newJourneyLimits ::
  JourneyLimits
newJourneyLimits =
  JourneyLimits'
    { endpointReentryCap = Core.Nothing,
      messagesPerSecond = Core.Nothing,
      dailyCap = Core.Nothing
    }

-- | The maximum number of times that a participant can enter the journey.
-- The maximum value is 100. To allow participants to enter the journey an
-- unlimited number of times, set this value to 0.
journeyLimits_endpointReentryCap :: Lens.Lens' JourneyLimits (Core.Maybe Core.Int)
journeyLimits_endpointReentryCap = Lens.lens (\JourneyLimits' {endpointReentryCap} -> endpointReentryCap) (\s@JourneyLimits' {} a -> s {endpointReentryCap = a} :: JourneyLimits)

-- | The maximum number of messages that the journey can send each second.
journeyLimits_messagesPerSecond :: Lens.Lens' JourneyLimits (Core.Maybe Core.Int)
journeyLimits_messagesPerSecond = Lens.lens (\JourneyLimits' {messagesPerSecond} -> messagesPerSecond) (\s@JourneyLimits' {} a -> s {messagesPerSecond = a} :: JourneyLimits)

-- | The maximum number of messages that the journey can send to a single
-- participant during a 24-hour period. The maximum value is 100.
journeyLimits_dailyCap :: Lens.Lens' JourneyLimits (Core.Maybe Core.Int)
journeyLimits_dailyCap = Lens.lens (\JourneyLimits' {dailyCap} -> dailyCap) (\s@JourneyLimits' {} a -> s {dailyCap = a} :: JourneyLimits)

instance Core.FromJSON JourneyLimits where
  parseJSON =
    Core.withObject
      "JourneyLimits"
      ( \x ->
          JourneyLimits'
            Core.<$> (x Core..:? "EndpointReentryCap")
            Core.<*> (x Core..:? "MessagesPerSecond")
            Core.<*> (x Core..:? "DailyCap")
      )

instance Core.Hashable JourneyLimits

instance Core.NFData JourneyLimits

instance Core.ToJSON JourneyLimits where
  toJSON JourneyLimits' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EndpointReentryCap" Core..=)
              Core.<$> endpointReentryCap,
            ("MessagesPerSecond" Core..=)
              Core.<$> messagesPerSecond,
            ("DailyCap" Core..=) Core.<$> dailyCap
          ]
      )
