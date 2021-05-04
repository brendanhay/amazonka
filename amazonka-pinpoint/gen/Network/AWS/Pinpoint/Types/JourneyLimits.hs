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
-- Module      : Network.AWS.Pinpoint.Types.JourneyLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyLimits where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies limits on the messages that a journey can send and the number
-- of times participants can enter a journey.
--
-- /See:/ 'newJourneyLimits' smart constructor.
data JourneyLimits = JourneyLimits'
  { -- | The maximum number of times that a participant can enter the journey.
    -- The maximum value is 100. To allow participants to enter the journey an
    -- unlimited number of times, set this value to 0.
    endpointReentryCap :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of messages that the journey can send each second.
    messagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of messages that the journey can send to a single
    -- participant during a 24-hour period. The maximum value is 100.
    dailyCap :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { endpointReentryCap =
        Prelude.Nothing,
      messagesPerSecond = Prelude.Nothing,
      dailyCap = Prelude.Nothing
    }

-- | The maximum number of times that a participant can enter the journey.
-- The maximum value is 100. To allow participants to enter the journey an
-- unlimited number of times, set this value to 0.
journeyLimits_endpointReentryCap :: Lens.Lens' JourneyLimits (Prelude.Maybe Prelude.Int)
journeyLimits_endpointReentryCap = Lens.lens (\JourneyLimits' {endpointReentryCap} -> endpointReentryCap) (\s@JourneyLimits' {} a -> s {endpointReentryCap = a} :: JourneyLimits)

-- | The maximum number of messages that the journey can send each second.
journeyLimits_messagesPerSecond :: Lens.Lens' JourneyLimits (Prelude.Maybe Prelude.Int)
journeyLimits_messagesPerSecond = Lens.lens (\JourneyLimits' {messagesPerSecond} -> messagesPerSecond) (\s@JourneyLimits' {} a -> s {messagesPerSecond = a} :: JourneyLimits)

-- | The maximum number of messages that the journey can send to a single
-- participant during a 24-hour period. The maximum value is 100.
journeyLimits_dailyCap :: Lens.Lens' JourneyLimits (Prelude.Maybe Prelude.Int)
journeyLimits_dailyCap = Lens.lens (\JourneyLimits' {dailyCap} -> dailyCap) (\s@JourneyLimits' {} a -> s {dailyCap = a} :: JourneyLimits)

instance Prelude.FromJSON JourneyLimits where
  parseJSON =
    Prelude.withObject
      "JourneyLimits"
      ( \x ->
          JourneyLimits'
            Prelude.<$> (x Prelude..:? "EndpointReentryCap")
            Prelude.<*> (x Prelude..:? "MessagesPerSecond")
            Prelude.<*> (x Prelude..:? "DailyCap")
      )

instance Prelude.Hashable JourneyLimits

instance Prelude.NFData JourneyLimits

instance Prelude.ToJSON JourneyLimits where
  toJSON JourneyLimits' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EndpointReentryCap" Prelude..=)
              Prelude.<$> endpointReentryCap,
            ("MessagesPerSecond" Prelude..=)
              Prelude.<$> messagesPerSecond,
            ("DailyCap" Prelude..=) Prelude.<$> dailyCap
          ]
      )
