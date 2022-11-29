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
-- Module      : Amazonka.Pinpoint.Types.JourneyStateRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyStateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types.State
import qualified Amazonka.Prelude as Prelude

-- | Changes the status of a journey.
--
-- /See:/ 'newJourneyStateRequest' smart constructor.
data JourneyStateRequest = JourneyStateRequest'
  { -- | The status of the journey. Currently, Supported values are ACTIVE,
    -- PAUSED, and CANCELLED
    --
    -- If you cancel a journey, Amazon Pinpoint continues to perform activities
    -- that are currently in progress, until those activities are complete.
    -- Amazon Pinpoint also continues to collect and aggregate analytics data
    -- for those activities, until they are complete, and any activities that
    -- were complete when you cancelled the journey.
    --
    -- After you cancel a journey, you can\'t add, change, or remove any
    -- activities from the journey. In addition, Amazon Pinpoint stops
    -- evaluating the journey and doesn\'t perform any activities that haven\'t
    -- started.
    --
    -- When the journey is paused, Amazon Pinpoint continues to perform
    -- activities that are currently in progress, until those activities are
    -- complete. Endpoints will stop entering journeys when the journey is
    -- paused and will resume entering the journey after the journey is
    -- resumed. For wait activities, wait time is paused when the journey is
    -- paused. Currently, PAUSED only supports journeys with a segment refresh
    -- interval.
    state :: Prelude.Maybe State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyStateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'journeyStateRequest_state' - The status of the journey. Currently, Supported values are ACTIVE,
-- PAUSED, and CANCELLED
--
-- If you cancel a journey, Amazon Pinpoint continues to perform activities
-- that are currently in progress, until those activities are complete.
-- Amazon Pinpoint also continues to collect and aggregate analytics data
-- for those activities, until they are complete, and any activities that
-- were complete when you cancelled the journey.
--
-- After you cancel a journey, you can\'t add, change, or remove any
-- activities from the journey. In addition, Amazon Pinpoint stops
-- evaluating the journey and doesn\'t perform any activities that haven\'t
-- started.
--
-- When the journey is paused, Amazon Pinpoint continues to perform
-- activities that are currently in progress, until those activities are
-- complete. Endpoints will stop entering journeys when the journey is
-- paused and will resume entering the journey after the journey is
-- resumed. For wait activities, wait time is paused when the journey is
-- paused. Currently, PAUSED only supports journeys with a segment refresh
-- interval.
newJourneyStateRequest ::
  JourneyStateRequest
newJourneyStateRequest =
  JourneyStateRequest' {state = Prelude.Nothing}

-- | The status of the journey. Currently, Supported values are ACTIVE,
-- PAUSED, and CANCELLED
--
-- If you cancel a journey, Amazon Pinpoint continues to perform activities
-- that are currently in progress, until those activities are complete.
-- Amazon Pinpoint also continues to collect and aggregate analytics data
-- for those activities, until they are complete, and any activities that
-- were complete when you cancelled the journey.
--
-- After you cancel a journey, you can\'t add, change, or remove any
-- activities from the journey. In addition, Amazon Pinpoint stops
-- evaluating the journey and doesn\'t perform any activities that haven\'t
-- started.
--
-- When the journey is paused, Amazon Pinpoint continues to perform
-- activities that are currently in progress, until those activities are
-- complete. Endpoints will stop entering journeys when the journey is
-- paused and will resume entering the journey after the journey is
-- resumed. For wait activities, wait time is paused when the journey is
-- paused. Currently, PAUSED only supports journeys with a segment refresh
-- interval.
journeyStateRequest_state :: Lens.Lens' JourneyStateRequest (Prelude.Maybe State)
journeyStateRequest_state = Lens.lens (\JourneyStateRequest' {state} -> state) (\s@JourneyStateRequest' {} a -> s {state = a} :: JourneyStateRequest)

instance Prelude.Hashable JourneyStateRequest where
  hashWithSalt _salt JourneyStateRequest' {..} =
    _salt `Prelude.hashWithSalt` state

instance Prelude.NFData JourneyStateRequest where
  rnf JourneyStateRequest' {..} = Prelude.rnf state

instance Core.ToJSON JourneyStateRequest where
  toJSON JourneyStateRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [("State" Core..=) Prelude.<$> state]
      )
