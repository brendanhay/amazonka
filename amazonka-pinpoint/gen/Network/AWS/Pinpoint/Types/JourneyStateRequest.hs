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
-- Module      : Network.AWS.Pinpoint.Types.JourneyStateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyStateRequest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.State
import qualified Network.AWS.Prelude as Prelude

-- | Changes the status of a journey.
--
-- /See:/ 'newJourneyStateRequest' smart constructor.
data JourneyStateRequest = JourneyStateRequest'
  { -- | The status of the journey. Currently, the only supported value is
    -- CANCELLED.
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
    state :: Prelude.Maybe State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JourneyStateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'journeyStateRequest_state' - The status of the journey. Currently, the only supported value is
-- CANCELLED.
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
newJourneyStateRequest ::
  JourneyStateRequest
newJourneyStateRequest =
  JourneyStateRequest' {state = Prelude.Nothing}

-- | The status of the journey. Currently, the only supported value is
-- CANCELLED.
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
journeyStateRequest_state :: Lens.Lens' JourneyStateRequest (Prelude.Maybe State)
journeyStateRequest_state = Lens.lens (\JourneyStateRequest' {state} -> state) (\s@JourneyStateRequest' {} a -> s {state = a} :: JourneyStateRequest)

instance Prelude.Hashable JourneyStateRequest

instance Prelude.NFData JourneyStateRequest

instance Prelude.ToJSON JourneyStateRequest where
  toJSON JourneyStateRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("State" Prelude..=) Prelude.<$> state]
      )
