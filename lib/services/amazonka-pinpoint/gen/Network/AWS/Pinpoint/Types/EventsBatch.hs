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
-- Module      : Network.AWS.Pinpoint.Types.EventsBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventsBatch where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Event
import Network.AWS.Pinpoint.Types.PublicEndpoint
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a batch of endpoints and events to process.
--
-- /See:/ 'newEventsBatch' smart constructor.
data EventsBatch = EventsBatch'
  { -- | A set of properties and attributes that are associated with the
    -- endpoint.
    endpoint :: PublicEndpoint,
    -- | A set of properties that are associated with the event.
    events :: Prelude.HashMap Prelude.Text Event
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventsBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'eventsBatch_endpoint' - A set of properties and attributes that are associated with the
-- endpoint.
--
-- 'events', 'eventsBatch_events' - A set of properties that are associated with the event.
newEventsBatch ::
  -- | 'endpoint'
  PublicEndpoint ->
  EventsBatch
newEventsBatch pEndpoint_ =
  EventsBatch'
    { endpoint = pEndpoint_,
      events = Prelude.mempty
    }

-- | A set of properties and attributes that are associated with the
-- endpoint.
eventsBatch_endpoint :: Lens.Lens' EventsBatch PublicEndpoint
eventsBatch_endpoint = Lens.lens (\EventsBatch' {endpoint} -> endpoint) (\s@EventsBatch' {} a -> s {endpoint = a} :: EventsBatch)

-- | A set of properties that are associated with the event.
eventsBatch_events :: Lens.Lens' EventsBatch (Prelude.HashMap Prelude.Text Event)
eventsBatch_events = Lens.lens (\EventsBatch' {events} -> events) (\s@EventsBatch' {} a -> s {events = a} :: EventsBatch) Prelude.. Lens.coerced

instance Prelude.Hashable EventsBatch

instance Prelude.NFData EventsBatch

instance Core.ToJSON EventsBatch where
  toJSON EventsBatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Endpoint" Core..= endpoint),
            Prelude.Just ("Events" Core..= events)
          ]
      )
