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
-- Module      : Amazonka.Pinpoint.Types.EventsBatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EventsBatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Event
import Amazonka.Pinpoint.Types.PublicEndpoint
import qualified Amazonka.Prelude as Prelude

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

instance Prelude.Hashable EventsBatch where
  hashWithSalt _salt EventsBatch' {..} =
    _salt `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` events

instance Prelude.NFData EventsBatch where
  rnf EventsBatch' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf events

instance Data.ToJSON EventsBatch where
  toJSON EventsBatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Endpoint" Data..= endpoint),
            Prelude.Just ("Events" Data..= events)
          ]
      )
