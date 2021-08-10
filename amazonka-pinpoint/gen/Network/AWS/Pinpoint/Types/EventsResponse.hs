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
-- Module      : Network.AWS.Pinpoint.Types.EventsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventsResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ItemResponse
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about endpoints and the events that they\'re
-- associated with.
--
-- /See:/ 'newEventsResponse' smart constructor.
data EventsResponse = EventsResponse'
  { -- | A map that contains a multipart response for each endpoint. For each
    -- item in this object, the endpoint ID is the key and the item response is
    -- the value. If no item response exists, the value can also be one of the
    -- following: 202, the request was processed successfully; or 400, the
    -- payload wasn\'t valid or required fields were missing.
    results :: Prelude.Maybe (Prelude.HashMap Prelude.Text ItemResponse)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'results', 'eventsResponse_results' - A map that contains a multipart response for each endpoint. For each
-- item in this object, the endpoint ID is the key and the item response is
-- the value. If no item response exists, the value can also be one of the
-- following: 202, the request was processed successfully; or 400, the
-- payload wasn\'t valid or required fields were missing.
newEventsResponse ::
  EventsResponse
newEventsResponse =
  EventsResponse' {results = Prelude.Nothing}

-- | A map that contains a multipart response for each endpoint. For each
-- item in this object, the endpoint ID is the key and the item response is
-- the value. If no item response exists, the value can also be one of the
-- following: 202, the request was processed successfully; or 400, the
-- payload wasn\'t valid or required fields were missing.
eventsResponse_results :: Lens.Lens' EventsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ItemResponse))
eventsResponse_results = Lens.lens (\EventsResponse' {results} -> results) (\s@EventsResponse' {} a -> s {results = a} :: EventsResponse) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON EventsResponse where
  parseJSON =
    Core.withObject
      "EventsResponse"
      ( \x ->
          EventsResponse'
            Prelude.<$> (x Core..:? "Results" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable EventsResponse

instance Prelude.NFData EventsResponse
