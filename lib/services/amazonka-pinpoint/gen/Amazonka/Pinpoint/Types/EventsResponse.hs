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
-- Module      : Amazonka.Pinpoint.Types.EventsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EventsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ItemResponse
import qualified Amazonka.Prelude as Prelude

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
eventsResponse_results = Lens.lens (\EventsResponse' {results} -> results) (\s@EventsResponse' {} a -> s {results = a} :: EventsResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EventsResponse where
  parseJSON =
    Data.withObject
      "EventsResponse"
      ( \x ->
          EventsResponse'
            Prelude.<$> (x Data..:? "Results" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EventsResponse where
  hashWithSalt _salt EventsResponse' {..} =
    _salt `Prelude.hashWithSalt` results

instance Prelude.NFData EventsResponse where
  rnf EventsResponse' {..} = Prelude.rnf results
