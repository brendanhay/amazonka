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
-- Module      : Amazonka.Pinpoint.Types.ItemResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ItemResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.EndpointItemResponse
import Amazonka.Pinpoint.Types.EventItemResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the results of a request to create or update
-- an endpoint that\'s associated with an event.
--
-- /See:/ 'newItemResponse' smart constructor.
data ItemResponse = ItemResponse'
  { -- | A multipart response object that contains a key and a value for each
    -- event in the request. In each object, the event ID is the key and an
    -- EventItemResponse object is the value.
    eventsItemResponse :: Prelude.Maybe (Prelude.HashMap Prelude.Text EventItemResponse),
    -- | The response that was received after the endpoint data was accepted.
    endpointItemResponse :: Prelude.Maybe EndpointItemResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventsItemResponse', 'itemResponse_eventsItemResponse' - A multipart response object that contains a key and a value for each
-- event in the request. In each object, the event ID is the key and an
-- EventItemResponse object is the value.
--
-- 'endpointItemResponse', 'itemResponse_endpointItemResponse' - The response that was received after the endpoint data was accepted.
newItemResponse ::
  ItemResponse
newItemResponse =
  ItemResponse'
    { eventsItemResponse = Prelude.Nothing,
      endpointItemResponse = Prelude.Nothing
    }

-- | A multipart response object that contains a key and a value for each
-- event in the request. In each object, the event ID is the key and an
-- EventItemResponse object is the value.
itemResponse_eventsItemResponse :: Lens.Lens' ItemResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text EventItemResponse))
itemResponse_eventsItemResponse = Lens.lens (\ItemResponse' {eventsItemResponse} -> eventsItemResponse) (\s@ItemResponse' {} a -> s {eventsItemResponse = a} :: ItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response that was received after the endpoint data was accepted.
itemResponse_endpointItemResponse :: Lens.Lens' ItemResponse (Prelude.Maybe EndpointItemResponse)
itemResponse_endpointItemResponse = Lens.lens (\ItemResponse' {endpointItemResponse} -> endpointItemResponse) (\s@ItemResponse' {} a -> s {endpointItemResponse = a} :: ItemResponse)

instance Core.FromJSON ItemResponse where
  parseJSON =
    Core.withObject
      "ItemResponse"
      ( \x ->
          ItemResponse'
            Prelude.<$> ( x Core..:? "EventsItemResponse"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EndpointItemResponse")
      )

instance Prelude.Hashable ItemResponse where
  hashWithSalt _salt ItemResponse' {..} =
    _salt `Prelude.hashWithSalt` eventsItemResponse
      `Prelude.hashWithSalt` endpointItemResponse

instance Prelude.NFData ItemResponse where
  rnf ItemResponse' {..} =
    Prelude.rnf eventsItemResponse
      `Prelude.seq` Prelude.rnf endpointItemResponse
