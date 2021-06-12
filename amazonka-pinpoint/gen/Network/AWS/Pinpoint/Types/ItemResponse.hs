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
-- Module      : Network.AWS.Pinpoint.Types.ItemResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ItemResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointItemResponse
import Network.AWS.Pinpoint.Types.EventItemResponse

-- | Provides information about the results of a request to create or update
-- an endpoint that\'s associated with an event.
--
-- /See:/ 'newItemResponse' smart constructor.
data ItemResponse = ItemResponse'
  { -- | A multipart response object that contains a key and a value for each
    -- event in the request. In each object, the event ID is the key and an
    -- EventItemResponse object is the value.
    eventsItemResponse :: Core.Maybe (Core.HashMap Core.Text EventItemResponse),
    -- | The response that was received after the endpoint data was accepted.
    endpointItemResponse :: Core.Maybe EndpointItemResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { eventsItemResponse = Core.Nothing,
      endpointItemResponse = Core.Nothing
    }

-- | A multipart response object that contains a key and a value for each
-- event in the request. In each object, the event ID is the key and an
-- EventItemResponse object is the value.
itemResponse_eventsItemResponse :: Lens.Lens' ItemResponse (Core.Maybe (Core.HashMap Core.Text EventItemResponse))
itemResponse_eventsItemResponse = Lens.lens (\ItemResponse' {eventsItemResponse} -> eventsItemResponse) (\s@ItemResponse' {} a -> s {eventsItemResponse = a} :: ItemResponse) Core.. Lens.mapping Lens._Coerce

-- | The response that was received after the endpoint data was accepted.
itemResponse_endpointItemResponse :: Lens.Lens' ItemResponse (Core.Maybe EndpointItemResponse)
itemResponse_endpointItemResponse = Lens.lens (\ItemResponse' {endpointItemResponse} -> endpointItemResponse) (\s@ItemResponse' {} a -> s {endpointItemResponse = a} :: ItemResponse)

instance Core.FromJSON ItemResponse where
  parseJSON =
    Core.withObject
      "ItemResponse"
      ( \x ->
          ItemResponse'
            Core.<$> ( x Core..:? "EventsItemResponse"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "EndpointItemResponse")
      )

instance Core.Hashable ItemResponse

instance Core.NFData ItemResponse
