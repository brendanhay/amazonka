{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ItemResponse
  ( ItemResponse (..),

    -- * Smart constructor
    mkItemResponse,

    -- * Lenses
    iEndpointItemResponse,
    iEventsItemResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointItemResponse
import Network.AWS.Pinpoint.Types.EventItemResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the results of a request to create or update an endpoint that's associated with an event.
--
-- /See:/ 'mkItemResponse' smart constructor.
data ItemResponse = ItemResponse'
  { -- | The response that was received after the endpoint data was accepted.
    endpointItemResponse :: Lude.Maybe EndpointItemResponse,
    -- | A multipart response object that contains a key and a value for each event in the request. In each object, the event ID is the key and an EventItemResponse object is the value.
    eventsItemResponse :: Lude.Maybe (Lude.HashMap Lude.Text (EventItemResponse))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ItemResponse' with the minimum fields required to make a request.
--
-- * 'endpointItemResponse' - The response that was received after the endpoint data was accepted.
-- * 'eventsItemResponse' - A multipart response object that contains a key and a value for each event in the request. In each object, the event ID is the key and an EventItemResponse object is the value.
mkItemResponse ::
  ItemResponse
mkItemResponse =
  ItemResponse'
    { endpointItemResponse = Lude.Nothing,
      eventsItemResponse = Lude.Nothing
    }

-- | The response that was received after the endpoint data was accepted.
--
-- /Note:/ Consider using 'endpointItemResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEndpointItemResponse :: Lens.Lens' ItemResponse (Lude.Maybe EndpointItemResponse)
iEndpointItemResponse = Lens.lens (endpointItemResponse :: ItemResponse -> Lude.Maybe EndpointItemResponse) (\s a -> s {endpointItemResponse = a} :: ItemResponse)
{-# DEPRECATED iEndpointItemResponse "Use generic-lens or generic-optics with 'endpointItemResponse' instead." #-}

-- | A multipart response object that contains a key and a value for each event in the request. In each object, the event ID is the key and an EventItemResponse object is the value.
--
-- /Note:/ Consider using 'eventsItemResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEventsItemResponse :: Lens.Lens' ItemResponse (Lude.Maybe (Lude.HashMap Lude.Text (EventItemResponse)))
iEventsItemResponse = Lens.lens (eventsItemResponse :: ItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text (EventItemResponse))) (\s a -> s {eventsItemResponse = a} :: ItemResponse)
{-# DEPRECATED iEventsItemResponse "Use generic-lens or generic-optics with 'eventsItemResponse' instead." #-}

instance Lude.FromJSON ItemResponse where
  parseJSON =
    Lude.withObject
      "ItemResponse"
      ( \x ->
          ItemResponse'
            Lude.<$> (x Lude..:? "EndpointItemResponse")
            Lude.<*> (x Lude..:? "EventsItemResponse" Lude..!= Lude.mempty)
      )
