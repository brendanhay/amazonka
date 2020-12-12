{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventsResponse
  ( EventsResponse (..),

    -- * Smart constructor
    mkEventsResponse,

    -- * Lenses
    eResults,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ItemResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about endpoints and the events that they're associated with.
--
-- /See:/ 'mkEventsResponse' smart constructor.
newtype EventsResponse = EventsResponse'
  { results ::
      Lude.Maybe (Lude.HashMap Lude.Text (ItemResponse))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventsResponse' with the minimum fields required to make a request.
--
-- * 'results' - A map that contains a multipart response for each endpoint. For each item in this object, the endpoint ID is the key and the item response is the value. If no item response exists, the value can also be one of the following: 202, the request was processed successfully; or 400, the payload wasn't valid or required fields were missing.
mkEventsResponse ::
  EventsResponse
mkEventsResponse = EventsResponse' {results = Lude.Nothing}

-- | A map that contains a multipart response for each endpoint. For each item in this object, the endpoint ID is the key and the item response is the value. If no item response exists, the value can also be one of the following: 202, the request was processed successfully; or 400, the payload wasn't valid or required fields were missing.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResults :: Lens.Lens' EventsResponse (Lude.Maybe (Lude.HashMap Lude.Text (ItemResponse)))
eResults = Lens.lens (results :: EventsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (ItemResponse))) (\s a -> s {results = a} :: EventsResponse)
{-# DEPRECATED eResults "Use generic-lens or generic-optics with 'results' instead." #-}

instance Lude.FromJSON EventsResponse where
  parseJSON =
    Lude.withObject
      "EventsResponse"
      ( \x ->
          EventsResponse'
            Lude.<$> (x Lude..:? "Results" Lude..!= Lude.mempty)
      )
