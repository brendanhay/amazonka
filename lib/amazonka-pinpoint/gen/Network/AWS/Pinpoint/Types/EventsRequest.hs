{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventsRequest
  ( EventsRequest (..),

    -- * Smart constructor
    mkEventsRequest,

    -- * Lenses
    erBatchItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventsBatch
import qualified Network.AWS.Prelude as Lude

-- | Specifies a batch of events to process.
--
-- /See:/ 'mkEventsRequest' smart constructor.
newtype EventsRequest = EventsRequest'
  { -- | The batch of events to process. For each item in a batch, the endpoint ID acts as a key that has an EventsBatch object as its value.
    batchItem :: Lude.HashMap Lude.Text (EventsBatch)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventsRequest' with the minimum fields required to make a request.
--
-- * 'batchItem' - The batch of events to process. For each item in a batch, the endpoint ID acts as a key that has an EventsBatch object as its value.
mkEventsRequest ::
  EventsRequest
mkEventsRequest = EventsRequest' {batchItem = Lude.mempty}

-- | The batch of events to process. For each item in a batch, the endpoint ID acts as a key that has an EventsBatch object as its value.
--
-- /Note:/ Consider using 'batchItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erBatchItem :: Lens.Lens' EventsRequest (Lude.HashMap Lude.Text (EventsBatch))
erBatchItem = Lens.lens (batchItem :: EventsRequest -> Lude.HashMap Lude.Text (EventsBatch)) (\s a -> s {batchItem = a} :: EventsRequest)
{-# DEPRECATED erBatchItem "Use generic-lens or generic-optics with 'batchItem' instead." #-}

instance Lude.ToJSON EventsRequest where
  toJSON EventsRequest' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BatchItem" Lude..= batchItem)])
