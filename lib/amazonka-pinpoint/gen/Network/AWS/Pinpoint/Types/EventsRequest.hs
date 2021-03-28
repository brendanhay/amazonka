{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EventsRequest
  ( EventsRequest (..)
  -- * Smart constructor
  , mkEventsRequest
  -- * Lenses
  , erBatchItem
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EventsBatch as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies a batch of events to process.
--
-- /See:/ 'mkEventsRequest' smart constructor.
newtype EventsRequest = EventsRequest'
  { batchItem :: Core.HashMap Core.Text Types.EventsBatch
    -- ^ The batch of events to process. For each item in a batch, the endpoint ID acts as a key that has an EventsBatch object as its value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EventsRequest' value with any optional fields omitted.
mkEventsRequest
    :: EventsRequest
mkEventsRequest = EventsRequest'{batchItem = Core.mempty}

-- | The batch of events to process. For each item in a batch, the endpoint ID acts as a key that has an EventsBatch object as its value.
--
-- /Note:/ Consider using 'batchItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erBatchItem :: Lens.Lens' EventsRequest (Core.HashMap Core.Text Types.EventsBatch)
erBatchItem = Lens.field @"batchItem"
{-# INLINEABLE erBatchItem #-}
{-# DEPRECATED batchItem "Use generic-lens or generic-optics with 'batchItem' instead"  #-}

instance Core.FromJSON EventsRequest where
        toJSON EventsRequest{..}
          = Core.object
              (Core.catMaybes [Core.Just ("BatchItem" Core..= batchItem)])
