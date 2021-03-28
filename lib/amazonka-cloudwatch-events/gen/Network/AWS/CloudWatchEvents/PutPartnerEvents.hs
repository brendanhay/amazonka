{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutPartnerEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is used by SaaS partners to write events to a customer's partner event bus. AWS customers do not use this operation.
module Network.AWS.CloudWatchEvents.PutPartnerEvents
    (
    -- * Creating a request
      PutPartnerEvents (..)
    , mkPutPartnerEvents
    -- ** Request lenses
    , ppeEntries

    -- * Destructuring the response
    , PutPartnerEventsResponse (..)
    , mkPutPartnerEventsResponse
    -- ** Response lenses
    , pperrsEntries
    , pperrsFailedEntryCount
    , pperrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutPartnerEvents' smart constructor.
newtype PutPartnerEvents = PutPartnerEvents'
  { entries :: Core.NonEmpty Types.PutPartnerEventsRequestEntry
    -- ^ The list of events to write to the event bus.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'PutPartnerEvents' value with any optional fields omitted.
mkPutPartnerEvents
    :: Core.NonEmpty Types.PutPartnerEventsRequestEntry -- ^ 'entries'
    -> PutPartnerEvents
mkPutPartnerEvents entries = PutPartnerEvents'{entries}

-- | The list of events to write to the event bus.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppeEntries :: Lens.Lens' PutPartnerEvents (Core.NonEmpty Types.PutPartnerEventsRequestEntry)
ppeEntries = Lens.field @"entries"
{-# INLINEABLE ppeEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

instance Core.ToQuery PutPartnerEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutPartnerEvents where
        toHeaders PutPartnerEvents{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.PutPartnerEvents") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutPartnerEvents where
        toJSON PutPartnerEvents{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Entries" Core..= entries)])

instance Core.AWSRequest PutPartnerEvents where
        type Rs PutPartnerEvents = PutPartnerEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutPartnerEventsResponse' Core.<$>
                   (x Core..:? "Entries") Core.<*> x Core..:? "FailedEntryCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutPartnerEventsResponse' smart constructor.
data PutPartnerEventsResponse = PutPartnerEventsResponse'
  { entries :: Core.Maybe [Types.PutPartnerEventsResultEntry]
    -- ^ The list of events from this operation that were successfully written to the partner event bus.
  , failedEntryCount :: Core.Maybe Core.Int
    -- ^ The number of events from this operation that could not be written to the partner event bus.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPartnerEventsResponse' value with any optional fields omitted.
mkPutPartnerEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutPartnerEventsResponse
mkPutPartnerEventsResponse responseStatus
  = PutPartnerEventsResponse'{entries = Core.Nothing,
                              failedEntryCount = Core.Nothing, responseStatus}

-- | The list of events from this operation that were successfully written to the partner event bus.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pperrsEntries :: Lens.Lens' PutPartnerEventsResponse (Core.Maybe [Types.PutPartnerEventsResultEntry])
pperrsEntries = Lens.field @"entries"
{-# INLINEABLE pperrsEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

-- | The number of events from this operation that could not be written to the partner event bus.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pperrsFailedEntryCount :: Lens.Lens' PutPartnerEventsResponse (Core.Maybe Core.Int)
pperrsFailedEntryCount = Lens.field @"failedEntryCount"
{-# INLINEABLE pperrsFailedEntryCount #-}
{-# DEPRECATED failedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pperrsResponseStatus :: Lens.Lens' PutPartnerEventsResponse Core.Int
pperrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pperrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
