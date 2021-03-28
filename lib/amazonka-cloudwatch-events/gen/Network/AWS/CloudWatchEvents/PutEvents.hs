{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends custom events to Amazon EventBridge so that they can be matched to rules.
module Network.AWS.CloudWatchEvents.PutEvents
    (
    -- * Creating a request
      PutEvents (..)
    , mkPutEvents
    -- ** Request lenses
    , peEntries

    -- * Destructuring the response
    , PutEventsResponse (..)
    , mkPutEventsResponse
    -- ** Response lenses
    , perrsEntries
    , perrsFailedEntryCount
    , perrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutEvents' smart constructor.
newtype PutEvents = PutEvents'
  { entries :: Core.NonEmpty Types.PutEventsRequestEntry
    -- ^ The entry that defines an event in your system. You can specify several parameters for the entry such as the source and type of the event, resources associated with the event, and so on.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'PutEvents' value with any optional fields omitted.
mkPutEvents
    :: Core.NonEmpty Types.PutEventsRequestEntry -- ^ 'entries'
    -> PutEvents
mkPutEvents entries = PutEvents'{entries}

-- | The entry that defines an event in your system. You can specify several parameters for the entry such as the source and type of the event, resources associated with the event, and so on.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEntries :: Lens.Lens' PutEvents (Core.NonEmpty Types.PutEventsRequestEntry)
peEntries = Lens.field @"entries"
{-# INLINEABLE peEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

instance Core.ToQuery PutEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutEvents where
        toHeaders PutEvents{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.PutEvents") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutEvents where
        toJSON PutEvents{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Entries" Core..= entries)])

instance Core.AWSRequest PutEvents where
        type Rs PutEvents = PutEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutEventsResponse' Core.<$>
                   (x Core..:? "Entries") Core.<*> x Core..:? "FailedEntryCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { entries :: Core.Maybe [Types.PutEventsResultEntry]
    -- ^ The successfully and unsuccessfully ingested events results. If the ingestion was successful, the entry has the event ID in it. Otherwise, you can use the error code and error message to identify the problem with the entry.
  , failedEntryCount :: Core.Maybe Core.Int
    -- ^ The number of failed entries.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEventsResponse' value with any optional fields omitted.
mkPutEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutEventsResponse
mkPutEventsResponse responseStatus
  = PutEventsResponse'{entries = Core.Nothing,
                       failedEntryCount = Core.Nothing, responseStatus}

-- | The successfully and unsuccessfully ingested events results. If the ingestion was successful, the entry has the event ID in it. Otherwise, you can use the error code and error message to identify the problem with the entry.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsEntries :: Lens.Lens' PutEventsResponse (Core.Maybe [Types.PutEventsResultEntry])
perrsEntries = Lens.field @"entries"
{-# INLINEABLE perrsEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

-- | The number of failed entries.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsFailedEntryCount :: Lens.Lens' PutEventsResponse (Core.Maybe Core.Int)
perrsFailedEntryCount = Lens.field @"failedEntryCount"
{-# INLINEABLE perrsFailedEntryCount #-}
{-# DEPRECATED failedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsResponseStatus :: Lens.Lens' PutEventsResponse Core.Int
perrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE perrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
