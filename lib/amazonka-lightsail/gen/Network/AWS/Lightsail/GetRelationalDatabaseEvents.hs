{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of events for a specific database in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseEvents
    (
    -- * Creating a request
      GetRelationalDatabaseEvents (..)
    , mkGetRelationalDatabaseEvents
    -- ** Request lenses
    , grdeRelationalDatabaseName
    , grdeDurationInMinutes
    , grdePageToken

    -- * Destructuring the response
    , GetRelationalDatabaseEventsResponse (..)
    , mkGetRelationalDatabaseEventsResponse
    -- ** Response lenses
    , grderrsNextPageToken
    , grderrsRelationalDatabaseEvents
    , grderrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseEvents' smart constructor.
data GetRelationalDatabaseEvents = GetRelationalDatabaseEvents'
  { relationalDatabaseName :: Types.ResourceName
    -- ^ The name of the database from which to get events.
  , durationInMinutes :: Core.Maybe Core.Int
    -- ^ The number of minutes in the past from which to retrieve events. For example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@ 
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
  , pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseEvents' value with any optional fields omitted.
mkGetRelationalDatabaseEvents
    :: Types.ResourceName -- ^ 'relationalDatabaseName'
    -> GetRelationalDatabaseEvents
mkGetRelationalDatabaseEvents relationalDatabaseName
  = GetRelationalDatabaseEvents'{relationalDatabaseName,
                                 durationInMinutes = Core.Nothing, pageToken = Core.Nothing}

-- | The name of the database from which to get events.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdeRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseEvents Types.ResourceName
grdeRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# INLINEABLE grdeRelationalDatabaseName #-}
{-# DEPRECATED relationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead"  #-}

-- | The number of minutes in the past from which to retrieve events. For example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@ 
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdeDurationInMinutes :: Lens.Lens' GetRelationalDatabaseEvents (Core.Maybe Core.Int)
grdeDurationInMinutes = Lens.field @"durationInMinutes"
{-# INLINEABLE grdeDurationInMinutes #-}
{-# DEPRECATED durationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdePageToken :: Lens.Lens' GetRelationalDatabaseEvents (Core.Maybe Core.Text)
grdePageToken = Lens.field @"pageToken"
{-# INLINEABLE grdePageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetRelationalDatabaseEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRelationalDatabaseEvents where
        toHeaders GetRelationalDatabaseEvents{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.GetRelationalDatabaseEvents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRelationalDatabaseEvents where
        toJSON GetRelationalDatabaseEvents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("relationalDatabaseName" Core..= relationalDatabaseName),
                  ("durationInMinutes" Core..=) Core.<$> durationInMinutes,
                  ("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetRelationalDatabaseEvents where
        type Rs GetRelationalDatabaseEvents =
             GetRelationalDatabaseEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseEventsResponse' Core.<$>
                   (x Core..:? "nextPageToken") Core.<*>
                     x Core..:? "relationalDatabaseEvents"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetRelationalDatabaseEvents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"relationalDatabaseEvents" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetRelationalDatabaseEventsResponse' smart constructor.
data GetRelationalDatabaseEventsResponse = GetRelationalDatabaseEventsResponse'
  { nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseEvents@ request and specify the next page token using the @pageToken@ parameter.
  , relationalDatabaseEvents :: Core.Maybe [Types.RelationalDatabaseEvent]
    -- ^ An object describing the result of your get relational database events request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetRelationalDatabaseEventsResponse' value with any optional fields omitted.
mkGetRelationalDatabaseEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRelationalDatabaseEventsResponse
mkGetRelationalDatabaseEventsResponse responseStatus
  = GetRelationalDatabaseEventsResponse'{nextPageToken =
                                           Core.Nothing,
                                         relationalDatabaseEvents = Core.Nothing, responseStatus}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseEvents@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grderrsNextPageToken :: Lens.Lens' GetRelationalDatabaseEventsResponse (Core.Maybe Core.Text)
grderrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grderrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An object describing the result of your get relational database events request.
--
-- /Note:/ Consider using 'relationalDatabaseEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grderrsRelationalDatabaseEvents :: Lens.Lens' GetRelationalDatabaseEventsResponse (Core.Maybe [Types.RelationalDatabaseEvent])
grderrsRelationalDatabaseEvents = Lens.field @"relationalDatabaseEvents"
{-# INLINEABLE grderrsRelationalDatabaseEvents #-}
{-# DEPRECATED relationalDatabaseEvents "Use generic-lens or generic-optics with 'relationalDatabaseEvents' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grderrsResponseStatus :: Lens.Lens' GetRelationalDatabaseEventsResponse Core.Int
grderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
